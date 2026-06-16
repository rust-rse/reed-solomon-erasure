#[path = "../benches/common/mod.rs"]
mod bench_common;
mod common;

use std::fs;
use std::path::PathBuf;
use std::time::Instant;

use rustfs_erasure_codec::{ShardSlot, galois_8::ReedSolomon};

use self::bench_common::{
    ARTIFACT_SCHEMA_VERSION, BenchCase, Operation, backend, backend_id, backend_kind,
    backend_override, benchmark_metrics_enabled, derived_seed, features, git_revision,
    make_full_shards, target_triple,
};
use self::common::{assert_backend_override_honored_if_strict, override_honored};

struct SmallFileResult {
    operation: &'static str,
    data_shards: usize,
    parity_shards: usize,
    shard_size: usize,
    logical_data_bytes: usize,
    case_label: &'static str,
    seed: u64,
    throughput_mb_s: f64,
    ns_per_iter: f64,
}

#[derive(Clone, Copy)]
struct SmallFileProfileSnapshot {
    operation: &'static str,
    case_label: &'static str,
    iterations: usize,
    throughput_mb_s: f64,
    ns_per_iter: f64,
    stats: rustfs_erasure_codec::RuntimeProfileStats,
}

#[derive(Clone, Copy)]
enum SmallFileOp {
    Standard(Operation),
    VerifyWithBuffer,
    ReconstructOpt,
    ReconstructShardSlot,
    ReconstructSomeDataOnly,
}

impl SmallFileOp {
    fn as_str(self) -> &'static str {
        match self {
            SmallFileOp::Standard(operation) => operation.as_str(),
            SmallFileOp::VerifyWithBuffer => "verify_with_buffer",
            SmallFileOp::ReconstructOpt => "reconstruct_opt",
            SmallFileOp::ReconstructShardSlot => "reconstruct_shard_slot",
            SmallFileOp::ReconstructSomeDataOnly => "reconstruct_some_data_only",
        }
    }
}

fn shards_to_slots(original: &[Vec<u8>]) -> Vec<ShardSlot<Vec<u8>>> {
    original
        .iter()
        .cloned()
        .map(ShardSlot::new_present)
        .collect()
}

fn mark_missing_slots(slots: &mut [ShardSlot<Vec<u8>>], missing: &[usize]) {
    for &idx in missing {
        slots[idx].mark_missing();
    }
}

fn capture_reconstruct_profile_10x4_64k() {
    let case = BenchCase {
        data_shards: 10,
        parity_shards: 4,
        shard_size: 64 * 1024,
        label: "10x4_64k",
    };
    let iterations = small_file_iterations();
    let seed = derived_seed(Operation::Reconstruct, case);
    let rs = ReedSolomon::new(case.data_shards, case.parity_shards).unwrap();
    let logical_data_bytes = case.shard_size * case.data_shards;
    let mut original = make_full_shards(seed, case.data_shards, case.parity_shards, case.shard_size);
    rs.encode(&mut original).unwrap();

    let mut snapshots = Vec::new();

    let start = Instant::now();
    rs.reset_runtime_profile_stats();
    for _ in 0..iterations {
        let mut shards: Vec<Option<Vec<u8>>> = original.iter().cloned().map(Some).collect();
        shards[0] = None;
        shards[case.data_shards] = None;
        rs.reconstruct(&mut shards).unwrap();
    }
    let elapsed = start.elapsed();
    let ns_per_iter = elapsed.as_nanos() as f64 / iterations as f64;
    snapshots.push(SmallFileProfileSnapshot {
        operation: "reconstruct",
        case_label: case.label,
        iterations,
        throughput_mb_s: logical_data_bytes as f64 / (1024.0 * 1024.0) / (ns_per_iter / 1_000_000_000.0),
        ns_per_iter,
        stats: rs.runtime_profile_stats(),
    });

    let start = Instant::now();
    rs.reset_runtime_profile_stats();
    for _ in 0..iterations {
        let mut shards: Vec<Option<Vec<u8>>> = original.iter().cloned().map(Some).collect();
        shards[0] = None;
        shards[case.data_shards] = None;
        rs.reconstruct_opt(&mut shards).unwrap();
    }
    let elapsed = start.elapsed();
    let ns_per_iter = elapsed.as_nanos() as f64 / iterations as f64;
    snapshots.push(SmallFileProfileSnapshot {
        operation: "reconstruct_opt",
        case_label: case.label,
        iterations,
        throughput_mb_s: logical_data_bytes as f64 / (1024.0 * 1024.0) / (ns_per_iter / 1_000_000_000.0),
        ns_per_iter,
        stats: rs.runtime_profile_stats(),
    });

    let start = Instant::now();
    rs.reset_runtime_profile_stats();
    for _ in 0..iterations {
        let mut shards: Vec<Option<Vec<u8>>> = original.iter().cloned().map(Some).collect();
        shards[0] = None;
        shards[case.data_shards] = None;
        let _ = rs.plan_option_vec_reconstruct_for_bench(&shards, None).unwrap();
    }
    let elapsed = start.elapsed();
    let ns_per_iter = elapsed.as_nanos() as f64 / iterations as f64;
    snapshots.push(SmallFileProfileSnapshot {
        operation: "reconstruct_plan_only",
        case_label: case.label,
        iterations,
        throughput_mb_s: logical_data_bytes as f64 / (1024.0 * 1024.0) / (ns_per_iter / 1_000_000_000.0),
        ns_per_iter,
        stats: rs.runtime_profile_stats(),
    });

    let start = Instant::now();
    rs.reset_runtime_profile_stats();
    for _ in 0..iterations {
        let mut shards: Vec<Option<Vec<u8>>> = original.iter().cloned().map(Some).collect();
        shards[0] = None;
        shards[case.data_shards] = None;
        rs.execute_option_vec_reconstruct_plan_serial_for_bench(&mut shards, false)
            .unwrap();
    }
    let elapsed = start.elapsed();
    let ns_per_iter = elapsed.as_nanos() as f64 / iterations as f64;
    snapshots.push(SmallFileProfileSnapshot {
        operation: "reconstruct_execute_serial_only",
        case_label: case.label,
        iterations,
        throughput_mb_s: logical_data_bytes as f64 / (1024.0 * 1024.0) / (ns_per_iter / 1_000_000_000.0),
        ns_per_iter,
        stats: rs.runtime_profile_stats(),
    });

    let mut preplanned_shards: Vec<Option<Vec<u8>>> = original.iter().cloned().map(Some).collect();
    preplanned_shards[0] = None;
    preplanned_shards[case.data_shards] = None;
    let preplanned = rs.prepare_reconstruct_opt_workspace(&preplanned_shards).unwrap();

    let start = Instant::now();
    rs.reset_runtime_profile_stats();
    for _ in 0..iterations {
        let mut shards: Vec<Option<Vec<u8>>> = original.iter().cloned().map(Some).collect();
        shards[0] = None;
        shards[case.data_shards] = None;
        rs.reconstruct_opt_with_workspace(&mut shards, &preplanned)
            .unwrap();
    }
    let elapsed = start.elapsed();
    let ns_per_iter = elapsed.as_nanos() as f64 / iterations as f64;
    snapshots.push(SmallFileProfileSnapshot {
        operation: "reconstruct_execute_preplanned_serial",
        case_label: case.label,
        iterations,
        throughput_mb_s: logical_data_bytes as f64 / (1024.0 * 1024.0) / (ns_per_iter / 1_000_000_000.0),
        ns_per_iter,
        stats: rs.runtime_profile_stats(),
    });

    let start = Instant::now();
    rs.reset_runtime_profile_stats();
    for _ in 0..iterations {
        let mut shards = shards_to_slots(&original);
        mark_missing_slots(&mut shards, &[0, case.data_shards]);
        rs.reconstruct(&mut shards).unwrap();
    }
    let elapsed = start.elapsed();
    let ns_per_iter = elapsed.as_nanos() as f64 / iterations as f64;
    snapshots.push(SmallFileProfileSnapshot {
        operation: "reconstruct_shard_slot",
        case_label: case.label,
        iterations,
        throughput_mb_s: logical_data_bytes as f64 / (1024.0 * 1024.0) / (ns_per_iter / 1_000_000_000.0),
        ns_per_iter,
        stats: rs.runtime_profile_stats(),
    });

    let start = Instant::now();
    rs.reset_runtime_profile_stats();
    for _ in 0..iterations {
        let mut shards: Vec<Option<Vec<u8>>> = original.iter().cloned().map(Some).collect();
        shards[0] = None;
        shards[case.data_shards] = None;
        let mut required = vec![false; case.data_shards + case.parity_shards];
        required[0] = true;
        rs.reconstruct_some(&mut shards, &required).unwrap();
    }
    let elapsed = start.elapsed();
    let ns_per_iter = elapsed.as_nanos() as f64 / iterations as f64;
    snapshots.push(SmallFileProfileSnapshot {
        operation: "reconstruct_some_data_only",
        case_label: case.label,
        iterations,
        throughput_mb_s: logical_data_bytes as f64 / (1024.0 * 1024.0) / (ns_per_iter / 1_000_000_000.0),
        ns_per_iter,
        stats: rs.runtime_profile_stats(),
    });

    write_small_file_profile_snapshots(&snapshots);
}

fn write_small_file_profile_snapshots(snapshots: &[SmallFileProfileSnapshot]) {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("target/benchmark-smoke");
    fs::create_dir_all(&dir).unwrap();
    let json_path = dir.join("small-file-reconstruct-profile-10x4_64k.json");
    let csv_path = dir.join("small-file-reconstruct-profile-10x4_64k.csv");

    let mut json = String::from("[\n");
    for (i, snapshot) in snapshots.iter().enumerate() {
        let suffix = if i + 1 == snapshots.len() { "\n" } else { ",\n" };
        json.push_str(&format!(
            concat!(
                "  {{\"operation\":\"{}\",\"case_label\":\"{}\",\"iterations\":{},",
                "\"throughput_mb_s\":{:.4},\"ns_per_iter\":{:.2},",
                "\"reconstruct_entry_parallel_calls\":{},\"reconstruct_entry_serial_calls\":{},",
                "\"reconstruct_opt_fallback_serial_calls\":{},",
                "\"reconstruct_data_stage_calls\":{},\"reconstruct_parity_stage_calls\":{},",
                "\"code_some_serial_calls\":{},\"code_some_parallel_calls\":{},",
                "\"parallel_policy_calls\":{},\"parallel_policy_parallel\":{},",
                "\"parallel_policy_serial\":{}}}{}"
            ),
            snapshot.operation,
            snapshot.case_label,
            snapshot.iterations,
            snapshot.throughput_mb_s,
            snapshot.ns_per_iter,
            snapshot.stats.reconstruct_entry_parallel_calls,
            snapshot.stats.reconstruct_entry_serial_calls,
            snapshot.stats.reconstruct_opt_fallback_serial_calls,
            snapshot.stats.reconstruct_data_stage_calls,
            snapshot.stats.reconstruct_parity_stage_calls,
            snapshot.stats.code_some_serial_calls,
            snapshot.stats.code_some_parallel_calls,
            snapshot.stats.parallel_policy_calls,
            snapshot.stats.parallel_policy_parallel,
            snapshot.stats.parallel_policy_serial,
            suffix
        ));
    }
    json.push(']');
    fs::write(&json_path, json).unwrap();

    let mut csv = String::from(
        "operation,case_label,iterations,throughput_mb_s,ns_per_iter,reconstruct_entry_parallel_calls,reconstruct_entry_serial_calls,reconstruct_opt_fallback_serial_calls,reconstruct_data_stage_calls,reconstruct_parity_stage_calls,code_some_serial_calls,code_some_parallel_calls,parallel_policy_calls,parallel_policy_parallel,parallel_policy_serial\n",
    );
    for snapshot in snapshots {
        csv.push_str(&format!(
            "{},{},{},{:.4},{:.2},{},{},{},{},{},{},{},{},{},{}\n",
            snapshot.operation,
            snapshot.case_label,
            snapshot.iterations,
            snapshot.throughput_mb_s,
            snapshot.ns_per_iter,
            snapshot.stats.reconstruct_entry_parallel_calls,
            snapshot.stats.reconstruct_entry_serial_calls,
            snapshot.stats.reconstruct_opt_fallback_serial_calls,
            snapshot.stats.reconstruct_data_stage_calls,
            snapshot.stats.reconstruct_parity_stage_calls,
            snapshot.stats.code_some_serial_calls,
            snapshot.stats.code_some_parallel_calls,
            snapshot.stats.parallel_policy_calls,
            snapshot.stats.parallel_policy_parallel,
            snapshot.stats.parallel_policy_serial
        ));
    }
    fs::write(&csv_path, csv).unwrap();
}

const QUICK_SMALL_FILE_CASES: &[BenchCase] = &[
    BenchCase {
        data_shards: 4,
        parity_shards: 2,
        shard_size: 1024,
        label: "4x2_1k",
    },
    BenchCase {
        data_shards: 4,
        parity_shards: 2,
        shard_size: 4 * 1024,
        label: "4x2_4k",
    },
    BenchCase {
        data_shards: 4,
        parity_shards: 2,
        shard_size: 16 * 1024,
        label: "4x2_16k",
    },
    BenchCase {
        data_shards: 4,
        parity_shards: 2,
        shard_size: 64 * 1024,
        label: "4x2_64k",
    },
];

const FAST_SMALL_FILE_CASES: &[BenchCase] = &[
    BenchCase {
        data_shards: 4,
        parity_shards: 2,
        shard_size: 1024,
        label: "4x2_1k",
    },
    BenchCase {
        data_shards: 4,
        parity_shards: 2,
        shard_size: 4 * 1024,
        label: "4x2_4k",
    },
    BenchCase {
        data_shards: 4,
        parity_shards: 2,
        shard_size: 16 * 1024,
        label: "4x2_16k",
    },
    BenchCase {
        data_shards: 4,
        parity_shards: 2,
        shard_size: 64 * 1024,
        label: "4x2_64k",
    },
    BenchCase {
        data_shards: 4,
        parity_shards: 2,
        shard_size: 128 * 1024,
        label: "4x2_128k",
    },
    BenchCase {
        data_shards: 4,
        parity_shards: 2,
        shard_size: 256 * 1024,
        label: "4x2_256k",
    },
    BenchCase {
        data_shards: 4,
        parity_shards: 2,
        shard_size: 512 * 1024,
        label: "4x2_512k",
    },
    BenchCase {
        data_shards: 10,
        parity_shards: 4,
        shard_size: 16 * 1024,
        label: "10x4_16k",
    },
    BenchCase {
        data_shards: 10,
        parity_shards: 4,
        shard_size: 64 * 1024,
        label: "10x4_64k",
    },
    BenchCase {
        data_shards: 10,
        parity_shards: 4,
        shard_size: 256 * 1024,
        label: "10x4_256k",
    },
    BenchCase {
        data_shards: 10,
        parity_shards: 4,
        shard_size: 512 * 1024,
        label: "10x4_512k",
    },
];

const EXTENDED_SMALL_FILE_CASES: &[BenchCase] = &[
    BenchCase {
        data_shards: 4,
        parity_shards: 2,
        shard_size: 1024,
        label: "4x2_1k",
    },
    BenchCase {
        data_shards: 4,
        parity_shards: 2,
        shard_size: 4 * 1024,
        label: "4x2_4k",
    },
    BenchCase {
        data_shards: 4,
        parity_shards: 2,
        shard_size: 16 * 1024,
        label: "4x2_16k",
    },
    BenchCase {
        data_shards: 4,
        parity_shards: 2,
        shard_size: 64 * 1024,
        label: "4x2_64k",
    },
    BenchCase {
        data_shards: 4,
        parity_shards: 2,
        shard_size: 128 * 1024,
        label: "4x2_128k",
    },
    BenchCase {
        data_shards: 4,
        parity_shards: 2,
        shard_size: 256 * 1024,
        label: "4x2_256k",
    },
    BenchCase {
        data_shards: 4,
        parity_shards: 2,
        shard_size: 512 * 1024,
        label: "4x2_512k",
    },
    BenchCase {
        data_shards: 4,
        parity_shards: 2,
        shard_size: 1024 * 1024,
        label: "4x2_1m",
    },
    BenchCase {
        data_shards: 10,
        parity_shards: 4,
        shard_size: 1024,
        label: "10x4_1k",
    },
    BenchCase {
        data_shards: 10,
        parity_shards: 4,
        shard_size: 4 * 1024,
        label: "10x4_4k",
    },
    BenchCase {
        data_shards: 10,
        parity_shards: 4,
        shard_size: 16 * 1024,
        label: "10x4_16k",
    },
    BenchCase {
        data_shards: 10,
        parity_shards: 4,
        shard_size: 64 * 1024,
        label: "10x4_64k",
    },
    BenchCase {
        data_shards: 10,
        parity_shards: 4,
        shard_size: 128 * 1024,
        label: "10x4_128k",
    },
    BenchCase {
        data_shards: 10,
        parity_shards: 4,
        shard_size: 256 * 1024,
        label: "10x4_256k",
    },
    BenchCase {
        data_shards: 10,
        parity_shards: 4,
        shard_size: 512 * 1024,
        label: "10x4_512k",
    },
    BenchCase {
        data_shards: 10,
        parity_shards: 4,
        shard_size: 1024 * 1024,
        label: "10x4_1m",
    },
];

fn small_file_profile() -> &'static str {
    std::env::var("RSE_SMALL_FILE_PROFILE")
        .ok()
        .as_deref()
        .map(|value| match value {
            "extended" => "extended",
            "quick" => "quick",
            _ => "fast",
        })
        .unwrap_or("fast")
}

fn small_file_cases() -> &'static [BenchCase] {
    match small_file_profile() {
        "quick" => QUICK_SMALL_FILE_CASES,
        "extended" => EXTENDED_SMALL_FILE_CASES,
        _ => FAST_SMALL_FILE_CASES,
    }
}

fn selected_small_file_cases() -> Vec<BenchCase> {
    let cases = small_file_cases();
    let Some(raw_filter) = std::env::var("RSE_SMALL_FILE_CASE_FILTER").ok() else {
        return cases.to_vec();
    };

    let wanted: Vec<&str> = raw_filter
        .split(',')
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .collect();
    if wanted.is_empty() {
        return cases.to_vec();
    }

    cases
        .iter()
        .copied()
        .filter(|case| wanted.contains(&case.label))
        .collect()
}

fn small_file_iterations() -> usize {
    std::env::var("RSE_SMALL_FILE_ITERATIONS")
        .ok()
        .and_then(|value| value.parse::<usize>().ok())
        .filter(|value| *value > 0)
        .unwrap_or_else(|| match small_file_profile() {
            "extended" => 5,
            "quick" => 3,
            _ => 4,
        })
}

fn run_operation(case: BenchCase, operation: SmallFileOp, iterations: usize) -> SmallFileResult {
    let seed = derived_seed(
        match operation {
            SmallFileOp::Standard(op) => op,
            SmallFileOp::VerifyWithBuffer => Operation::Verify,
            SmallFileOp::ReconstructOpt => Operation::Reconstruct,
            SmallFileOp::ReconstructShardSlot => Operation::Reconstruct,
            SmallFileOp::ReconstructSomeDataOnly => Operation::ReconstructData,
        },
        case,
    );
    let rs = ReedSolomon::new(case.data_shards, case.parity_shards).unwrap();
    let logical_data_bytes = case.shard_size * case.data_shards;

    let start = Instant::now();
    match operation {
        SmallFileOp::Standard(Operation::Encode) => {
            let mut shards =
                make_full_shards(seed, case.data_shards, case.parity_shards, case.shard_size);
            for _ in 0..iterations {
                rs.encode(&mut shards).unwrap();
            }
        }
        SmallFileOp::Standard(Operation::LeopardSetup) => {
            for _ in 0..iterations {
                let codec = ReedSolomon::with_options(
                    case.data_shards,
                    case.parity_shards,
                    rustfs_erasure_codec::CodecOptions {
                        codec_family: rustfs_erasure_codec::CodecFamily::LeopardGF8,
                        ..rustfs_erasure_codec::CodecOptions::default()
                    },
                )
                .unwrap();
                let _ = codec.leopard_setup_matrix_shape();
            }
        }
        SmallFileOp::Standard(Operation::LeopardEncode) => {
            let codec = ReedSolomon::with_options(
                case.data_shards,
                case.parity_shards,
                rustfs_erasure_codec::CodecOptions {
                    codec_family: rustfs_erasure_codec::CodecFamily::LeopardGF8,
                    ..rustfs_erasure_codec::CodecOptions::default()
                },
            )
            .unwrap();
            for _ in 0..iterations {
                let mut shards =
                    make_full_shards(seed, case.data_shards, case.parity_shards, case.shard_size);
                codec.encode(&mut shards).unwrap();
            }
        }
        SmallFileOp::Standard(Operation::Update) => {
            let mut original =
                make_full_shards(seed, case.data_shards, case.parity_shards, case.shard_size);
            rs.encode(&mut original).unwrap();
            let old_data = original[..case.data_shards].to_vec();
            let mut updated = old_data.clone();
            if case.data_shards > 0 && case.shard_size > 0 {
                updated[0][0] ^= 0x5a;
            }
            let old_refs = old_data.iter().collect::<Vec<_>>();
            let changes = (0..case.data_shards)
                .map(|idx| if idx == 0 { Some(&updated[0]) } else { None })
                .collect::<Vec<_>>();
            for _ in 0..iterations {
                let mut parity = original[case.data_shards..].to_vec();
                let mut parity_refs = parity.iter_mut().collect::<Vec<_>>();
                rs.update(&old_refs, &changes, &mut parity_refs).unwrap();
            }
        }
        SmallFileOp::Standard(Operation::Verify) => {
            let mut shards =
                make_full_shards(seed, case.data_shards, case.parity_shards, case.shard_size);
            rs.encode(&mut shards).unwrap();
            for _ in 0..iterations {
                rs.verify(&shards).unwrap();
            }
        }
        SmallFileOp::VerifyWithBuffer => {
            let mut shards =
                make_full_shards(seed, case.data_shards, case.parity_shards, case.shard_size);
            rs.encode(&mut shards).unwrap();
            let mut buffer = vec![vec![0u8; case.shard_size]; case.parity_shards];
            for _ in 0..iterations {
                rs.verify_with_buffer(&shards, &mut buffer).unwrap();
            }
        }
        SmallFileOp::ReconstructOpt => {
            let mut original =
                make_full_shards(seed, case.data_shards, case.parity_shards, case.shard_size);
            rs.encode(&mut original).unwrap();
            for _ in 0..iterations {
                let mut shards: Vec<Option<Vec<u8>>> = original.iter().cloned().map(Some).collect();
                shards[0] = None;
                shards[case.data_shards] = None;
                rs.reconstruct_opt(&mut shards).unwrap();
            }
        }
        SmallFileOp::ReconstructShardSlot => {
            let mut original =
                make_full_shards(seed, case.data_shards, case.parity_shards, case.shard_size);
            rs.encode(&mut original).unwrap();
            for _ in 0..iterations {
                let mut shards = shards_to_slots(&original);
                mark_missing_slots(&mut shards, &[0, case.data_shards]);
                rs.reconstruct(&mut shards).unwrap();
            }
        }
        SmallFileOp::ReconstructSomeDataOnly => {
            let mut original =
                make_full_shards(seed, case.data_shards, case.parity_shards, case.shard_size);
            rs.encode(&mut original).unwrap();
            let mut required = vec![false; case.data_shards + case.parity_shards];
            required[0] = true;
            for _ in 0..iterations {
                let mut shards: Vec<Option<Vec<u8>>> = original.iter().cloned().map(Some).collect();
                shards[0] = None;
                shards[case.data_shards] = None;
                rs.reconstruct_some(&mut shards, &required).unwrap();
            }
        }
        SmallFileOp::Standard(Operation::Reconstruct) => {
            let mut original =
                make_full_shards(seed, case.data_shards, case.parity_shards, case.shard_size);
            rs.encode(&mut original).unwrap();
            for _ in 0..iterations {
                let mut shards: Vec<Option<Vec<u8>>> = original.iter().cloned().map(Some).collect();
                shards[0] = None;
                shards[case.data_shards] = None;
                rs.reconstruct(&mut shards).unwrap();
            }
        }
        SmallFileOp::Standard(Operation::ReconstructData) => {
            let mut original =
                make_full_shards(seed, case.data_shards, case.parity_shards, case.shard_size);
            rs.encode(&mut original).unwrap();
            for _ in 0..iterations {
                let mut shards: Vec<Option<Vec<u8>>> = original.iter().cloned().map(Some).collect();
                shards[0] = None;
                shards[1] = None;
                rs.reconstruct_data(&mut shards).unwrap();
            }
        }
    }
    let elapsed = start.elapsed();
    let ns_per_iter = elapsed.as_nanos() as f64 / iterations as f64;
    let throughput_mb_s =
        logical_data_bytes as f64 / (1024.0 * 1024.0) / (ns_per_iter / 1_000_000_000.0);

    SmallFileResult {
        operation: operation.as_str(),
        data_shards: case.data_shards,
        parity_shards: case.parity_shards,
        shard_size: case.shard_size,
        logical_data_bytes,
        case_label: case.label,
        seed,
        throughput_mb_s,
        ns_per_iter,
    }
}

fn write_results(results: &[SmallFileResult]) {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("target/benchmark-smoke");
    fs::create_dir_all(&dir).unwrap();

    let revision = git_revision();
    let target = target_triple();
    let features = features();
    let backend = backend();
    let backend_id = backend_id();
    let backend_kind = backend_kind();
    let backend_override = backend_override();
    let override_honored = override_honored();
    let metrics_enabled = benchmark_metrics_enabled();
    let profile = small_file_profile();
    let iterations = small_file_iterations();

    let json_path = dir.join("small-file-results.json");
    let csv_path = dir.join("small-file-results.csv");

    let mut json = String::from("[\n");
    for (i, result) in results.iter().enumerate() {
        let suffix = if i + 1 == results.len() { "\n" } else { ",\n" };
        json.push_str(&format!(
            "  {{\"schema_version\":{},\"artifact_kind\":\"small-file-results\",\"git_revision\":\"{}\",\"target_triple\":\"{}\",\"features\":\"{}\",\"benchmark_metrics_enabled\":{},\"backend\":\"{}\",\"backend_id\":\"{}\",\"backend_kind\":\"{}\",\"backend_override\":\"{}\",\"override_honored\":{},\"profile\":\"{}\",\"iterations\":{},\"operation\":\"{}\",\"data_shards\":{},\"parity_shards\":{},\"shard_size\":{},\"logical_data_bytes\":{},\"case_label\":\"{}\",\"seed\":{},\"throughput_mb_s\":{:.4},\"ns_per_iter\":{:.2}}}{}",
            ARTIFACT_SCHEMA_VERSION,
            revision,
            target,
            features,
            metrics_enabled,
            backend,
            backend_id,
            backend_kind,
            backend_override,
            override_honored,
            profile,
            iterations,
            result.operation,
            result.data_shards,
            result.parity_shards,
            result.shard_size,
            result.logical_data_bytes,
            result.case_label,
            result.seed,
            result.throughput_mb_s,
            result.ns_per_iter,
            suffix
        ));
    }
    json.push(']');
    fs::write(&json_path, json).unwrap();

    let mut csv = String::from(
        "schema_version,artifact_kind,git_revision,target_triple,features,benchmark_metrics_enabled,backend,backend_id,backend_kind,backend_override,override_honored,profile,iterations,operation,data_shards,parity_shards,shard_size,logical_data_bytes,case_label,seed,throughput_mb_s,ns_per_iter\n",
    );
    for result in results {
        csv.push_str(&format!(
            "{},small-file-results,{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{:.4},{:.2}\n",
            ARTIFACT_SCHEMA_VERSION,
            revision,
            target,
            features,
            metrics_enabled,
            backend,
            backend_id,
            backend_kind,
            backend_override,
            override_honored,
            profile,
            iterations,
            result.operation,
            result.data_shards,
            result.parity_shards,
            result.shard_size,
            result.logical_data_bytes,
            result.case_label,
            result.seed,
            result.throughput_mb_s,
            result.ns_per_iter
        ));
    }
    fs::write(&csv_path, csv).unwrap();

    assert!(json_path.exists());
    assert!(csv_path.exists());
}

#[test]
#[ignore]
fn benchmark_small_file_matrix_runs_and_exports_results() {
    assert_backend_override_honored_if_strict();
    let mut results = Vec::new();
    let iterations = small_file_iterations();
    let cases = selected_small_file_cases();

    let should_capture_reconstruct_profile = cases.len() == 1 && cases[0].label == "10x4_64k";

    for case in cases {
        results.push(run_operation(
            case,
            SmallFileOp::Standard(Operation::Encode),
            iterations,
        ));
        results.push(run_operation(
            case,
            SmallFileOp::Standard(Operation::Verify),
            iterations,
        ));
        results.push(run_operation(
            case,
            SmallFileOp::VerifyWithBuffer,
            iterations,
        ));
        results.push(run_operation(
            case,
            SmallFileOp::Standard(Operation::Reconstruct),
            iterations,
        ));
        results.push(run_operation(case, SmallFileOp::ReconstructOpt, iterations));
        results.push(run_operation(case, SmallFileOp::ReconstructShardSlot, iterations));
        results.push(run_operation(case, SmallFileOp::ReconstructSomeDataOnly, iterations));
        results.push(run_operation(
            case,
            SmallFileOp::Standard(Operation::ReconstructData),
            iterations,
        ));
    }

    assert!(!results.is_empty());
    assert!(
        results
            .iter()
            .all(|result| result.throughput_mb_s.is_finite())
    );
    write_results(&results);
    if should_capture_reconstruct_profile {
        capture_reconstruct_profile_10x4_64k();
    }
}

# Cross-Platform Benchmark Report

> Date: 2026-06-01
> Library: rustfs-erasure-codec v7.0.0

---

## Benchmark Infrastructure

### Benchmark Targets

| Target | File | Description |
|--------|------|-------------|
| `bandwidth` | `benches/bandwidth.rs` | Encode/reconstruct throughput (GiB/s) |
| `throughput_matrix` | `benches/throughput_matrix.rs` | Encode throughput across config matrix |
| `smoke` | `tests/benchmark_smoke.rs` | CI-friendly smoke tests (fast profile) |

### Configuration Matrix (22 configs)

Defined in `benches/common/mod.rs`:

| Shard Size | Data×Parity Configs |
|------------|---------------------|
| 1 KiB | 10×4, 12×4, 8×3, 8×4, 6×3, 4×2 |
| 4 KiB | 10×4, 12×4, 8×3, 8×4, 6×3, 4×2 |
| 64 KiB | 10×4, 12×4, 8×3, 8×4, 6×3, 4×2 |
| 1 MiB | 10×4, 12×4, 8×3, 8×4, 6×3, 4×2 |

### How to Run

```bash
# Full benchmark suite
cargo bench

# Specific SIMD backend
cargo bench --features simd-avx2    # x86_64 AVX2
cargo bench --features simd-neon    # aarch64 NEON
cargo bench --features simd-gfni    # x86_64 GFNI (requires Intel Ice Lake+)

# Smoke tests (CI)
VALIDATION_PROFILE=fast cargo test --test benchmark_smoke

# Export results as JSON
RSE_WRITE_PROFILE_REPORT=1 cargo bench
```

---

## Platform Results

### aarch64 (Apple Silicon)

**Hardware**: Apple M5 Max MacBook Pro
**Backend**: `rust-neon` (auto-selected)
**Feature**: `simd-neon`
**Artifact source**: `benchmarks/small-file/2026-05-27-aarch64-apple-silicon-extended.{json,csv}`
**Profile**: `extended` (`iterations = 5`)

**Benchmark Environment**
- OS: `macOS 26.5.1`
- Kernel: `Darwin 25.5.0`
- Hostname: `zhi-mbp.local`
- Target triple: `aarch64-macos-unknown`
- Architecture: `aarch64`
- Features: `std|simd-accel`
- Backend kind: `RustSimd`
- Backend override: `auto` (`override_honored = true`)
- Benchmark metrics feature: disabled
- CPU / SoC: `Apple M5 Max`
- Logical CPU parallelism: `18`
- Reported CPU frequency: `4608 MHz`
- Memory: `128 GB total`, `74.26 GB used`, `121.22 GB available`
- Swap: `0 B`
- Root disk: `1.81 TB APFS`, `390.05 GB used`, `1.43 TB available`
- Rust toolchain: `rustc 1.96.0 (ac68faa20 2026-05-25)`
- Build profile of the system info collector: `debug`
- Collector git branch / commit: `main` / `c405470f7350e0bbc01a6e2c25ab03ac2789c648`
- Runtime parallelism: `18`
- Power / thermal state during benchmark: not recorded

| Config | Shard Size | Encode (GiB/s) | Reconstruct (GiB/s) |
|--------|------------|----------------|---------------------|
| 10×4 | 4 KiB | 3.30 | 3.02 |
| 10×4 | 64 KiB | 3.62 | 3.52 |
| 10×4 | 1 MiB | 4.38 | 4.34 |

Supplementary observations from the same artifact set:

- `verify` on `10x4_4k` reached `3135.4851 MB/s` (`3.06 GiB/s`)
- `verify` on `10x4_64k` reached `3275.6813 MB/s` (`3.20 GiB/s`)
- `verify` on `10x4_1m` reached `4029.8207 MB/s` (`3.94 GiB/s`)
- `reconstruct_data` on `10x4_1m` reached `4590.9467 MB/s` (`4.48 GiB/s`)

The aarch64 curve is encouraging: throughput climbs steadily as shard size increases, and the
`1 MiB` case shows `reconstruct` staying close to `encode`, which matches the current NEON path's
large-block behavior.

### x86_64 (AMD EPYC)

**Hardware**: AMD EPYC 9V45 (96-core)
**Backend**: `rust-gfni-avx512` (auto-selected on the current host rerun)
**Feature**: `simd-avx2`
**Artifact source**: `benchmarks/small-file/2026-06-16-x86_64-linux-extended.{json,csv}`
**Profile**: `extended` (`iterations = 20` for `10x4_4k` / `10x4_1m`; dedicated `iterations = 40` rerun for `10x4_64k`)

**Benchmark Environment**
- OS: `Linux (Ubuntu 24.04)`
- Kernel: `Linux 6.17.0-1015-azure`
- Hostname: `rustfs-jumpbox`
- Target triple: `x86_64-linux-unknown`
- Architecture: `x86_64`
- Features: `std|simd-accel`
- Backend kind: `RustSimd`
- Backend override: `auto` (`override_honored = true`)
- Benchmark metrics feature: disabled
- CPU / SoC: `AMD EPYC 9V45 96-Core Processor`
- Logical CPU parallelism: `16`
- Reported CPU frequency: `4115 MHz`
- Memory: `31.34 GB total`, `2.75 GB used`, `28.59 GB available`
- Swap: `0 B`
- Root data disk: `/data/rustfs` on `xfs`, `499.76 GB total`, `35.49 GB used`, `464.26 GB available`
- Rust toolchain: `rustc 1.96.0 (ac68faa20 2026-05-25)`
- Git revision in the benchmark artifact: `b338c34`
| Config | Shard Size | Encode (GiB/s) | Reconstruct (GiB/s) |
|--------|------------|----------------|---------------------|
| 10×4 | 4 KiB | 6.39 | 7.51 |
| 10×4 | 64 KiB | 8.61 | 1.92 |
| 10×4 | 1 MiB | 6.30 | 4.32 |

Data sources:
- `benchmarks/small-file/2026-06-16-x86_64-linux-extended.csv` — current-host targeted rerun used for `10x4_4k` and `10x4_1m`
- `benchmarks/small-file/2026-06-16-x86_64-linux-10x4_64k-rerun.csv` — dedicated higher-iteration rerun used for `10x4_64k`
- `benchmarks/small-file/2026-05-27-x86_64-linux-extended.csv` — older archived x86_64 `auto` sample for historical comparison
- `benchmarks/x86_64-simd/comprehensive-x86_64-benchmark.json` — broader x86_64 encode-only matrix for deeper drill-down

> **Note**: The x86_64 table now reflects the `2026-06-16` current-host rerun, not the older `2026-05-27` archived sample. The newer rerun selected `rust-gfni-avx512` under `auto`, so these values are not directly comparable to the earlier `rust-avx2`-based cross-platform snapshot without accounting for backend-policy drift.

Supplementary observations from the current artifact set:

- `verify` on `10x4_4k` reached `7834.5150 MB/s` (`7.65 GiB/s`)
- `verify` on `10x4_64k` reached `10244.1047 MB/s` (`10.00 GiB/s`) in the dedicated `40`-iteration rerun
- `verify` on `10x4_1m` reached `6297.5276 MB/s` (`6.15 GiB/s`)
- `reconstruct_data` on `10x4_1m` reached `4923.6896 MB/s` (`4.81 GiB/s`)

### Interpreting The Current aarch64 vs x86 Gap

At first glance the x86_64 rerun looks stronger on `encode`, while `reconstruct` is mixed and
still workload-sensitive:

- aarch64 `10x4_1m`: `encode 4.38 GiB/s`, `reconstruct 4.34 GiB/s`
- x86_64 `10x4_1m`: `encode 6.30 GiB/s`, `reconstruct 4.32 GiB/s`

This should still **not** be read as “x86_64 is universally faster than aarch64” or vice versa.
The more accurate interpretation is:

1. These small-file results are primarily **in-memory** measurements, not disk-I/O benchmarks.
   The benchmark constructs shard buffers in memory, runs encode / verify / reconstruct loops,
   and only writes JSON / CSV artifacts after timing has finished.

2. The measured path includes more than raw SIMD math.
   It also captures buffer cloning, `Vec<Option<Vec<u8>>>` reconstruction setup, cache behavior,
   allocator behavior, and per-call orchestration overhead. That makes the result sensitive to:
   - single-core performance
   - memory latency / bandwidth
   - cache hierarchy
   - runtime backend selection

3. This is not a same-host comparison.
   The x86_64 numbers come from an Ubuntu 24.04 Azure VM (`rustfs-jumpbox`) with `16` visible
   CPUs, while the aarch64 numbers come from an Apple Silicon MacBook Pro host. The x86 CPU brand
   string is `AMD EPYC 9V45 96-Core Processor`, but only `16` CPUs / parallelism were exposed to
   the runtime in this benchmark environment. That alone can materially change Rayon scheduling,
   cache pressure, and sustained large-shard throughput.

4. The backend choice is part of the result, not just the ISA label.
   The current x86_64 rerun selected `rust-gfni-avx512` under `auto`, while the aarch64 sample
   used `rust-neon`.

5. The current numbers are mixed rather than uniformly better on one side.
   Against the aarch64 artifact, x86_64 is roughly:
   - `+93.8% / +148.6%` at `10x4_4k`
   - `+138.0% / -45.4%` at `10x4_64k`
   - `+44.0% / -0.3%` at `10x4_1m`
   for `encode / reconstruct`.

6. That pattern points to host and runtime-path differences more than a simple “x86 vs ARM” ISA
   conclusion. In particular, this rerun is strong on `encode`, but `reconstruct` remains
   workload-sensitive on the current x86_64 host, and `10x4_64k` stayed abnormally weak even
   after a dedicated higher-iteration rerun.

If we want a stricter architecture comparison, the next step is to rerun both hosts under the same
commit, same profile, same iteration count, and explicit backend controls where possible.

### x86_64 (Intel with GFNI)

**Status**: No data yet. Requires Intel Ice Lake (10th gen Xeon) or later.

Expected backends:
- `rust-gfni-avx512` (if AVX-512 + GFNI available)
- `rust-gfni-avx2` (if AVX2 + GFNI available)

---

## Methodology

### Metrics

- **Encode throughput**: `data_shards × shard_size / encode_time` (GiB/s)
- **Reconstruct throughput**: `data_shards × shard_size / reconstruct_time` (GiB/s)
- **Backend**: Auto-detected at runtime, reported in benchmark output

### Warm-up

- Criterion benchmarks: 3-second warm-up, 10 measurement iterations
- Smoke tests: No warm-up (CI validation only)

### Environment Variables

| Variable | Purpose |
|----------|---------|
| `RSE_BACKEND_OVERRIDE` | Force specific backend (`auto`, `scalar`, `rust-avx2`, etc.) |
| `RSE_WRITE_PROFILE_REPORT` | Export profiling data as JSON |
| `VALIDATION_PROFILE` | Smoke test profile (`fast`, `extended`) |
| `RS_PARALLEL_POLICY_MAX_JOBS` | Limit parallelism for reproducible results |

### Why Hardware Context Matters

The current report mixes data from different hosts and collection styles:

- Apple Silicon numbers come from small-file `extended` smoke artifacts
- AMD EPYC numbers combine archived `10x4` small-file samples and broader x86 benchmark summaries

When absolute throughput differs substantially, read the results together with:

- CPU / SoC class
- target triple
- selected SIMD backend
- profile / iteration count
- whether the source is a smoke artifact or a Criterion benchmark

Without that context, large gaps are easy to misread as regressions when they may simply reflect hardware class, runtime backend selection, or workload shape.

---

## Comparison with klauspost/reedsolomon (Go)

### Methodology Alignment

To compare Rust vs Go implementations:

1. Same hardware, same OS
2. Same (data, parity) configurations
3. Same shard sizes
4. Single-threaded comparison (set `RS_PARALLEL_POLICY_MAX_JOBS=1`)
5. Multi-threaded comparison (default parallelism)

### Go Benchmark Command

```bash
# In klauspost/reedsolomon repo:
go test -bench=BenchmarkEncode -benchtime=5s -cpu=1
go test -bench=BenchmarkReconstruct -benchtime=5s -cpu=1
```

### Key Differences

| Aspect | rustfs-erasure-codec (Rust) | klauspost/reedsolomon (Go) |
|--------|---------------------------|---------------------------|
| SIMD dispatch | Runtime (CPUID) | Runtime (CPUID) |
| GF backends | Scalar, SSSE3, AVX2, AVX-512, GFNI, NEON, VSX(feature-gated) | Scalar, SSSE3, AVX2, AVX-512, GFNI, NEON, ppc64le accel |
| Leopard codec | GF8 + GF16 | GF8 + GF16 |
| Parallelism | Rayon + configurable policy | Goroutines |
| Streaming API | Supported on `galois_8` block-based path | Supported via `NewStream()` |

---

## Action Items

1. **Extend Apple Silicon coverage** with Criterion `bandwidth` / `throughput_matrix` outputs in addition to small-file smoke artifacts
2. **Run benchmarks on Intel Ice Lake** to get GFNI numbers
3. **Run Go benchmarks on same hardware** for direct comparison
4. **Create comparison table** with normalized throughput (GiB/s per core)

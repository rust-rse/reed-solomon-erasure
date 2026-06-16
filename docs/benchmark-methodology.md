# Benchmark Methodology

## 1. Scope

This document standardizes how to run, collect, and compare benchmark results for this crate.

Targets:

- smoke regression (`tests/benchmark_smoke.rs`)
- small-file matrix (`tests/benchmark_small_files.rs`)
- throughput matrix (`benches/throughput_matrix.rs`)
- backend kernel benchmarks (`benches/galois_backend.rs`)

## 2. Baseline Commands

Smoke (fast regression):

```bash
cargo test --test benchmark_smoke benchmark_smoke_matrix_runs_and_exports_results -- --ignored --nocapture
```

Smoke profiles:

- `RSE_SMOKE_PROFILE=quick`: single small 4+2 / 64 KiB case for default release-check use
- `RSE_SMOKE_PROFILE=fast`: quick profile plus 10+4 / 1 MiB
- `RSE_SMOKE_PROFILE=extended`: full smoke matrix with higher default iteration count

Example:

```bash
RSE_SMOKE_PROFILE=quick cargo test --test benchmark_smoke benchmark_smoke_matrix_runs_and_exports_results -- --ignored --nocapture
```

Small-file matrix:

```bash
RSE_SMALL_FILE_PROFILE=fast \
cargo test --release --features "std simd-accel" \
  --test benchmark_small_files \
  benchmark_small_file_matrix_runs_and_exports_results -- --ignored --nocapture
```

Small-file profiles:

- `RSE_SMALL_FILE_PROFILE=quick`: `4+2` on `1 KiB / 4 KiB / 16 KiB / 64 KiB`
- `RSE_SMALL_FILE_PROFILE=fast`: `4+2` on `1 KiB / 4 KiB / 16 KiB / 64 KiB / 128 KiB / 256 KiB / 512 KiB`, plus `10+4` on `16 KiB / 64 KiB / 256 KiB / 512 KiB`
- `RSE_SMALL_FILE_PROFILE=extended`: full small-file baseline/update run through `1 MiB` for both `4+2` and `10+4`

Small-file helper script:

```bash
bash scripts/run_small_file_benchmark_matrix.sh
```

Repeated reconstruct planning reuse:

- for repeated `Option<Vec<u8>>` reconstruct workloads with a stable missing
  pattern, prefer `prepare_reconstruct_opt_workspace(...)` once and
  `reconstruct_opt_with_workspace(...)` inside the hot loop
- this avoids rebuilding the option-vec reconstruct plan on every call and is
  the preferred measurement shape when evaluating repeated serial reconstruct
  workloads such as `10x4_64k`

Throughput matrix:

```bash
cargo bench --bench throughput_matrix --features std
```

Backend kernels:

```bash
cargo bench --bench galois_backend --features std
```

SIMD smoke (when available):

```bash
cargo test --features "std simd-accel" --test benchmark_smoke benchmark_smoke_matrix_runs_and_exports_results -- --ignored --nocapture
```
Use `--ignored` here as well because the artifact-producing smoke benchmark is intentionally not part of the default `cargo test` surface:

```bash
cargo test --features "std simd-accel" --test benchmark_smoke benchmark_smoke_matrix_runs_and_exports_results -- --ignored --nocapture
```

## 3. Determinism Rules

- Use fixed seeds from `benches/common/mod.rs` (`BASE_SEED` + `derived_seed`).
- Do not mix old and new build artifacts when comparing results.
- For A/B comparisons, run both sides from clean builds.

## 4. Output Artifacts

Smoke outputs:

- `target/benchmark-smoke/smoke-results.json`
- `target/benchmark-smoke/smoke-results.csv`

Small-file outputs:

- `target/benchmark-smoke/small-file-results.json`
- `target/benchmark-smoke/small-file-results.csv`

Cache analysis outputs (tests):

- `target/benchmark-smoke/reconstruction-cache-stats.json`
- `target/benchmark-smoke/reconstruction-cache-patterns.csv`
- `target/benchmark-smoke/reconstruction-hotspot-results.json`
- `target/benchmark-smoke/reconstruction-hotspot-results.csv`

Artifact history rule:

- benchmark-smoke JSON/CSV artifacts are append-history ledgers, not single-run
  overwrite snapshots
- repeated executions append new records to the existing file
- when comparing one run to another, either filter by the newest appended rows
  or archive/copy the file before the next run

These artifact-producing reconstruction benchmarks are marked `#[ignore]` on purpose.
Run them explicitly so normal `cargo test` stays bounded and does not mix
correctness validation with long-running CPU-saturating workload measurement.

Explicit commands:

```bash
cargo test --features std benchmark_parallel_helpers_quantify_gain -- --ignored --nocapture
cargo test --features std benchmark_reconstruction_cache_patterns -- --ignored --nocapture
cargo test --features std benchmark_reconstruction_cache_layers -- --ignored --nocapture
cargo test --release --features "std simd-accel" benchmark_reconstruction_hotspots -- --ignored --nocapture
```

If you are debugging a hang-like symptom or want a lighter local sampling pass,
lower the inner loop count:

```bash
RSE_TEST_BENCH_ITERATIONS=1 \
  cargo test --features std benchmark_parallel_helpers_quantify_gain -- --ignored --nocapture
```

Regression gate inputs / outputs:

- baseline artifact: user-provided `smoke-results.json` via `RSE_SMOKE_BASELINE`
- current artifact: `target/benchmark-smoke/smoke-results.json`
- consistency sweep: `scripts/check_backend_consistency.sh`

Small-file gate inputs / outputs:

- baseline artifact: archived `small-file-results.json` or `small-file-results.csv`
- current artifact: `target/benchmark-smoke/small-file-results.json`
- recommended metric for small shards: `ns_per_iter`

Throughput profiling output (optional):

- `target/benchmark-smoke/throughput-profile-report.json` (when `RSE_WRITE_PROFILE_REPORT=1`)

## 4.1 Unified Artifact Schema

Every benchmark or profiling artifact should now expose a stable core envelope so
results from different phases can be compared without guessing field meaning.

Required core fields when applicable:

- `schema_version`
- `artifact_kind`
- `benchmark_metrics_enabled`
- `git_revision`
- `target_triple`
- `features`
- `backend`
- `backend_id`
- `backend_kind`
- `backend_override`
- `operation`
- `data_shards`
- `parity_shards`
- `shard_size`

Artifact-specific fields remain allowed, but the core envelope should stay stable.

Current artifact mapping:

- `smoke-results`: regression-oriented throughput snapshot
- `small-file-results`: small-shard latency/throughput snapshot
- `parallel-helper-results`: serial vs optimized helper comparison
- `reconstruction-hotspot-results`: reconstruct workload comparison
- `reconstruction-cache-stats`: cache observability snapshot
- `reconstruction-cache-patterns`: cache workload comparison
- `throughput-profile-report`: runtime/profile counter export

Current schema version:

- `schema_version = 1`

Notes:

- JSON artifacts should always include `schema_version` and `artifact_kind`.
- CSV artifacts should keep a fixed leading column order that starts with
  `schema_version,artifact_kind`.
- Scripts may ignore extra fields, but should not assume field order beyond the
  documented CSV header.

## 5. Comparison Protocol

1. Record commit hash and feature set.
2. Keep backend selection explicit:
   - default runtime dispatch, or
   - force backend via `RSE_BACKEND_OVERRIDE`.
3. For each operation (`encode`, `verify`, `reconstruct`, `reconstruct_data`), compare:
   - throughput (`throughput_mb_s`)
   - latency (`ns_per_iter`) when available
4. For small-file decisions, prioritize `ns_per_iter` over `throughput_mb_s`, especially for `1 KiB` to `64 KiB`.
5. Use median of repeated runs for decisions; avoid single-run conclusions.
6. If only `1 KiB` or `4 KiB` cases look bad while neighboring points are stable, rerun that case in isolation with a higher iteration count before treating it as a real regression.
7. If a full `extended` run shows broad small-file regressions but targeted filtered reruns disagree, treat the filtered reruns as the more trustworthy signal for code-change decisions.

Example drill-down:

```bash
RSE_SMALL_FILE_PROFILE=extended \
RSE_SMALL_FILE_CASE_FILTER=10x4_1k \
RSE_SMALL_FILE_ITERATIONS=40 \
cargo test --release --features "std simd-accel" \
  --test benchmark_small_files \
  benchmark_small_file_matrix_runs_and_exports_results -- --ignored --nocapture
```

## 6. Cache Metrics Interpretation

For reconstruction cache stats:

- `requests`: total decode-matrix lookup attempts
- `hits`: cache hits
- `misses`: cache misses
- `inserts`: inserted decode-matrix entries
- `evictions`: entries evicted by LRU capacity pressure

Derived analysis fields:

- `hit_rate = hits / requests`
- `reuse_ratio = hits / inserts`
- `miss_cost_per_request = misses / requests`

## 7. Recommended Release Checklist

Use the fixed script:

```bash
./scripts/release-check.sh
```

This script runs test gates for:

- unit/integration correctness
- self-test entry
- benchmark smoke
- smoke regression gate (when `RSE_SMOKE_BASELINE` is set)
- small-file regression gate (when `RUN_SMALL_FILE_GATE=1` and `RSE_SMALL_FILE_BASELINE` is set)
- backend/ISA consistency sweep (when `RUN_BACKEND_CONSISTENCY=1`)
- `no_std` build path
- `std` and optional `simd-accel` paths

If you want a lower-noise local pass that avoids test-level parallelism and
keeps smoke work on the smallest profile, use:

```bash
./scripts/serial-test-check.sh
```

## 8. Smoke Regression Gate

Compare a fresh smoke run against a checked-in or archived baseline:

```bash
RSE_SMOKE_BASELINE=/abs/path/to/smoke-results.json \
python3 scripts/check_benchmark_regression.py \
  --baseline "$RSE_SMOKE_BASELINE" \
  --current target/benchmark-smoke/smoke-results.json
```

Default allowed regressions:

- `encode`: 10%
- `verify`: 12%
- `reconstruct`: 15%
- `reconstruct_data`: 15%

Thresholds can be overridden per operation:

```bash
python3 scripts/check_benchmark_regression.py \
  --baseline old.json \
  --current new.json \
  --threshold reconstruct=0.18 \
  --threshold reconstruct_data=0.18
```

The same tool also supports small-file latency checks:

```bash
python3 scripts/check_benchmark_regression.py \
  --baseline old-small-file.json \
  --current target/benchmark-smoke/small-file-results.json \
  --metric ns_per_iter \
  --threshold verify_with_buffer=0.12 \
  --require-case encode:4:2:1024 \
  --require-case verify_with_buffer:4:2:4096 \
  --require-case reconstruct_data:10:4:65536
```

`release-check.sh` can also run the small-file gate:

```bash
VALIDATION_PROFILE=extended \
RUN_SMALL_FILE_GATE=1 \
RSE_SMALL_FILE_PROFILE=extended \
RSE_SMALL_FILE_BASELINE=/abs/path/to/small-file-results.json \
./scripts/release-check.sh
```

Recommended defaults for small-file checks:

- `RSE_SMALL_FILE_METRIC=ns_per_iter`
- `RSE_SMALL_FILE_PROFILE=fast` for day-to-day validation
- `RSE_SMALL_FILE_PROFILE=extended` when refreshing an archived baseline

## 8.1 Baseline Update Governance

Updating a benchmark baseline is allowed only when the new baseline reflects an
intentional and verified steady-state change.

Allowed cases:

1. An intentional algorithm or scheduling change has landed and the previous
   baseline no longer represents the intended default behavior.
2. Backend priority or dispatch behavior changed intentionally and the new
   default path is the one being validated.
3. A benchmark schema migration occurred, and the old artifact can no longer be
   compared mechanically without lossy translation.

Disallowed cases:

1. A single noisy run looks slower or faster than expected.
2. Different machines, CPUs, or feature sets are being compared as if they were
   the same baseline lineage.
3. The current run used a different backend override, different workload shape,
   or different build cleanliness than the archived baseline without explicit
   justification.

Minimum evidence before refreshing a baseline:

1. Same-machine comparison
2. Matching feature set
3. Matching or explicitly documented backend selection
4. Clean-build comparison when performance-sensitive conclusions are involved
5. Repeated runs with median-oriented interpretation
6. Short rationale describing why the old baseline is no longer authoritative

Recommended baseline refresh note template:

- reason:
- old baseline:
- new baseline:
- machine / cpu:
- feature set:
- backend mode:
- repeated-run summary:
- expected long-term effect:

## 9. Backend Consistency Sweep

When SIMD is available, run the reusable backend override sweep:

```bash
RUN_BACKEND_CONSISTENCY=1 ./scripts/release-check.sh
```

Or invoke it directly:

```bash
bash scripts/check_backend_consistency.sh
```

Host-specific override smoke matrix helpers are also available:

```bash
bash scripts/run_x86_backend_smoke_matrix.sh
bash scripts/run_aarch64_backend_smoke_matrix.sh
```

On Apple Silicon or other `aarch64` hosts, the ARM64 sweep validates
`auto`, `scalar`, and `rust-neon`.

## 9.1 Reconstruction Hotspot Gate

The phase-5 hotspot workloads can now be promoted from ad hoc benchmarks to a
repeatable regression gate.

Generate the current hotspot artifact:

```bash
cargo test --release --features "std simd-accel" benchmark_reconstruction_hotspots -- --ignored --nocapture
```

Compare it against a saved baseline:

```bash
python3 scripts/check_reconstruction_hotspot_gate.py \
  --baseline /path/to/reconstruction-hotspot-results.json \
  --current target/benchmark-smoke/reconstruction-hotspot-results.json \
  --require-scenario reconstruct_data_missing_1_data \
  --require-scenario reconstruct_some_required_1_of_2_missing_data
```

Use `--min-speedup scenario=value` only for scenarios that have already been
proven to require a positive optimization margin on the target machine. Do not
assume every hotspot candidate is universally faster across all architectures.

Or drive it through the release workflow:

```bash
RUN_RECONSTRUCTION_HOTSPOT_GATE=1 \
RSE_RECONSTRUCTION_HOTSPOT_BASELINE=/path/to/reconstruction-hotspot-results.json \
./scripts/release-check.sh
```

## 10. ISA Integration Template

Use this template whenever adding a new SIMD or ISA-specific backend.

Checklist:

1. Add a named backend entry with stable `name`, `id`, and `kind`.
2. Connect runtime dispatch without breaking scalar fallback.
3. Add or update `RSE_BACKEND_OVERRIDE` support for the new backend name.
4. Add scalar correctness comparison tests.
5. Include the backend in reusable consistency sweep scripts when the platform
   can expose it.
6. Validate smoke benchmark output with the new backend selected explicitly.
7. Validate kernel benchmark output where applicable.
8. Document whether the backend is experimental, candidate-default, or
   fallback-only.

Minimum deliverables:

- correctness tests
- backend override coverage
- consistency sweep coverage
- smoke or workload benchmark evidence
- dispatch and fallback notes

## 11. Matrix Mode Integration Template

Use this template whenever extending `MatrixMode` or adding a new matrix
construction strategy.

Checklist:

1. State the target workload or compatibility need.
2. Preserve current default behavior unless the change is explicitly intended to
   alter defaults.
3. Add encode correctness coverage.
4. Add reconstruction correctness coverage.
5. Check interaction with `reconstruct_data` and `reconstruct_some`.
6. Evaluate benchmark impact on at least one representative workload.
7. Update API and governance docs together with the implementation.

Minimum deliverables:

- constructor / option-path coverage
- correctness tests for encode and reconstruct flows
- documented benchmark impact
- rollback story if the mode is not yet ready for general default use

## 12. Metrics Feature Gate

Heavy benchmark and reconstruction metrics are now governed by the
`benchmark-metrics` feature.

Current behavior:

- default features keep `benchmark-metrics` enabled
- disabling default features or explicitly removing `benchmark-metrics` should
  preserve correctness behavior while allowing metric fields to degrade safely
  to zero-valued snapshots

Guidelines:

1. Do not make correctness depend on metrics collection.
2. Benchmark artifacts should record whether metrics were enabled through
   `benchmark_metrics_enabled`.
3. When metrics are disabled, scripts should treat zero-valued counters as
   “metrics unavailable in this build” unless the workload naturally produces
   zeros.

## 13. Native AVX2 Same-Machine Benchmark Runbook

Use this workflow when deciding whether `rust-avx2` is ready to outrank
`simd-c` on a real x86_64 AVX2 machine.

### 13.1 Preconditions

1. Run on a native x86_64 host that actually exposes AVX2.
2. Keep machine, governor, and feature set stable across the whole run.
3. Prefer a clean worktree or at least a clearly recorded commit / diff scope.
4. Avoid mixing incremental artifacts from unrelated benchmark sessions.

### 13.2 Required commands

Primary collection flow:

```bash
bash scripts/collect_x86_simd_benchmarks.sh
```

This flow collects:

- release smoke results per backend override
- `galois_backend` criterion results
- machine summary JSON under `benchmarks/x86_64-simd/`

### 13.3 Minimum evidence package

The benchmark package is not considered decision-ready unless it includes:

1. release smoke comparisons for:
   - `auto`
   - `simd-c`
   - `rust-avx2`
   - `scalar`
2. kernel benchmark comparisons for:
   - `galois_mul_slice`
   - `galois_mul_slice_xor`
3. override-honored checks with no silent mismatch for the compared backends
4. archived machine JSON in `benchmarks/x86_64-simd/`

### 13.4 Decision template

When judging whether `rust-avx2` may become the default x86_64 priority, record:

- machine json:
- commit or diff scope:
- compared backends:
- smoke winner by operation:
- kernel winner by operation:
- recommended priority from summary script:
- diverges from current runtime priority:
- repeated-run stability conclusion:
- adoption decision:
- fallback plan:

### 13.5 Stability rule

Do not promote `rust-avx2` based on one archived run alone.

Recommended minimum:

1. At least two same-machine archived runs
2. No correctness or override mismatch anomalies
3. No contradictory conclusion caused solely by noisy single-run outliers
4. If recommendation changes across runs, prefer the backend that remains
   competitive across smoke and kernel results rather than blindly following one
   aggregate score

### 13.6 Promotion decision classes

Use one of the following final labels:

- `candidate-only`: correctness is good, but performance evidence is not yet stable
- `candidate-default`: evidence is strong enough to test a default-priority change
- `fallback-only`: backend remains useful for explicit override or niche hosts, but
  should not be promoted

### 13.7 Ready-To-Paste Result Record

Use the following record when the AVX2 run finishes:

```text
machine json:
run meta:
commit or diff scope:
compared backends:
smoke winner by operation:
kernel winner by operation:
recommended priority from summary script:
diverges from current runtime priority:
override mismatch count:
repeated-run stability conclusion:
adoption decision:
fallback plan:
follow-up action:
```

Recommended follow-up action values:

- `promote-rust-avx2-candidate`
- `keep-current-default-and-retest`
- `treat-as-fallback-only`

### 13.8 Exit Criteria

Treat a same-machine AVX2 run as decision-ready only when all of the following
are true:

1. at least one archived machine JSON and matching `run-meta.json` exist
2. release smoke evidence is present for the compared backends
3. kernel benchmark evidence is present for the compared backends
4. `override_mismatch_count` is zero
5. a final adoption label has been chosen
6. the result has been written back to the task board and phase 4 document

## 14. Archived X86 Summary Interpretation

The archived JSON under `benchmarks/x86_64-simd/` should be interpreted as
evidence artifacts, not as automatic truth.

Important fields:

- `rankings_10x4_1m`
- `criterion_rankings`
- `recommended_default_priority`
- `release_smoke_override_mismatches`

Interpretation guidance:

1. `recommended_default_priority` is a benchmark-driven suggestion, not an
   unconditional promotion decision.
2. `diverges_from_current_runtime_priority_x86=true` means the archived result
   disagrees with the current runtime order and must be reviewed manually.
3. If different archived runs recommend different orders, treat the result as
   unstable until the reason is explained or repeated runs converge.

## 15. ARM64 Profiling And Feature-Detect Contract

This section standardizes how aarch64 backend evolution should expose feature
detect signals and profiling fields so a future SVE backend can plug into the
same observability story without redefining the rules.

### 15.1 Feature-Detect Rules

1. aarch64 capability detection should flow through `Aarch64FeatureSet`.
2. New aarch64 backend candidates should add explicit fields there before
   dispatch logic starts branching on ad hoc conditionals elsewhere.
3. Placeholder capability fields are allowed when they do not change current
   backend priority or runtime behavior.
4. A new backend must not change existing dispatch order until correctness,
   override behavior, and benchmark evidence are in place.

### 15.2 Override Rules

For each aarch64 backend candidate:

1. `RSE_BACKEND_OVERRIDE` must map to a stable backend name.
2. Strict override mode must either honor the request or fail loudly.
3. Backend metadata should remain consistent across:
   - `active_backend_name()`
   - `active_backend_id()`
   - `active_backend_kind()`
   - smoke result metadata fields

### 15.3 Profiling Rules

1. Profiling should distinguish implementation work from tail fallback work.
2. Runtime profile fields should remain comparable across ARM64 experiments.
3. Backend-local profiling is allowed, but it should not replace the shared
   runtime/profile counters already exported by the benchmark flow.
4. If a future SVE backend adds counters, it should follow the same naming style
   as the current NEON profiling surface.

### 15.4 Minimum Validation For A Future SVE Backend

Before a future `rust-sve` backend is considered usable, it should add:

1. dispatch priority tests
2. scalar correctness comparison
3. override metadata validation
4. smoke metadata verification
5. workload and kernel benchmark evidence

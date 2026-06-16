#!/usr/bin/env bash

set -euo pipefail

VALIDATION_PROFILE="${VALIDATION_PROFILE:-fast}"
STRICT_RELEASE=0

if [[ "${VALIDATION_PROFILE}" == "release" ]]; then
  STRICT_RELEASE=1
  VALIDATION_PROFILE="extended"
  RUN_BACKEND_CONSISTENCY=1
  RUN_SMALL_FILE_GATE=1
  RUN_RECONSTRUCTION_HOTSPOT_GATE=1
  RUN_SIMD_ACCEL_TESTS=1
fi

require_strict_baseline() {
  local var_name="$1"
  local name="$2"

  if [[ -z "${!var_name:-}" ]]; then
    if [[ "${STRICT_RELEASE}" == "1" ]]; then
      echo "[release-check] ERROR: strict release mode requires ${var_name} for ${name}"
      return 1
    fi
    echo "[release-check] ${name} without baseline compare (set ${var_name}=...)"
    return 0
  fi

  return 0
}

run() {
  echo
  echo "[release-check] $*"
  "$@"
}

run_smoke_profile() {
  local profile="$1"
  shift
  run env RSE_SMOKE_PROFILE="${profile}" "$@"
}

run_backend_override_matrix() {
  case "$(uname -m)" in
    x86_64)
      if [[ "${RUN_X86_SIMD_OVERRIDE_MATRIX:-1}" != "1" ]]; then
        echo
        echo "[release-check] skipping x86 override matrix (set RUN_X86_SIMD_OVERRIDE_MATRIX=1)"
        return
      fi
      run bash scripts/run_x86_backend_smoke_matrix.sh
      ;;
    arm64|aarch64)
      if [[ "${RUN_AARCH64_SIMD_OVERRIDE_MATRIX:-1}" != "1" ]]; then
        echo
        echo "[release-check] skipping aarch64 override matrix (set RUN_AARCH64_SIMD_OVERRIDE_MATRIX=1)"
        return
      fi
      run bash scripts/run_aarch64_backend_smoke_matrix.sh
      ;;
    *)
      echo
      echo "[release-check] skipping host override matrix (unsupported arch $(uname -m))"
      ;;
  esac
}

run_reconstruction_hotspot_gate() {
  if [[ "${RUN_RECONSTRUCTION_HOTSPOT_GATE:-0}" != "1" ]]; then
    echo
    echo "[release-check] skipping reconstruction hotspot gate (set RUN_RECONSTRUCTION_HOTSPOT_GATE=1)"
    return
  fi

  run cargo test --release --features "std simd-accel" \
    benchmark_reconstruction_hotspots -- --ignored --nocapture

  if [[ -n "${RSE_RECONSTRUCTION_HOTSPOT_BASELINE:-}" ]]; then
    run python3 scripts/check_reconstruction_hotspot_gate.py \
      --baseline "${RSE_RECONSTRUCTION_HOTSPOT_BASELINE}" \
      --current target/benchmark-smoke/reconstruction-hotspot-results.json \
      --require-scenario reconstruct_data_missing_1_data \
      --require-scenario reconstruct_data_missing_2_data \
      --require-scenario reconstruct_data_missing_data_plus_parity \
      --require-scenario reconstruct_data_32x16_missing_2_data \
      --require-scenario reconstruct_some_required_1_of_2_missing_data \
      --require-scenario reconstruct_some_required_2_of_3_missing_data \
      --require-scenario reconstruct_some_required_data_and_skip_parity \
      --require-scenario reconstruct_some_32x16_required_2_of_4_missing_data
  elif ! require_strict_baseline "RSE_RECONSTRUCTION_HOTSPOT_BASELINE" "reconstruction hotspot gate"; then
    return 1
  else
    echo
    echo "[release-check] hotspot results generated without baseline compare (set RSE_RECONSTRUCTION_HOTSPOT_BASELINE=/path/to/reconstruction-hotspot-results.json)"
  fi
}

run_small_file_gate() {
  if [[ "${RUN_SMALL_FILE_GATE:-0}" != "1" ]]; then
    echo
    echo "[release-check] skipping small-file gate (set RUN_SMALL_FILE_GATE=1)"
    return
  fi

  run env RSE_SMALL_FILE_PROFILE="${RSE_SMALL_FILE_PROFILE:-fast}" \
    cargo test --release --features "std simd-accel" \
    --test benchmark_small_files \
    benchmark_small_file_matrix_runs_and_exports_results -- --ignored --nocapture

  if [[ -n "${RSE_SMALL_FILE_BASELINE:-}" ]]; then
    run python3 scripts/check_benchmark_regression.py \
      --baseline "${RSE_SMALL_FILE_BASELINE}" \
      --current target/benchmark-smoke/small-file-results.json \
      --metric "${RSE_SMALL_FILE_METRIC:-ns_per_iter}" \
      --threshold encode=0.12 \
      --threshold verify=0.12 \
      --threshold verify_with_buffer=0.12 \
      --threshold reconstruct=0.18 \
      --threshold reconstruct_data=0.18 \
      --require-case encode:4:2:1024 \
      --require-case verify_with_buffer:4:2:4096 \
      --require-case reconstruct:4:2:16384 \
      --require-case reconstruct_data:10:4:65536
  elif ! require_strict_baseline "RSE_SMALL_FILE_BASELINE" "small-file gate"; then
    return 1
  else
    echo
    echo "[release-check] small-file results generated without baseline compare (set RSE_SMALL_FILE_BASELINE=/path/to/small-file-results.json)"
  fi
}

run_fast_checks() {
  run cargo check --tests
  run cargo test --test selftest
  run_smoke_profile quick cargo test --test golden_vectors --test benchmark_smoke -- --ignored --nocapture
  run cargo test --no-default-features
  run cargo test --features std
}

run_extended_checks() {
  if [[ -n "${RSE_SMOKE_BASELINE:-}" ]]; then
    run python3 scripts/check_benchmark_regression.py \
      --baseline "${RSE_SMOKE_BASELINE}" \
      --current target/benchmark-smoke/smoke-results.json \
      --require-case encode:4:2:65536 \
      --require-case encode:10:4:1048576 \
      --require-case verify:10:4:1048576 \
      --require-case reconstruct:10:4:1048576 \
      --require-case reconstruct_data:10:4:1048576
  elif ! require_strict_baseline "RSE_SMOKE_BASELINE" "smoke benchmark gate"; then
    return 1
  else
    echo
    echo "[release-check] skipping benchmark regression gate (set RSE_SMOKE_BASELINE=/path/to/smoke-results.json)"
  fi

  if [[ "${RUN_BACKEND_CONSISTENCY:-0}" == "1" ]]; then
    run bash scripts/check_backend_consistency.sh
  else
    echo
    echo "[release-check] skipping backend consistency sweep (set RUN_BACKEND_CONSISTENCY=1)"
  fi

  if [[ "${RUN_SIMD_ACCEL_TESTS:-1}" == "1" ]]; then
    run cargo test --features "std simd-accel"
    run_backend_override_matrix
    run_small_file_gate
    run_reconstruction_hotspot_gate
  else
    echo
    echo "[release-check] skipping simd-accel tests (RUN_SIMD_ACCEL_TESTS=0)"
  fi
}

run_fast_checks

if [[ "${VALIDATION_PROFILE}" == "extended" || "${VALIDATION_PROFILE}" == "release" ]]; then
  run_extended_checks
else
  echo
  echo "[release-check] fast profile complete; skip extended checks (set VALIDATION_PROFILE=extended)"
fi

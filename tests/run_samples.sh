#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BIN="$ROOT_DIR/build/exec/jsonparser"

if [[ ! -x "$BIN" ]]; then
  idris2 "$ROOT_DIR/Main.idr" -o jsonparser
fi

run_case() {
  local name="$1"
  local file="$2"
  local expect_code="$3"

  echo
  echo "== $name =="
  set +e
  "$BIN" "$file"
  local code=$?
  set -e

  if [[ $code -ne $expect_code ]]; then
    echo "Expected exit code $expect_code, got $code" >&2
    exit 1
  fi
  echo
}

run_case "valid" "$ROOT_DIR/samples/sample-valid.json" 0
run_case "array" "$ROOT_DIR/samples/sample-array.json" 0
run_case "utf" "$ROOT_DIR/samples/sample-utf.json" 0
run_case "invalid" "$ROOT_DIR/samples/sample-invalid.json" 0

echo "All sample runs completed with expected exit codes."

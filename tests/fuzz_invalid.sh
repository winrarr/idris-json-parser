#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BIN="$ROOT_DIR/build/exec/jsonparser"

if [[ ! -x "$BIN" ]] || find "$ROOT_DIR" -name '*.idr' -newer "$BIN" -print -quit | grep -q .; then
  idris2 "$ROOT_DIR/Main.idr" -o jsonparser
fi

# Fuzz with guaranteed-invalid inputs.
# Each payload includes a character that is invalid at top-level in JSON (e.g., '@').
for i in $(seq 1 100); do
  payload="@${RANDOM}${RANDOM}"
  output=$(printf '%s' "$payload" | "$BIN")
  if ! grep -q "Parse error." <<< "$output"; then
    echo "Unexpected parse success for payload: $payload" >&2
    echo "$output" >&2
    exit 1
  fi
 done

echo "Invalid fuzz tests completed."

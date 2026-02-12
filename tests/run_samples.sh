#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BIN="$ROOT_DIR/build/exec/jsonparser"

if [[ ! -x "$BIN" ]]; then
  idris2 "$ROOT_DIR/Main.idr" -o jsonparser
fi

if [[ -t 1 ]]; then
  COLOR_RED="\033[31m"
  COLOR_GREEN="\033[32m"
  COLOR_YELLOW="\033[33m"
  COLOR_RESET="\033[0m"
else
  COLOR_RED=""
  COLOR_GREEN=""
  COLOR_YELLOW=""
  COLOR_RESET=""
fi

pass_count=0
fail_count=0
declare -a failures

run_case() {
  local name="$1"
  local file="$2"
  local expect_pattern="$3"

  printf "%-32s" "$name"
  local output
  output="$("$BIN" "$file")"
  if ! grep -q "$expect_pattern" <<< "$output"; then
    echo -e "${COLOR_RED}FAIL${COLOR_RESET}"
    failures+=("$name: expected '$expect_pattern', got: $output")
    fail_count=$((fail_count + 1))
    return 0
  fi
  echo -e "${COLOR_GREEN}OK${COLOR_RESET}"
  pass_count=$((pass_count + 1))
}

run_case "valid" "$ROOT_DIR/samples/sample-valid.json" "Parsed successfully:"
run_case "array" "$ROOT_DIR/samples/sample-array.json" "Parsed successfully:"
run_case "utf" "$ROOT_DIR/samples/sample-utf.json" "Parsed successfully:"
run_case "valid-empty" "$ROOT_DIR/samples/sample-valid-empty.json" "Parsed successfully:"
run_case "valid-empty-array" "$ROOT_DIR/samples/sample-valid-empty-array.json" "Parsed successfully:"
run_case "valid-whitespace" "$ROOT_DIR/samples/sample-valid-whitespace.json" "Parsed successfully:"
run_case "valid-numbers" "$ROOT_DIR/samples/sample-valid-numbers.json" "Parsed successfully:"
run_case "valid-strings" "$ROOT_DIR/samples/sample-valid-strings.json" "Parsed successfully:"
run_case "valid-deep" "$ROOT_DIR/samples/sample-valid-deep.json" "Parsed successfully:"
run_case "invalid" "$ROOT_DIR/samples/sample-invalid.json" "Parse error."
run_case "invalid-plus" "$ROOT_DIR/samples/sample-invalid-plus.json" "Parse error."
run_case "invalid-trailing-comma" "$ROOT_DIR/samples/sample-invalid-trailing-comma.json" "Parse error."
run_case "invalid-unclosed-string" "$ROOT_DIR/samples/sample-invalid-unclosed-string.json" "Parse error."
run_case "invalid-control-char" "$ROOT_DIR/samples/sample-invalid-control-char.json" "Parse error."
run_case "invalid-leading-zero" "$ROOT_DIR/samples/sample-invalid-leading-zero.json" "Parse error."
run_case "invalid-dot-leading" "$ROOT_DIR/samples/sample-invalid-dot-leading.json" "Parse error."
run_case "invalid-missing-comma" "$ROOT_DIR/samples/sample-invalid-missing-comma.json" "Parse error."
run_case "invalid-double-comma" "$ROOT_DIR/samples/sample-invalid-double-comma.json" "Parse error."
run_case "invalid-bare-identifier" "$ROOT_DIR/samples/sample-invalid-bare-identifier.json" "Parse error."
run_case "invalid-escape" "$ROOT_DIR/samples/sample-invalid-escape.json" "Parse error."
run_case "invalid-bad-unicode" "$ROOT_DIR/samples/sample-invalid-bad-unicode.json" "Parse error."
run_case "invalid-low-surrogate" "$ROOT_DIR/samples/sample-invalid-low-surrogate.json" "Parse error."
run_case "invalid-high-surrogate-no-low" "$ROOT_DIR/samples/sample-invalid-high-surrogate-no-low.json" "Parse error."
run_case "invalid-unexpected-char" "$ROOT_DIR/samples/sample-invalid-unexpected-char.json" "Parse error."
run_case "invalid-array-missing-comma" "$ROOT_DIR/samples/sample-invalid-array-missing-comma.json" "Parse error."
run_case "invalid-unclosed-array" "$ROOT_DIR/samples/sample-invalid-unclosed-array.json" "Parse error."
run_case "invalid-unclosed-object" "$ROOT_DIR/samples/sample-invalid-unclosed-object.json" "Parse error."
run_case "valid-escapes" "$ROOT_DIR/samples/sample-valid-escapes.json" "Parsed successfully:"

printf "%-32s" "fuzz-invalid"
if "$ROOT_DIR/tests/fuzz_invalid.sh" >/dev/null; then
  echo -e "${COLOR_GREEN}OK${COLOR_RESET}"
  pass_count=$((pass_count + 1))
else
  echo -e "${COLOR_RED}FAIL${COLOR_RESET}"
  failures+=("fuzz-invalid: fuzz_invalid.sh reported failure")
  fail_count=$((fail_count + 1))
fi

echo
echo -e "Summary: ${COLOR_GREEN}${pass_count} passed${COLOR_RESET}, ${COLOR_RED}${fail_count} failed${COLOR_RESET}"
if [[ $fail_count -ne 0 ]]; then
  echo -e "${COLOR_YELLOW}Failures:${COLOR_RESET}"
  for item in "${failures[@]}"; do
    echo "$item"
  done
  exit 1
fi

echo "All sample runs completed with expected exit codes."

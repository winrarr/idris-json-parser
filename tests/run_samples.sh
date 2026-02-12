#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BIN_JSON="$ROOT_DIR/build/exec/jsonparser"
BIN_YAML="$ROOT_DIR/build/exec/yamlparser"
BIN_AUTO="$ROOT_DIR/build/exec/autoparser"

ensure_bin() {
  local src="$1"
  local bin="$2"
  local name="$3"

  if [[ ! -x "$bin" ]] || find "$ROOT_DIR" -name '*.idr' -newer "$bin" -print -quit | grep -q .; then
    idris2 "$src" -o "$name"
  fi
}

ensure_bin "$ROOT_DIR/MainJSON.idr" "$BIN_JSON" "jsonparser"
ensure_bin "$ROOT_DIR/MainYAML.idr" "$BIN_YAML" "yamlparser"
ensure_bin "$ROOT_DIR/Main.idr" "$BIN_AUTO" "autoparser"

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
  local bin="$1"
  local name="$2"
  local file="$3"
  local expect_pattern="$4"

  printf "%-42s" "$name"
  local output
  output="$("$bin" "$file")"
  if ! grep -q "$expect_pattern" <<< "$output"; then
    echo -e "${COLOR_RED}FAIL${COLOR_RESET}"
    failures+=("$name: expected '$expect_pattern', got: $output")
    fail_count=$((fail_count + 1))
    return 0
  fi
  echo -e "${COLOR_GREEN}OK${COLOR_RESET}"
  pass_count=$((pass_count + 1))
}

run_case_stdin() {
  local bin="$1"
  local name="$2"
  local file="$3"
  local expect_pattern="$4"

  printf "%-42s" "$name"
  local output
  output="$(cat "$file" | "$bin")"
  if ! grep -q "$expect_pattern" <<< "$output"; then
    echo -e "${COLOR_RED}FAIL${COLOR_RESET}"
    failures+=("$name: expected '$expect_pattern', got: $output")
    fail_count=$((fail_count + 1))
    return 0
  fi
  echo -e "${COLOR_GREEN}OK${COLOR_RESET}"
  pass_count=$((pass_count + 1))
}

run_case "$BIN_JSON" "valid" "$ROOT_DIR/samples/sample-valid.json" "Parsed successfully"
run_case "$BIN_JSON" "array" "$ROOT_DIR/samples/sample-array.json" "Parsed successfully"
run_case "$BIN_JSON" "utf" "$ROOT_DIR/samples/sample-utf.json" "Parsed successfully"
run_case "$BIN_JSON" "valid-empty" "$ROOT_DIR/samples/sample-valid-empty.json" "Parsed successfully"
run_case "$BIN_JSON" "valid-empty-array" "$ROOT_DIR/samples/sample-valid-empty-array.json" "Parsed successfully"
run_case "$BIN_JSON" "valid-whitespace" "$ROOT_DIR/samples/sample-valid-whitespace.json" "Parsed successfully"
run_case "$BIN_JSON" "valid-numbers" "$ROOT_DIR/samples/sample-valid-numbers.json" "Parsed successfully"
run_case "$BIN_JSON" "valid-strings" "$ROOT_DIR/samples/sample-valid-strings.json" "Parsed successfully"
run_case "$BIN_JSON" "valid-deep" "$ROOT_DIR/samples/sample-valid-deep.json" "Parsed successfully"
run_case "$BIN_JSON" "invalid" "$ROOT_DIR/samples/sample-invalid.json" "Parse error."
run_case "$BIN_JSON" "invalid-plus" "$ROOT_DIR/samples/sample-invalid-plus.json" "Parse error."
run_case "$BIN_JSON" "invalid-trailing-comma" "$ROOT_DIR/samples/sample-invalid-trailing-comma.json" "Parse error."
run_case "$BIN_JSON" "invalid-unclosed-string" "$ROOT_DIR/samples/sample-invalid-unclosed-string.json" "Parse error."
run_case "$BIN_JSON" "invalid-control-char" "$ROOT_DIR/samples/sample-invalid-control-char.json" "Parse error."
run_case "$BIN_JSON" "invalid-leading-zero" "$ROOT_DIR/samples/sample-invalid-leading-zero.json" "Parse error."
run_case "$BIN_JSON" "invalid-dot-leading" "$ROOT_DIR/samples/sample-invalid-dot-leading.json" "Parse error."
run_case "$BIN_JSON" "invalid-missing-comma" "$ROOT_DIR/samples/sample-invalid-missing-comma.json" "Parse error."
run_case "$BIN_JSON" "invalid-double-comma" "$ROOT_DIR/samples/sample-invalid-double-comma.json" "Parse error."
run_case "$BIN_JSON" "invalid-bare-identifier" "$ROOT_DIR/samples/sample-invalid-bare-identifier.json" "Parse error."
run_case "$BIN_JSON" "invalid-escape" "$ROOT_DIR/samples/sample-invalid-escape.json" "Parse error."
run_case "$BIN_JSON" "invalid-bad-unicode" "$ROOT_DIR/samples/sample-invalid-bad-unicode.json" "Parse error."
run_case "$BIN_JSON" "invalid-low-surrogate" "$ROOT_DIR/samples/sample-invalid-low-surrogate.json" "Parse error."
run_case "$BIN_JSON" "invalid-high-surrogate-no-low" "$ROOT_DIR/samples/sample-invalid-high-surrogate-no-low.json" "Parse error."
run_case "$BIN_JSON" "invalid-nonjson-whitespace" "$ROOT_DIR/samples/sample-invalid-nonjson-whitespace.json" "Parse error."
run_case "$BIN_JSON" "invalid-nonjson-whitespace-between" "$ROOT_DIR/samples/sample-invalid-nonjson-whitespace-between.json" "Parse error."
run_case "$BIN_JSON" "invalid-nonjson-whitespace-eof" "$ROOT_DIR/samples/sample-invalid-nonjson-whitespace-eof.json" "Parse error."
run_case "$BIN_JSON" "invalid-unicode-digit" "$ROOT_DIR/samples/sample-invalid-unicode-digit.json" "Parse error."
run_case "$BIN_JSON" "invalid-unexpected-char" "$ROOT_DIR/samples/sample-invalid-unexpected-char.json" "Parse error."
run_case "$BIN_JSON" "invalid-array-missing-comma" "$ROOT_DIR/samples/sample-invalid-array-missing-comma.json" "Parse error."
run_case "$BIN_JSON" "invalid-unclosed-array" "$ROOT_DIR/samples/sample-invalid-unclosed-array.json" "Parse error."
run_case "$BIN_JSON" "invalid-unclosed-object" "$ROOT_DIR/samples/sample-invalid-unclosed-object.json" "Parse error."
run_case "$BIN_JSON" "valid-escapes" "$ROOT_DIR/samples/sample-valid-escapes.json" "Parsed successfully"
run_case "$BIN_YAML" "yaml-valid" "$ROOT_DIR/samples/sample-valid.yaml" "Parsed successfully"
run_case "$BIN_YAML" "yaml-valid-seq" "$ROOT_DIR/samples/sample-valid-yaml-seq.yaml" "Parsed successfully"
run_case "$BIN_YAML" "yaml-valid-block" "$ROOT_DIR/samples/sample-valid-yaml-block.yaml" "Parsed successfully"
run_case "$BIN_YAML" "yaml-valid-flow" "$ROOT_DIR/samples/sample-valid-yaml-flow.yaml" "Parsed successfully"
run_case "$BIN_YAML" "yaml-invalid-tab-indent" "$ROOT_DIR/samples/sample-invalid-yaml-tab-indent.yaml" "Parse error."
run_case "$BIN_YAML" "yaml-invalid-anchor" "$ROOT_DIR/samples/sample-invalid-yaml-anchor.yaml" "Parse error."
run_case "$BIN_YAML" "yaml-invalid-flow" "$ROOT_DIR/samples/sample-invalid-yaml-flow.yaml" "Parse error."
run_case_stdin "$BIN_AUTO" "auto-json" "$ROOT_DIR/samples/sample-valid.json" "Parsed successfully (JSON)"
run_case_stdin "$BIN_AUTO" "auto-yaml" "$ROOT_DIR/samples/sample-valid.yaml" "Parsed successfully (YAML)"

printf "%-42s" "fuzz-invalid"
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

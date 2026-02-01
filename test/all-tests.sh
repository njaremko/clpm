#!/bin/sh
set -eu

ROOT="$(CDPATH= cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

SBCL_BIN="${SBCL:-sbcl}"

run_test() {
  test_file="$1"
  echo "==> $test_file"
  "$SBCL_BIN" --script "$test_file"
}

find test -maxdepth 1 -name '*-test.lisp' -print | sort | while read -r f; do
  run_test "$f"
done

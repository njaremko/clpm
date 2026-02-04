#!/bin/sh
set -eu

ROOT="$(CDPATH= cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

SBCL_BIN="${SBCL:-sbcl}"

run_test() {
  test_file="$1"
  echo "==> $test_file"
  tmp="${TMPDIR:-/tmp}/clpm-test.$(date +%s).$$.$(basename "$test_file" .lisp)"
  rm -rf "$tmp"
  mkdir -p "$tmp/home" "$tmp/cache" "$tmp/config"
  HOME="$tmp/home" \
  XDG_CACHE_HOME="$tmp/cache" \
  XDG_CONFIG_HOME="$tmp/config" \
  "$SBCL_BIN" --script "$test_file"
  rm -rf "$tmp"
}

find test -maxdepth 1 -name '*-test.lisp' -print | sort | while read -r f; do
  run_test "$f"
done

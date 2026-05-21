#!/bin/sh
set -eu

ROOT="$(CDPATH= cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

SBCL_BIN="${SBCL:-sbcl}"
JOBS="${CLPM_TEST_JOBS:-}"
if [ -z "$JOBS" ]; then
  JOBS="$(getconf _NPROCESSORS_ONLN 2>/dev/null || true)"
fi
if [ -z "$JOBS" ]; then
  JOBS="$(sysctl -n hw.ncpu 2>/dev/null || true)"
fi
case "$JOBS" in
  ''|0|*[!0-9]*) JOBS=1 ;;
esac
if [ "$JOBS" -gt 16 ]; then
  JOBS=16
fi

RUN_ROOT="${TMPDIR:-/tmp}/clpm-tests.$(date +%s).$$"
TEST_LIST="$RUN_ROOT/tests.list"
RESULT_DIR="$RUN_ROOT/results"
mkdir -p "$RESULT_DIR"

cleanup() {
  rm -rf "$RUN_ROOT"
}
trap cleanup EXIT
trap 'cleanup; exit 130' INT
trap 'cleanup; exit 143' TERM

find test -maxdepth 1 -name '*-test.lisp' -print | sort > "$TEST_LIST"

test_count="$(wc -l < "$TEST_LIST" | tr -d ' ')"
if [ "$test_count" -eq 0 ]; then
  echo "No tests found."
  exit 1
fi

echo "Running $test_count tests with $JOBS worker(s)..."

set +e
xargs -n 1 -P "$JOBS" sh -c '
  sbcl_bin="$1"
  result_dir="$2"
  test_file="$3"
  name="$(basename "$test_file" .lisp)"
  tmp="${TMPDIR:-/tmp}/clpm-test.$(date +%s).$$.$name"
  log="$result_dir/$name.log"
  status="$result_dir/$name.status"

  rm -rf "$tmp"
  mkdir -p "$tmp/home" "$tmp/cache" "$tmp/config"

  start="$(date +%s)"
  {
    echo "==> $test_file"
    HOME="$tmp/home" \
    XDG_CACHE_HOME="$tmp/cache" \
    XDG_CONFIG_HOME="$tmp/config" \
    "$sbcl_bin" --script "$test_file"
  } > "$log" 2>&1
  code="$?"
  end="$(date +%s)"

  rm -rf "$tmp"
  printf "%s\t%s\t%s\n" "$code" "$((end - start))" "$test_file" > "$status"
  exit "$code"
' sh "$SBCL_BIN" "$RESULT_DIR" < "$TEST_LIST"
xargs_status="$?"
set -e

failures=0
while IFS= read -r test_file; do
  name="$(basename "$test_file" .lisp)"
  status="$RESULT_DIR/$name.status"
  if [ ! -f "$status" ]; then
    failures=$((failures + 1))
    printf "not ok ?s %s (missing result)\n" "$test_file"
    continue
  fi

  IFS='	' read -r code seconds recorded_test < "$status"
  if [ "$code" -eq 0 ]; then
    printf "ok %ss %s\n" "$seconds" "$recorded_test"
  else
    failures=$((failures + 1))
    printf "not ok %ss %s\n" "$seconds" "$recorded_test"
  fi
done < "$TEST_LIST"

if [ "$failures" -ne 0 ] || [ "$xargs_status" -ne 0 ]; then
  echo
  echo "Failure logs:"
  while IFS= read -r test_file; do
    name="$(basename "$test_file" .lisp)"
    status="$RESULT_DIR/$name.status"
    log="$RESULT_DIR/$name.log"
    if [ -f "$status" ]; then
      IFS='	' read -r code _seconds _recorded_test < "$status"
    else
      code=1
    fi
    if [ "$code" -ne 0 ] && [ -f "$log" ]; then
      echo "----- $test_file -----"
      cat "$log"
    fi
  done < "$TEST_LIST"
  exit 1
fi

echo
echo "All tests PASSED!"

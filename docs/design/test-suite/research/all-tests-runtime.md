# All Tests Runtime

## Problem
`sh test/all-tests.sh` is user-visible as extremely slow. The script currently runs every `test/*-test.lisp` file in sequence, starting a fresh SBCL process and fresh isolated home/cache/config directories for each file.

## Fixed Means
The full `sh test/all-tests.sh` suite finishes substantially faster without reducing isolation, skipping tests, or hiding failures. The final gate is `sh test/all-tests.sh` passing.

## Current Status
Fixed and verifying.

## Hypotheses
- [confirmed] Sequential per-file SBCL startup dominates runtime. The serial harness completed 95 tests in 3:36.88, while the same tests passed in 57.262s with 4 workers and 29.875s with the default 16-worker cap.
- [partly confirmed] Some REPL tests are naturally longer, with `repl-watch-test.lisp` taking 12-14s under parallel runs, but this is no longer the suite wall-clock bottleneck.

## Evidence Log
- 2026-05-21: `sh test/all-tests.sh` passed serially in 3:36.88 (`194.19s user`, `20.49s system`, `98% cpu`).
- 2026-05-21: `CLPM_TEST_JOBS=4 sh test/all-tests.sh` passed in 57.262s (`201.02s user`, `23.04s system`, `391% cpu`).
- 2026-05-21: `sh test/all-tests.sh` passed with the default 16-worker cap in 29.875s (`246.89s user`, `32.60s system`, `935% cpu`).
- 2026-05-21: Earlier run of `sh test/all-tests.sh` failed once in `test/build-parallel-test.lisp`, but the isolated test and later full-suite runs passed. Treat as observed transient until it reproduces again.
- 2026-05-21: Created this note before changing the test harness. Current repro command: `sh test/all-tests.sh`.

## Harness Improvements
`test/all-tests.sh` now runs test files in parallel worker processes. Each test still gets a fresh `HOME`, `XDG_CACHE_HOME`, and `XDG_CONFIG_HOME`; output is captured per test and emitted in full for failures.

## Things Tried
- A single isolated `sbcl --script test/build-parallel-test.lisp` did not reproduce the one early full-suite failure.

## Commits / SHAs
Base working copy: `2e889d5fe604dd76039fa8c51299e98fa6e56006` / `zrnvulysqvxmutlyqzluptnzqqkwlmtl`.

## Next Steps
1. Re-run `sh test/all-tests.sh` after the worker-cap/trap cleanup.
2. If `test/build-parallel-test.lisp` flakes again, instrument `build-release` to preserve the child build log outside the temp tree.

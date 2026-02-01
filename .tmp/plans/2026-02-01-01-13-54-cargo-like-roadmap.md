# CLPM Cargo/uv-Style Roadmap (Phase-Gated, Test-Driven)

## 1) Problem outline

### User request
Design a fully specified, phase-gated implementation plan (≥20 phases) to evolve this repo into a Cargo/uv-like Common Lisp package manager with:
- Excellent developer UX (new projects, add/remove deps, run/test/package/clean, etc.)
- Secure dependency fetching (integrity + registry authenticity)
- Support for local dependencies
- Robust dependency resolution (good errors, deterministic lockfiles)

### Acceptance criteria (observable “done” conditions)
AC1. `clpm --help` lists all implemented commands and global options; `clpm help <cmd>` exists and prints command-specific help.
AC2. `clpm new myproj --bin` creates a runnable project scaffold with `clpm.project`, `myproj.asd`, `src/`, `test/`, and a default `:run` entrypoint; `clpm run` works immediately.
AC3. `clpm new mylib --lib` creates a library scaffold with `clpm.project`, `mylib.asd`, `src/`, and a test scaffold; `clpm test` runs.
AC4. `clpm add alexandria` adds a dependency to `clpm.project` and writes/updates `clpm.lock` deterministically; repeated runs are idempotent.
AC5. `clpm add alexandria@^1.4.0` and `clpm add alexandria@=1.4.0` work; lockfile reflects constraints and selections.
AC6. `clpm add --path ../local-lib` adds a local dependency; `clpm.lock` records `:path` and `:tree-sha256`; `clpm install --offline` succeeds if store contains required sources/builds.
AC7. `clpm add --git <url> --ref <ref>` adds a git dependency; `clpm.lock` records the resolved commit; fetch checks out that exact commit deterministically.
AC8. `clpm resolve` implements real PubGrub-style backtracking; on conflicts it prints a deterministic, human-readable conflict explanation.
AC9. Registry snapshot signatures are verified with Ed25519 by default; invalid signatures fail resolution/fetch with a clear error; `--insecure` bypasses verification but prints a warning.
AC10. Release metadata signatures are verified (snapshot authenticates the set of releases; each `release.sxp` is authenticated); lockfile records what was verified (key id + signature hash).
AC11. Archive extraction rejects path traversal and absolute paths for tarballs/zips; malicious archives do not write outside the extraction directory.
AC12. `clpm test` builds `:test-depends`, activates the project env, then runs tests via `asdf:test-system` for configured test systems; non-zero exit on failure.
AC13. `clpm package` produces a runnable executable in `dist/` based on `clpm.lock` and `:package` settings in `clpm.project` (entry function + output path); output is reproducible given the same lock and toolchain.
AC14. `clpm clean` removes per-project activation/build outputs; `clpm gc` reclaims unreferenced store entries by scanning real GC roots (known lockfiles + activation envs).
AC15. `--jobs N` enables parallel fetch/build (bounded concurrency), with stable progress output and deterministic results.
AC16. Global config exists at `~/.config/clpm/config.sxp` (or platform equivalent) for default registries/settings; `clpm registry add/list/update` manages it.

### Non-goals (explicitly out of scope for this plan)
- Supporting non-SBCL implementations (the repo is SBCL-first).
- Sandboxing Lisp compilation/execution (OS sandbox integration can be a later project).
- Hosting a public registry service (this plan targets the client and on-disk formats).

### Constraints
- Repo policy: never use `git` for VCS operations; use `jj` if VCS commands are needed (this plan does not require VCS commands).
- Keep bootstrap story: CLPM should still build from SBCL + ASDF without Quicklisp.
- Keep file formats data-only S-expressions (`src/io/sexp.lisp` validation rules apply).
- Indentation/style: 2 spaces, follow existing file layout.
- Tests must be deterministic and fast; failures must exit non-zero.

## 2) State of the world (as-of 2026-02-01)

### What exists now (observed in repo)
- CLI skeleton: `src/main.lisp`, `src/commands.lisp` with commands: `init`, `resolve`, `fetch`, `build`, `install`, `update`, `repl`, `run`, `gc`.
- Project/lockfile parsing and canonical writing: `src/project.lisp`, `src/io/sexp.lisp`.
- Registry model: git-based registries with signed snapshot + release metadata (`src/registry/git.lisp`).
- Content-addressed store: sources/artifacts/builds (`src/store.lisp`).
- Basic solver: greedy “choose highest” without real backtracking; minimal conflict info (`src/solver/pubgrub.lisp`).
- Crypto: SHA-256 implemented (`src/crypto/sha256.lisp`); Ed25519 verification is a stub (always fails) (`src/crypto/ed25519.lisp`).
- Tests: a single script test `test/basic-test.lisp`.

### Concrete gaps/bugs to close (observed)
- Ed25519 verification is non-functional (`src/crypto/ed25519.lisp` returns NIL in all verifier backends).
- Directory traversal uses `*.*` globbing in tree hashing and store copy, which can skip files (dotfiles/extensionless) (`src/crypto/sha256.lisp`, `src/store.lisp`).
- `clpm.platform:run-program` doesn’t accept `:timeout`, but build code passes it (`src/build/driver.lisp`, `src/platform.lisp`).
- `(:git ...)` / `(:path ...)` constraints parse to `pinned-source`, but solver ignores them (`src/solver/constraint.lisp`, `src/solver/pubgrub.lisp`).
- Lockfile generation does not populate `:tree-sha256`, so offline mode, store integrity checks, and build caching are unreliable (`src/solver/pubgrub.lisp`, `src/fetch.lisp`).
- `gc-store` is a placeholder (does not scan real roots).
- `--jobs` is parsed but build orchestration ignores it (sequential build).
- `cmd-run` ignores args and `(:lisp ...)` scripts are not executed meaningfully.
- `cmd-update` ignores requested systems.
- Project writer drops many fields (dev/test depends, scripts, build options, etc.) (`src/project.lisp`).

### What we don’t know (and how this plan removes ambiguity)
- Exact registry key/signature file encoding: this plan standardizes them (hex public keys, base64/hex signatures) and updates loader + docs accordingly.
- Exact expectations for “package/run/test” semantics: this plan defines explicit `clpm.project` keys for run/test/package and implements them with deterministic behavior and tests.

### Surfaces impacted (by design)
- CLI: `src/main.lisp`, `src/commands.lisp`, `src/packages.lisp`.
- Formats: `src/project.lisp`, `src/io/sexp.lisp`, `README.md`.
- Security: `src/crypto/*`, `src/registry/git.lisp`, `src/fetch.lisp`.
- Resolver: `src/solver/*`.
- Store/build: `src/store.lisp`, `src/build/*`, `src/platform.lisp`.
- Tests: `test/*`.

### Prior art inside the repo to reuse
- Canonical S-expression writer: `clpm.io.sexp:write-canonical-sexp`.
- Content-addressed store layout + hashing: `clpm.store`, `clpm.crypto.sha256`.
- Existing lockfile + project structs in `src/project.lisp` (extend rather than replace).

## 3) Justification: phases, steps, and ordering

### Why these phases exist
- Early phases fix correctness/security bugs that would invalidate later work (timeouts, tree hashing, lockfile tree hashes, signature verification).
- Mid phases establish stable formats and config surfaces so later UX commands (`new`, `add`, `test`, `package`) have deterministic targets.
- Later phases add Cargo/uv UX and performance (parallelism, gc roots) once core semantics are correct and test-covered.

### Why the ordering is optimal
- “Correctness first”: make the store, hashes, lockfiles, and signature verification trustworthy before adding UX layers that depend on them.
- “Testability first”: refactor CLI entrypoint to be testable, then add script tests per feature; each phase gates on the same deterministic test runner.
- “Surface-area control”: each phase touches a small, known set of files and has an explicit verification gate; no broad refactors without tests.

### Why this plan is necessary (no redundant phases)
- Each phase either (a) unblocks later work by fixing a known bug, or (b) introduces a user-facing command/format needed for Cargo/uv parity, with tests.

### Why this plan is sufficient
- The phases cover: secure registry verification, secure fetch/extract, deterministic lockfiles with sources pinned, full-feature dependency solving, and the core dev commands (new/add/run/test/package/clean/gc) with parallelism and config.

### Why the proposed solution is correct (invariants + validation)
Key invariants enforced by design + tests:
- I1. Lockfile determinism: same inputs produce byte-identical `clpm.lock`.
- I2. Integrity: every downloaded artifact is SHA-256 verified; every source tree used for builds is identified by `tree-sha256`.
- I3. Authenticity: registries are accepted only when snapshot + release metadata signatures verify, unless explicitly bypassed with `--insecure`.
- I4. Isolation of side effects: mutations are limited to `.clpm/` (project) and CLPM’s XDG dirs; file formats remain data-only.
- I5. Reproducibility: build cache IDs are deterministic over `(tree-sha256, sbcl-version, platform, asdf-version, compile policy, features-hash)`.

## Phase 1: Add deterministic test runner

### Goal
- Make “run all tests” a single stable command for gating every phase.

### Files to read into context
- `test/basic-test.lisp`
- `README.md`

### Steps

#### Step 1.1: Add `test/all-tests.sh`
- [ ] Read the phase context files listed above
- [ ] Create `test/all-tests.sh` that runs `sbcl --script` for each `test/*-test.lisp` file in lexicographic order
- [ ] Ensure the script uses `set -eu` and prints which test file is running
- [ ] Update/add tests for CLPM (not applicable; this is test harness only)
- [ ] Sanity-check locally: run `sh test/all-tests.sh` and confirm it runs `test/basic-test.lisp`

#### Step 1.2: Document test command
- [ ] Read the phase context files listed above
- [ ] Update `README.md` to add a “Testing” section: `sh test/all-tests.sh`
- [ ] Sanity-check locally: re-run `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- `test/all-tests.sh` exists and is executable via `sh test/all-tests.sh`
- `README.md` documents the test command

## Phase 2: Make CLI testable (no `sb-ext:exit` in core logic)

### Goal
- Enable testing CLI behavior without terminating the SBCL process.

### Files to read into context
- `src/main.lisp`
- `src/commands.lisp`
- `src/packages.lisp`
- `test/basic-test.lisp`

### Steps

#### Step 2.1: Split “run CLI” from “exit process”
- [ ] Read the phase context files listed above
- [ ] Refactor `clpm:main` into:
  - `clpm:run-cli` (new): takes args list, returns integer exit code, never calls `sb-ext:exit`
  - `clpm:main` (kept): calls `run-cli`, then `sb-ext:exit :code <result>`
- [ ] Update `src/packages.lisp` exports to include `clpm:run-cli`
- [ ] Update `test/basic-test.lisp` to call `clpm:run-cli` for a trivial command (`--version`) and assert exit code 0
- [ ] Sanity-check locally: `sbcl --script test/basic-test.lisp`

#### Step 2.2: Add a CLI-focused test script
- [ ] Read the phase context files listed above
- [ ] Create `test/cli-test.lisp` with assertions for:
  - `run-cli '("--help")` returns 0
  - `run-cli '("unknown-command")` returns 1
- [ ] Ensure `test/cli-test.lisp` exits non-zero on failure
- [ ] Sanity-check locally: `sbcl --script test/cli-test.lisp`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- CLI behavior is testable via `clpm:run-cli`
- `test/cli-test.lisp` exists and passes

## Phase 3: Fix `clpm.platform:run-program` timeout bug

### Goal
- Support `:timeout` and pass-through keywords so build orchestration doesn’t crash.

### Files to read into context
- `src/platform.lisp`
- `src/build/driver.lisp`
- `test/cli-test.lisp`

### Steps

#### Step 3.1: Add `:timeout` and `&allow-other-keys` support
- [ ] Read the phase context files listed above
- [ ] Define timeout units: `:timeout` is in seconds (integer or real), passed through to `uiop:run-program`
- [ ] Update `clpm.platform:run-program` lambda list to accept `&key timeout` and `&allow-other-keys`
- [ ] Pass `:timeout timeout` through to `uiop:run-program` when non-nil
- [ ] Add a test in `test/cli-test.lisp` that calls `clpm.platform:run-program` with `:timeout 1` on a fast command (`("sh" "-c" "exit 0")`) and asserts it returns exit code 0 (this test’s purpose is “no unknown keyword crash”)
- [ ] Sanity-check locally: run `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- `clpm.platform:run-program` accepts `:timeout` and does not error on unknown keywords
- A test asserts timeout handling

## Phase 4: Fix filesystem traversal (hashing + store copies include all files)

### Goal
- Ensure tree hashing and directory copying see dotfiles and extensionless files deterministically.

### Files to read into context
- `src/crypto/sha256.lisp`
- `src/store.lisp`
- `src/io/sexp.lisp`

### Steps

#### Step 4.1: Introduce a deterministic directory walker
- [ ] Read the phase context files listed above
- [ ] Add new file `src/io/fs.lisp` with:
  - `clpm.io.fs:list-directory-entries` (returns names excluding `.` and `..`, includes dotfiles)
  - `clpm.io.fs:walk-files` (returns `(rel-path . absolute-path)` for all regular files under a root, sorted by `rel-path`)
- [ ] Update `clpm.asd` to include `src/io/fs.lisp` before `src/crypto/sha256.lisp` and `src/store.lisp`
- [ ] Update `src/crypto/sha256.lisp` `sha256-tree` to use `walk-files` instead of `directory "*.*"`
- [ ] Update `src/store.lisp` `copy-directory-tree` to use `walk-files` and recreate directories explicitly
- [ ] Add `test/fs-test.lisp` that creates a temp tree with:
  - `file` (no extension)
  - `.hidden`
  - `dir/.inner`
  and asserts both hashing and copying include these files (hash changes when content changes; copied tree has the files)
- [ ] Sanity-check locally: `sbcl --script test/fs-test.lisp`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- Tree hashing and store copy do not rely on `*.*` globbing
- Tests cover dotfiles and extensionless files

## Phase 5: Make lockfiles record `tree-sha256` for all resolved sources

### Goal
- Populate `locked-release-tree-sha256` so offline mode and build caching are trustworthy.

### Files to read into context
- `src/solver/pubgrub.lisp`
- `src/fetch.lisp`
- `src/store.lisp`
- `src/project.lisp`

### Steps

#### Step 5.1: Compute tree hash during fetch/store and plumb into lockfile
- [ ] Read the phase context files listed above
- [ ] Update `clpm.fetch:fetch-artifact` to return `(values source-path tree-sha256)` for `:tarball` and `:git` sources
- [ ] Update `clpm.fetch:fetch-lockfile-deps` to:
  - when a locked release lacks `tree-sha256`, fetch/store the source, compute tree hash, and update the in-memory lockfile struct
  - write the updated lockfile back to disk atomically:
    - write to `<lock>.tmp` in the same directory
    - `rename-file` over the original `clpm.lock` (single-filesystem atomic rename)
- [ ] Update `cmd-fetch` to call the new behavior and log when it backfills `tree-sha256`
- [ ] Add `test/lockfile-tree-test.lisp`:
  - Create a minimal project + synthetic lockfile entry pointing at a local `:path` source
  - Run the fetch code path and assert the written lockfile now includes `:tree-sha256`
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- `clpm.lock` entries include `:tree-sha256` after `clpm fetch` / `clpm install`
- A regression test enforces this

## Phase 6: Honor `(:path ...)` dependencies end-to-end

### Goal
- Local deps work in manifest, resolution, lockfile, fetch, and build.

### Files to read into context
- `src/project.lisp`
- `src/solver/constraint.lisp`
- `src/solver/pubgrub.lisp`
- `src/fetch.lisp`

### Steps

#### Step 6.1: Define explicit semantics for pinned sources
- [ ] Read the phase context files listed above
- [ ] Update `clpm.solver.constraint:parse-constraint` so:
  - `(:path "<path>")` produces a constraint with `pinned-source` containing an absolute normalized path
- [ ] Update solver candidate selection so that when a constraint has `pinned-source`:
  - it bypasses registry lookup and creates a synthetic “release-ref” of the form `local:<system-id>@<tree-sha256>`
  - it records locked source kind `:path`, `:path` (original/normalized), and `:tree-sha256`
- [ ] Update fetch so `:path` locked sources are validated by hashing and stored in the store (no network)
- [ ] Add `test/path-dep-test.lisp` creating two local projects where one depends on the other via `(:path "../dep")`, then:
  - `run-cli '("install")` succeeds
  - `run-cli '("repl")` finds the dependency system (validate by loading it via ASDF in a non-interactive SBCL invocation)
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- `(:path ...)` deps are correctly resolved and locked
- Lockfile includes `:path` + `:tree-sha256` and builds use the pinned source

## Phase 7: Honor `(:git ...)` dependencies end-to-end

### Goal
- Git deps work in manifest and lockfile with commit pinning.

### Files to read into context
- `src/solver/constraint.lisp`
- `src/fetch.lisp`
- `src/registry/git.lisp`
- `src/project.lisp`

### Steps

#### Step 7.1: Resolve git refs to commits deterministically
- [ ] Read the phase context files listed above
- [ ] Standardize manifest format to `(:git :url "<url>" :ref "<ref>")`
- [ ] Implement `clpm.fetch:resolve-git-ref` with this exact algorithm:
  - If `<ref>` matches regex `^[0-9a-f]{40}$`, return it unchanged
  - Else run `git ls-remote --refs <url> <ref> <ref>^{}`
  - Parse stdout lines as `<sha>\t<refname>`
  - If any `<refname>` ends with `refs/tags/<ref>^{}`, pick its `<sha>`
  - Else pick the `<sha>` from the first output line
  - If there are zero lines, signal `clpm-fetch-error` with message `git ref not found`
- [ ] During resolve/lock generation, replace `:ref` with the resolved `:commit` in the lockfile’s `locked-source`
- [ ] Add `test/git-dep-test.lisp` that:
  - Creates a local git repo fixture (using `git init`, commit a minimal `.asd` + source)
  - Adds it as a dependency via `file://` URL and a ref (`HEAD`)
  - Asserts `clpm.lock` records a 40-hex commit and installs successfully
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- Git deps are pinned by commit in `clpm.lock`
- Install is deterministic for git deps

## Phase 8: Implement SHA-512 (required for Ed25519)

### Goal
- Provide a pure-CL SHA-512 implementation with test vectors.

### Files to read into context
- `src/crypto/sha256.lisp`
- `src/packages.lisp`
- `clpm.asd`
- `test/basic-test.lisp`

### Steps

#### Step 8.1: Add `src/crypto/sha512.lisp`
- [ ] Read the phase context files listed above
- [ ] Create `src/crypto/sha512.lisp` implementing:
  - `clpm.crypto.sha512:sha512` (string/octets -> 64-byte array)
  - `clpm.crypto.sha512:sha512-file`
- [ ] Reuse `clpm.crypto.sha256:bytes-to-hex` for hex formatting (do not duplicate hex helpers)
- [ ] Update `src/packages.lisp` to add `clpm.crypto.sha512` package exports
- [ ] Update `clpm.asd` to include the new file
- [ ] Add `test/sha512-test.lisp` with exact test vectors:
  - SHA-512("") =
    `cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e`
  - SHA-512("abc") =
    `ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f`
- [ ] Sanity-check locally: `sbcl --script test/sha512-test.lisp`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- SHA-512 exists and is test-covered

## Phase 9: Standardize key/signature encodings for registries

### Goal
- Make registry keys/signatures parseable without external crypto tooling.

### Files to read into context
- `src/crypto/ed25519.lisp`
- `src/registry/git.lisp`
- `README.md`

### Steps

#### Step 9.1: Define and implement key/signature parsing rules
- [ ] Read the phase context files listed above
- [ ] Define `.pub` encoding: ASCII hex of 32 bytes (64 hex chars) + optional newline
- [ ] Define `.sig` encoding: ASCII base64 of 64 bytes OR ASCII hex of 64 bytes (128 hex chars)
- [ ] Update `clpm.crypto.ed25519:load-public-key` to load hex-encoded keys (and keep raw-32-byte binary support)
- [ ] Update signature reading to accept both base64 and hex forms (detect by charset/length)
- [ ] Update `README.md` “Registry Format” section to document these exact encodings
- [ ] Add `test/key-format-test.lisp` to assert that hex/base64 parsing produces expected byte arrays
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- Key/signature file formats are explicitly defined and tested

## Phase 10: Implement Ed25519 verification (pure CL) using RFC 8032 test vectors

### Goal
- Make `clpm.crypto.ed25519:verify-file-signature` return correct results for real Ed25519 signatures.

### Files to read into context
- `src/crypto/ed25519.lisp`
- `src/crypto/sha512.lisp`
- `src/packages.lisp`

### Steps

#### Step 10.1: Implement core math and point decoding
- [ ] Read the phase context files listed above
- [ ] Implement field arithmetic mod p = 2^255-19 using SBCL bignums:
  - `p = 2^255 - 19`
  - `f+`, `f-`, `f*` (all reduce mod p)
  - `f^` (modular exponentiation)
  - `finv(a) = f^(a, p-2)`
  - `sqrtm1 = 19681161376707505956807079304988542015446066515923890162744021073123829784752` (sqrt(-1) mod p)
  - `fsqrt(u/v)` implemented as:
    - `x = (u * finv(v))^((p+3)/8) mod p`
    - if `f*(x,x) != u*finv(v)`, set `x = f*(x, sqrtm1)`
    - if still not equal, signal invalid-point
- [ ] Implement Edwards25519 constants (as exact integers):
  - `d = 37095705934669439343138083508754565189542113879843219016388785533085940283555`
  - basepoint affine coords:
    - `Bx = 15112221349535400772501151409588531511454012693041857206046113283949847762202`
    - `By = 46316835694926478169428394003475163141307993866256225615783033603165251855960`
- [ ] Implement points in extended coordinates `(X, Y, Z, T)` with invariant `T = X*Y/Z`
- [ ] Implement extended point addition (exact formulas):
  - `A = (Y1-X1)*(Y2-X2)`
  - `B = (Y1+X1)*(Y2+X2)`
  - `C = 2*d*T1*T2`
  - `D = 2*Z1*Z2`
  - `E = B-A`
  - `F = D-C`
  - `G = D+C`
  - `H = B+A`
  - `X3 = E*F`, `Y3 = G*H`, `Z3 = F*G`, `T3 = E*H`
- [ ] Implement extended point doubling (exact formulas):
  - `A = X1^2`
  - `B = Y1^2`
  - `C = 2*Z1^2`
  - `D = -A`
  - `E = (X1+Y1)^2 - A - B`
  - `G = D + B`
  - `F = G - C`
  - `H = D - B`
  - `X3 = E*F`, `Y3 = G*H`, `Z3 = F*G`, `T3 = E*H`
- [ ] Implement scalar multiplication with a fixed algorithm: left-to-right double-and-add over 256 bits
- [ ] Implement point decoding from 32-byte compressed form:
  - read `y` from low 255 bits (little-endian)
  - sign bit is high bit of last byte (x’s parity)
  - compute `x` via sqrt as above and select sign to match
- [ ] Add `test/ed25519-point-test.lisp` with exact decode tests:
  - basepoint encoded key (hex): `5866666666666666666666666666666666666666666666666666666666666666`
  - RFC 8032 test vector 1 public key (hex): `d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a`

#### Step 10.2: Implement signature verification and vectors
- [ ] Read the phase context files listed above
- [ ] Implement RFC 8032 verification:
- [ ] Implement scalar order and reduction:
  - `L = 2^252 + 27742317777372353535851937790883648493`
  - `L (hex) = 1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed`
  - Parse scalars as little-endian integers; reject `S >= L`
- [ ] Compute `h = SHA-512(R || A || M) mod L` where R/A are the 32-byte encodings
- [ ] Verify `[S]B = R + [h]A` using extended points and scalar multiplication from Step 10.1
- [ ] Add `test/ed25519-verify-test.lisp` with exact RFC 8032 vectors:
  - Vector 1:
    - `A` (public key) = `d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a`
    - `M` (message) = empty
    - `sig` =
      `e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b`
  - Vector 2:
    - `A` (public key) = `3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c`
    - `M` (message hex) = `72`
    - `sig` =
      `92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00`
- [ ] Add negative tests: altered signature fails; non-canonical S fails
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- Ed25519 verification works and is vector-tested
- No external `openssl/signify` dependency for correctness

## Phase 11: Enforce registry snapshot verification by default

### Goal
- `clpm resolve/fetch/update` reject registries with invalid snapshot signatures unless `--insecure`.

### Files to read into context
- `src/registry/git.lisp`
- `src/commands.lisp`
- `src/main.lisp`
- `src/errors.lisp`

### Steps

#### Step 11.1: Wire `--insecure` through registry loading and errors
- [ ] Read the phase context files listed above
- [ ] Update `clpm.registry:load-registry-snapshot` to:
  - If registry has `trust-key` and `clpm.commands:*insecure*` is false: verify signature and signal `clpm-signature-error` on failure
  - If `--insecure`: skip verification but emit a conspicuous warning via `log-info`/`log-error`
- [ ] Add `test/registry-sig-test.lisp`:
  - Create a minimal fake registry directory structure in a temp dir
  - Write a snapshot and an intentionally invalid signature
  - Assert resolve fails without `--insecure` and succeeds with `--insecure` (for the “succeeds” case, allow empty provides)
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- Snapshot verification is enforced and test-covered

## Phase 12: Verify release metadata signatures

### Goal
- Authenticate `registry/packages/*/*/release.sxp` via `release.sig` and record verification in lockfile.

### Files to read into context
- `src/registry/git.lisp`
- `src/solver/pubgrub.lisp`
- `src/project.lisp`

### Steps

#### Step 12.1: Load and verify `release.sig` alongside `release.sxp`
- [ ] Read the phase context files listed above
- [ ] Update `clpm.registry:get-release-metadata` to:
  - Require `release.sig` when registry is trusted
  - Verify `release.sxp` against `release.sig` with the same trust key lookup rules as snapshots
  - Signal `clpm-signature-error` on failure
- [ ] Extend lockfile registry entries to include the trust key id used and a hash of the verified snapshot signature bytes
- [ ] Add `test/release-sig-test.lisp` with a fake registry where `release.sig` is invalid and assert failure unless `--insecure`
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- Release metadata signatures are enforced and reflected in the lockfile

## Phase 13: Secure archive extraction (prevent path traversal)

### Goal
- Reject malicious tar/zip archives that attempt to write outside the extraction directory.

### Files to read into context
- `src/fetch.lisp`
- `src/platform.lisp`
- `src/errors.lisp`

### Steps

#### Step 13.1: Validate tar contents before extraction
- [ ] Read the phase context files listed above
- [ ] Before calling `tar -x*`, run `tar -t*` to list entries and reject any entry that:
  - starts with `/` (absolute)
  - contains `..` path segments
  - contains `:` as a Windows drive prefix
- [ ] Add a similar validator for zip using `unzip -Z1` when available; otherwise reject zip extraction with a clear error on non-Windows
- [ ] Add `test/extract-safety-test.lisp` that constructs a tarball with an entry like `../pwned` (using `tar`), then asserts extraction fails
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- Path traversal is blocked and test-covered

## Phase 14: Make artifact storage streaming (avoid loading whole tarballs into RAM)

### Goal
- Handle large downloads robustly by hashing/copying in a streaming way.

### Files to read into context
- `src/store.lisp`
- `src/crypto/sha256.lisp`
- `src/fetch.lisp`

### Steps

#### Step 14.1: Add streaming SHA-256 and streaming copy
- [ ] Read the phase context files listed above
- [ ] Add `clpm.crypto.sha256:sha256-stream` that updates a ctx from a binary stream in chunks
- [ ] Update `clpm.store:store-artifact` to:
  - compute sha256 by streaming the file
  - copy file to destination without reading into a single octet vector
- [ ] Add `test/store-streaming-test.lisp` that writes a 16 MiB temp file, stores it, and asserts hash matches expected
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- Store operations do not require O(file-size) memory

## Phase 15: Make `clpm.project` read/write round-trip complete

### Goal
- `write-project-file` preserves all fields that `parse-manifest` reads (depends/dev/test registries, scripts, sbcl, build).

### Files to read into context
- `src/project.lisp`
- `src/io/sexp.lisp`
- `test/basic-test.lisp`

### Steps

#### Step 15.1: Extend serialization to include all fields
- [ ] Read the phase context files listed above
- [ ] Update `write-project-file` to serialize:
  - `:depends`, `:dev-depends`, `:test-depends` (including `:source`, `:optional`, `:features` when present)
  - `:registries`, `:sbcl`, `:build`, `:scripts`
- [ ] Add `test/project-roundtrip-test.lisp`:
  - Write a project with all fields populated, read it back, write again, and assert canonical output is byte-identical
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- `clpm.project` round-trips with no data loss

## Phase 16: Add global config file support

### Goal
- Load defaults and registries from `~/.config/clpm/config.sxp` (or platform equivalent) and merge with per-project config.

### Files to read into context
- `src/platform.lisp`
- `src/commands.lisp`
- `src/io/sexp.lisp`
- `src/project.lisp`

### Steps

#### Step 16.1: Implement config format and loader
- [ ] Read the phase context files listed above
- [ ] Add `src/config.lisp` implementing:
  - `clpm.config:read-config` from `(clpm.platform:config-dir)/config.sxp`
  - config schema `(:config :format 1 :registries (...) :defaults (...))`
  - merge rule: project registries append after global registries; project build defaults override global defaults
- [ ] Update `clpm.asd` and `src/packages.lisp` accordingly
- [ ] Add `test/config-test.lisp` that creates a temp config dir via `CLPM_HOME` override and asserts registries merge correctly

#### Step 16.2: Add registry management commands
- [ ] Read the phase context files listed above
- [ ] Add commands:
  - `clpm registry list`
  - `clpm registry add --name <n> --url <u> --trust <ed25519:keyid>`
  - `clpm registry update [name...]`
- [ ] Add `test/registry-cmd-test.lisp` validating config file edits are canonical and idempotent
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- Global config exists, is parsed safely, and is managed by CLI commands

## Phase 17: Replace greedy solver with real PubGrub (core algorithm)

### Goal
- Implement backtracking resolution that is correct and deterministic for non-trivial graphs.

### Files to read into context
- `src/solver/pubgrub.lisp`
- `src/solver/constraint.lisp`
- `src/registry/git.lisp`
- `src/errors.lisp`

### Steps

#### Step 17.1: Introduce PubGrub data structures and propagation
- [ ] Read the phase context files listed above
- [ ] Replace current `solver-loop` with:
  - decision levels
  - incompatibility clauses
  - unit propagation
  - backjumping to previous decision level on conflict
- [ ] Keep deterministic tie-breaking: prefer lockfile selection if compatible; otherwise highest version
- [ ] Add `test/solver-backtrack-test.lisp` with a synthetic registry index where greedy fails but PubGrub succeeds

#### Step 17.2: Add deterministic lockfile preferences
- [ ] Read the phase context files listed above
- [ ] Implement “locked preference” as a soft preference (not a hard pin) unless explicitly constrained
- [ ] Add a test asserting that removing a constraint lets the solver keep previous selections when still valid
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- Solver backtracks correctly and is covered by tests

## Phase 18: PubGrub conflict explanations

### Goal
- Print actionable, stable conflict explanations for failed resolves.

### Files to read into context
- `src/solver/pubgrub.lisp`
- `src/errors.lisp`
- `src/commands.lisp`

### Steps

#### Step 18.1: Implement explanation extraction
- [ ] Read the phase context files listed above
- [ ] Store enough provenance in incompatibilities to explain:
  - which dependency edge introduced a constraint
  - which candidate versions were considered and rejected
- [ ] Implement `clpm.solver:conflict-explanation` returning a structured explanation object
- [ ] Update `cmd-resolve` to print explanations deterministically
- [ ] Add `test/solver-explain-test.lisp` that asserts on key substrings and structure (not on incidental formatting)
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- Conflicts produce stable, useful explanations and are test-covered

## Phase 19: Add `clpm new` (Cargo-like scaffolding)

### Goal
- Create new projects with one command and immediate “it runs” ergonomics.

### Files to read into context
- `src/commands.lisp`
- `src/project.lisp`
- `README.md`

### Steps

#### Step 19.1: Implement project templates
- [ ] Read the phase context files listed above
- [ ] Add `clpm new <name> --bin|--lib [--dir <path>]`:
  - Create directory `<dir>/<name>/`
  - Create `<name>.asd` with system name `<name>`
  - Create `src/<name>.lisp` with package + minimal exports
  - Create `test/<name>-test.lisp` skeleton
  - Create `clpm.project` with `:name`, `:version`, `:systems`, `:scripts`, and `:run`/`:test` metadata (defined in Phase 21/22)
- [ ] Add `test/new-command-test.lisp` that runs `run-cli` against a temp dir and asserts files exist with expected contents
- [ ] Update `README.md` quick start to prefer `clpm new`
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- `clpm new` exists and is test-covered

## Phase 20: Add `clpm add` / `clpm remove` (uv/cargo-like dependency editing)

### Goal
- Edit `clpm.project` programmatically and deterministically; auto-resolve and write lockfile.

### Files to read into context
- `src/commands.lisp`
- `src/project.lisp`
- `src/solver/constraint.lisp`
- `src/solver/pubgrub.lisp`

### Steps

#### Step 20.1: Define dep-spec grammar and implement parsing
- [ ] Read the phase context files listed above
- [ ] Define `clpm add` inputs:
  - `clpm add <system>`
  - `clpm add <system>@^<semver>`
  - `clpm add <system>@=<exact>`
  - `clpm add <system> --path <path>`
  - `clpm add <system> --git <url> --ref <ref>`
  - `--dev` and `--test` flags choose dependency section
- [ ] Define exact semantics for `clpm add <system>` (no explicit constraint):
  - Build registry index from configured registries
  - Select the highest available version `v_max` for `<system>`
  - Write `:constraint (:semver (format "^~A" v_max))` into `clpm.project`
- [ ] Implement parser in `src/commands.lisp` that maps CLI inputs to dependency structs using these exact constraint forms:
  - `@^...` → `(:semver "^...")`
  - `@=...` → `(:exact "...")`
  - `--path <p>` → `(:path "<p>")`
  - `--git <u> --ref <r>` → `(:git :url "<u>" :ref "<r>")`
- [ ] Implement deterministic insertion ordering: keep deps sorted by system name
- [ ] Add `test/add-remove-test.lisp` covering add/remove across depends/dev/test sections, plus idempotency

#### Step 20.2: Auto-resolve after edits
- [ ] Read the phase context files listed above
- [ ] After modifying `clpm.project`, run the same code path as `clpm install` (resolve/fetch/build optional via flags):
  - Default: resolve only and write lockfile
  - `--install`: resolve + fetch + build + activate
- [ ] Add tests asserting `--install` produces `.clpm/asdf-config.lisp`
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- `clpm add/remove` exist, are deterministic, and are test-covered

## Phase 21: Define and implement `clpm run` / `clpm exec`

### Goal
- Provide Cargo-like “run in project env” ergonomics with argument passing.

### Files to read into context
- `src/commands.lisp`
- `src/build/driver.lisp`
- `src/project.lisp`
- `src/platform.lisp`

### Steps

#### Step 21.1: Add `:run` metadata to `clpm.project`
- [ ] Read the phase context files listed above
- [ ] Extend manifest schema with:
  - `:run (:system "<system>" :function "<package>::<fn>")`
- [ ] Update project parser/writer and canonical writer test(s) accordingly

#### Step 21.2: Implement `clpm run [--] [args...]` and `clpm exec -- <cmd...>`
- [ ] Read the phase context files listed above
- [ ] `clpm run`:
  - Ensure project is activated (`.clpm/asdf-config.lisp` exists)
  - Spawn `sbcl --noinform --non-interactive --load .clpm/asdf-config.lisp --eval '(asdf:load-system "<system>")' --eval '(<fn> <args>)'`
  - Pass args as a list of strings to the function
- [ ] `clpm exec -- <cmd...>`:
  - Ensure activated
  - If `<cmd...>` starts with `sbcl`, run `sbcl` with `--load .clpm/asdf-config.lisp` injected immediately after `sbcl` (unless the user already provided a `--load .clpm/asdf-config.lisp`)
  - Otherwise, run `<cmd...>` unchanged but with env var `CLPM_PROJECT_ROOT=<project-root>`
- [ ] Add `test/run-exec-test.lisp` verifying args flow into the entry function and exit code is propagated
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- `clpm run` and `clpm exec` work and are test-covered

## Phase 22: Implement `clpm test`

### Goal
- Provide a first-class test command like Cargo/uv.

### Files to read into context
- `src/commands.lisp`
- `src/project.lisp`
- `src/build/driver.lisp`

### Steps

#### Step 22.1: Add `:test` metadata and implement command
- [ ] Read the phase context files listed above
- [ ] Extend manifest schema with:
  - `:test (:systems ("<test-system>" ...))`
- [ ] Implement `clpm test`:
  - Ensure installed/activated (or run `install` if missing lock/config)
  - Start `sbcl` non-interactive with `.clpm/asdf-config.lisp`
  - For each test system, run `(asdf:test-system "...")`
  - Exit non-zero if any fail
- [ ] Add `test/test-command-test.lisp` that generates a project with a passing and a failing test system and asserts exit codes
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- `clpm test` exists and is test-covered

## Phase 23: Implement `clpm package` (reproducible executable build)

### Goal
- Produce a runnable executable in `dist/` based on lockfile + manifest `:package` settings.

### Files to read into context
- `src/commands.lisp`
- `src/build/driver.lisp`
- `src/project.lisp`
- `clpm-bootstrap.lisp`

### Steps

#### Step 23.1: Define `:package` metadata
- [ ] Read the phase context files listed above
- [ ] Extend manifest schema with:
  - `:package (:output "dist/<name>" :system "<system>" :function "<package>::<fn>")`
- [ ] Update parser/writer + roundtrip tests

#### Step 23.2: Build the executable deterministically
- [ ] Read the phase context files listed above
- [ ] Implement `clpm package`:
  - Ensure installed/activated
  - Spawn SBCL non-interactive with `.clpm/asdf-config.lisp`
  - Load the package system
  - Call `sb-ext:save-lisp-and-die` to `:output` with toplevel that calls the entry function
  - Record build metadata alongside the binary at `dist/<name>.meta.sxp` including:
    - `:lock-sha256` = hex SHA-256 of the exact `clpm.lock` file bytes used
    - `:sbcl-version` = `(clpm.platform:sbcl-version)`
    - `:platform` = `(clpm.platform:platform-triple)`
- [ ] Add `test/package-command-test.lisp` that packages a tiny project and asserts the binary runs and prints expected output
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- `clpm package` produces a runnable binary and metadata, test-covered

## Phase 24: Implement `clpm clean` and real `clpm gc` roots

### Goal
- Provide predictable cleanup and safe reclamation of unused store entries.

### Files to read into context
- `src/commands.lisp`
- `src/store.lisp`
- `src/build/driver.lisp`
- `src/platform.lisp`

### Steps

#### Step 24.1: Implement `clpm clean`
- [ ] Read the phase context files listed above
- [ ] Add `clpm clean` that removes:
  - `.clpm/` in the current project
  - project-local `dist/` (optional flag `--dist`)
- [ ] Add `test/clean-command-test.lisp` verifying it only deletes expected paths

#### Step 24.2: Implement real GC mark phase
- [ ] Read the phase context files listed above
- [ ] Populate the project index automatically:
  - Update `clpm.build:activate-project` to upsert the absolute `project-root` into `(merge-pathnames "projects.sxp" (clpm.platform:data-dir))` using canonical S-expression writing
- [ ] Define GC roots:
  - A canonical project index file: `(merge-pathnames "projects.sxp" (clpm.platform:data-dir))` with format `(:projects :format 1 :roots ("<abs-root>" ...))`
  - For each `<abs-root>` in the index:
    - if `<abs-root>/clpm.lock` exists, parse it as a root
    - if `<abs-root>/.clpm/env.sexp` exists, parse it as a root
- [ ] Implement mark: parse each lockfile, mark `tree-sha256` and `artifact-sha256` and build IDs referenced
- [ ] Implement sweep: remove unmarked entries
- [ ] Add `test/gc-roots-test.lisp` that creates two lockfiles referencing different store entries and asserts GC keeps referenced and removes unreferenced
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- `clean` and `gc` behave deterministically and are test-covered

## Phase 25: Implement parallel fetch/build (`--jobs`)

### Goal
- Make `--jobs N` actually run bounded concurrency for fetch and build.

### Files to read into context
- `src/commands.lisp`
- `src/fetch.lisp`
- `src/build/orchestrator.lisp`
- `src/platform.lisp`

### Steps

#### Step 25.1: Parallel fetch
- [ ] Read the phase context files listed above
- [ ] Implement a worker pool using `sb-thread`:
  - A synchronized queue of fetch tasks
  - N worker threads, each running `fetch-artifact` and returning results
- [ ] Ensure deterministic output ordering: collect results and sort by system-id before returning
- [ ] Add `test/fetch-parallel-test.lisp` that uses multiple local `:path` dependencies and asserts results are deterministic under `--jobs 1` vs `--jobs 4`

#### Step 25.2: Parallel build
- [ ] Read the phase context files listed above
- [ ] Update `build-all` to schedule tasks whose dependencies are completed, up to `jobs` concurrent builds
- [ ] Ensure build cache writes are race-safe (use atomic directory creation and deterministic paths)
- [ ] Add `test/build-parallel-test.lisp` with multiple independent deps (local `:path` projects) and assert builds complete under `--jobs 4`
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- `--jobs` affects fetch/build and is test-covered

## Phase 26: Polish UX (help, formatting, diagnostics)

### Goal
- Reach a Cargo/uv-like “feels great” baseline: discoverability, stable outputs, and diagnostics.

### Files to read into context
- `src/main.lisp`
- `src/commands.lisp`
- `src/errors.lisp`
- `README.md`

### Steps

#### Step 26.1: Help, command grouping, and consistent exit codes
- [ ] Read the phase context files listed above
- [ ] Implement `clpm help <cmd>` and ensure every command has:
  - clear usage
  - flags documented
  - consistent exit codes (0 success, 1 user error, 2 resolve conflict, etc.)
- [ ] Add `test/help-output-test.lisp` asserting `--help` and `help <cmd>` contain expected command names

#### Step 26.2: Add `clpm doctor`
- [ ] Read the phase context files listed above
- [ ] Implement `clpm doctor` checks:
  - SBCL version requirement
  - ASDF available
  - downloader present
  - tar present
  - registries configured
- [ ] Add `test/doctor-test.lisp` that runs `doctor` in a minimal environment (use env overrides) and asserts deterministic messages
- [ ] Update `README.md` with “Getting started” flow: `clpm doctor`, `clpm registry add`, `clpm new`, `clpm add`, `clpm test`, `clpm package`
- [ ] Sanity-check locally: `sh test/all-tests.sh`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] If failures: fix before proceeding

### Exit criteria
- UX commands are discoverable and tested
- README matches the implemented workflow

## Proof: traceability matrix (acceptance criteria → phases/steps → validation)

| Acceptance criterion | Implemented in | Validated by |
|---|---|---|
| AC1 help coverage | Phase 26 | `test/help-output-test.lisp` |
| AC2 `new --bin` scaffold + run | Phase 19 + Phase 21 | `test/new-command-test.lisp`, `test/run-exec-test.lisp` |
| AC3 `new --lib` scaffold + test | Phase 19 + Phase 22 | `test/new-command-test.lisp`, `test/test-command-test.lisp` |
| AC4 `add` idempotent + lock | Phase 20 + Phase 15 | `test/add-remove-test.lisp`, `test/project-roundtrip-test.lisp` |
| AC5 semver/exact add | Phase 20 | `test/add-remove-test.lisp` |
| AC6 local path deps | Phase 6 + Phase 5 | `test/path-dep-test.lisp`, `test/lockfile-tree-test.lisp` |
| AC7 git deps pinned | Phase 7 | `test/git-dep-test.lisp` |
| AC8 real backtracking | Phase 17 + Phase 18 | `test/solver-backtrack-test.lisp`, `test/solver-explain-test.lisp` |
| AC9 snapshot ed25519 verify | Phase 8–11 | `test/ed25519-verify-test.lisp`, `test/registry-sig-test.lisp` |
| AC10 release sig verify | Phase 12 | `test/release-sig-test.lisp` |
| AC11 safe extraction | Phase 13 | `test/extract-safety-test.lisp` |
| AC12 `clpm test` | Phase 22 | `test/test-command-test.lisp` |
| AC13 `clpm package` | Phase 23 | `test/package-command-test.lisp` |
| AC14 clean/gc | Phase 24 | `test/clean-command-test.lisp`, `test/gc-roots-test.lisp` |
| AC15 parallel jobs | Phase 25 | `test/fetch-parallel-test.lisp`, `test/build-parallel-test.lisp` |
| AC16 global config/registry mgmt | Phase 16 | `test/config-test.lisp`, `test/registry-cmd-test.lisp` |

## Risks & mitigations

1) Risk: Ed25519 implementation bugs lead to false accepts/rejects.
- Mitigation: RFC 8032 vectors + negative tests (Phase 10) and registry integration tests (Phase 11/12).

2) Risk: Filesystem walking differs across platforms.
- Mitigation: Centralize traversal in `src/io/fs.lisp` with tests that include dotfiles and extensionless (Phase 4).

3) Risk: Parallel fetch/build introduces nondeterminism or races.
- Mitigation: deterministic ordering of outputs, atomic store writes, and parallel-vs-serial equivalence tests (Phase 25).

4) Risk: Secure extraction still allows edge cases (weird tar headers, unicode paths).
- Mitigation: strict path validators + reject on any suspicious entry; add more fixtures over time (Phase 13).

5) Risk: Real PubGrub implementation is complex.
- Mitigation: stepwise rollout: core backtracking first (Phase 17), then explanations (Phase 18), with synthetic unit tests.

## Out of scope (reiterated)
- Build sandboxing (network isolation, OS-level sandbox profiles).
- Multi-implementation Lisp support (CCL/ECL/CLISP).
- Remote binary caches and signing of build artifacts (future enhancement after lockfile + registry authenticity are solid).

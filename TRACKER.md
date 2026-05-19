# CLPM Issue Tracker

Open work items uncovered during a full read of the codebase. Each ticket cites the relevant files and lines so it can be picked up cold. Priorities are subjective: `P0` = correctness/integrity, `P1` = misleading/silently-broken behaviour, `P2` = missing feature surface, `P3` = polish.

Legend: `[ ]` open · `[~]` in progress · `[x]` done

## Progress log

- `2026-05-19` Started implementation pass. Baseline: 55 tests passing.
- `2026-05-19` **#013 landed.** Adds `clpm.platform:with-file-lock` combining a per-path SB-THREAD mutex (intra-process) with `lockf` on a sibling `.lock` file (inter-process). Consolidated duplicated `projects.sxp` helpers from `build/driver.lisp` into `store.lisp` and exported `upsert-project-index-root`/`remove-project-index-root`/`read-project-index-roots`. Added `config.lisp:update-config` for atomic read-modify-write. `test/concurrent-state-test.lisp` covers both layers (8 child SBCL procs + 6 threads).
- `2026-05-19` **#007 landed.** `native-requires` now flows: registry metadata → solver (`build-resolution` / `resolution-to-lockfile`) → `locked-release` struct field → lockfile serialization → `check-native-deps`. Rewrote orchestrator's `check-native-deps` to actually read `locked-release-native-requires` instead of the previous `(native-deps nil)` placeholder, with parsing for `(:kind "name")` and `(:kind . "name")` forms, dedup via hash table, and `clpm-missing-native-dep-error` raised on both unresolved deps and malformed entries. `test/native-deps-test.lisp` covers round-trip, empty, missing, and malformed cases. Full suite 57/57 green.
- `2026-05-19` **#012 landed.** `sha256-tree` now hashes a git-style mode token (`100644` / `100755` / `120000`) per file via `sb-posix:lstat`, so an `chmod +x` flips the digest. Symlinks now hash the link target string instead of the dereferenced contents, with `walk-files` switched to SBCL's `:resolve-symlinks nil` to preserve them through the walker. Bumped `compute-build-id` prefix `clpm-build-v1` → `clpm-build-v2` so stale cached builds don't collide with the new hash format. On non-Unix-SBCL platforms the executable bit is approximated by extension (`.bat/.cmd/.exe/.ps1/.sh`). `test/tree-mode-test.lisp` covers determinism, executable-bit flip, and symlink retargeting. Full suite 58/58 green.

## Lessons / decisions

- **POSIX advisory locks are per-process, not per-fd.** `lockf` (and `flock(2)` on most systems) tracked by `(pid, inode)`, so two threads inside one process see the lock as already held and don't serialize. Conclusion: lock helpers in this codebase combine a per-path Lisp mutex with the OS-level file lock. The first thread in a process acquires the kernel lock; subsequent threads queue on the mutex and find the kernel lock still held by their own process when they get their turn (no-op).
- **SBCL `sb-posix` does not export `flock` on Darwin.** Use `lockf` (POSIX-standard, exclusive-only). Always-exclusive locks are fine since CLPM's critical sections are short.
- **`SB-EXT` exports `wait-for`.** Test helpers should avoid the name or be defined in their own package, otherwise `defun wait-for ...` in CL-USER hits a package-lock error.
- **SBCL's `directory` follows symlinks by default and even deduplicates by truename.** A file and a symlink pointing at it collapse to a single entry. Pass `:resolve-symlinks nil` to surface symlinks distinctly — this is required if a tree hash is to encode link targets rather than dereferenced contents.

---

## Solver

### #001 — `[ ]` `P1` `solver` `naming` Rename or actually implement PubGrub

`src/solver/pubgrub.lisp` is documented as PubGrub and the README advertises "PubGrub-style conflict explanations", but the implementation is a depth-first backtracking solver with `snapshot-state` / `restore-state` (lines 184-205) and per-system reason strings (lines 93-99). None of the PubGrub primitives are present:

- No unit propagation
- No derivation graph
- No incompatibility set construction
- No conflict-driven clause learning
- No backjumping (decision-level is incremented but never used to jump)

Decide on a direction and follow through:

- **Option A — own the design.** Rename the file to `src/solver/backtrack.lisp`, update the README (`README.md:7`) to "deterministic backtracking solver with reason-chain explanations", and drop the dead struct fields tracked in #002 / #003.
- **Option B — implement real PubGrub.** Build incompatibilities from dependency clauses, add unit propagation in a fixpoint loop, derive new incompatibilities at conflicts, and backjump using decision levels. Existing reason chains can stay as a UI layer over the derivation graph.

**Acceptance criteria**

- File name and exported package match the actual algorithm.
- README and `clpm help` text accurately describe what the solver does.
- Either the unused state from #002 / #003 is gone, or it is now wired into a working PubGrub loop.

---

### #002 — `[ ]` `P2` `solver` `dead-code` Remove or use `solver-state-incompatibilities`

`solver-state` declares `(incompatibilities nil :type list)` at `src/solver/pubgrub.lisp:40` with the comment "learned conflict clauses". Nothing in the codebase ever writes to or reads this slot. If #001 lands as "Option A", delete the slot; if "Option B", populate it during conflict analysis.

**Acceptance criteria**

- `rg solver-state-incompatibilities src/` returns no results, or the slot is read and written in the conflict path with a test covering at least one learned-clause scenario.

---

### #003 — `[ ]` `P2` `solver` `dead-code` Remove or use `decision-stack` for backjumping

`solver-state-decision-stack` (`src/solver/pubgrub.lisp:42`) is pushed on every `decide` (line 244-245) and copied on snapshot/restore (lines 191, 204), but its contents are never consulted. The only state actually used for backtracking is the snapshot/restore alist.

Either:

- Delete the slot and the corresponding snapshot/restore fields, or
- Use it to implement targeted backjumping in the conflict path (jump to the most recent decision relevant to the conflict, instead of the alphabetically-prior depth-first frame).

**Acceptance criteria**

- The slot is either removed, or the conflict handler uses it to jump past irrelevant decisions, with a test that exhibits the speedup on a contrived diamond conflict.

---

### #004 — `[ ]` `P2` `solver` `correctness` Merge overlapping ranges in `constraint-union`

`src/solver/constraint.lisp:156-172` concatenates ranges without merging. The comment "Simplified: just append ranges (could be merged for optimization)" is honest about the limitation. The function is currently unused (see #005), but if it becomes used, an unmerged union of `>=1.2 <1.5` and `>=1.4 <2.0` will report two ranges where one suffices, and worse, callers that iterate ranges will see double-coverage.

**Acceptance criteria**

- `constraint-union` returns a canonical range list: sorted by lower bound, with all overlapping or adjacent ranges merged.
- Unit test in `test/` covers: disjoint ranges (no merge), overlapping ranges (merge), touching ranges with matching inclusivity (merge), touching ranges with mismatched inclusivity (no merge).

---

### #005 — `[ ]` `P3` `solver` `dead-code` Decide the fate of `constraint-union`

`clpm.solver.constraint:constraint-union` is exported (`src/packages.lisp:300`) and defined (`src/solver/constraint.lisp:156`) but called from nowhere in `src/` or `test/`. Either remove it (and the export) or land a caller — for example, when a system is declared by two registries with disjoint version ranges, the candidate set could be expressed as a union and reasoned about uniformly.

**Acceptance criteria**

- Either: the function and its export are deleted, and any docs referencing union-style constraints are updated.
- Or: at least one solver code path consumes a unioned constraint, with a test that exercises a 2-registry disjoint-range scenario.

Depends on #004 if kept.

---

### #006 — `[ ]` `P1` `cli` `update` Implement selective update in `cmd-update`

`src/commands.lisp:1407-1441` declares `(declare (ignore systems))  ; TODO: selective update` but the help text (`src/commands.lisp:3605-3609`) advertises `clpm update [system ...]`. A user running `clpm update alexandria` today silently gets a full re-resolve.

**Expected behaviour**

- `clpm update` (no args) → bump every system to the highest version that satisfies its constraint (current behaviour).
- `clpm update <sys> [<sys>...]` → only relax the lockfile preference for the named systems, hold every other system at its currently-locked version.

**Implementation sketch**

In the solver, when a system is listed in an `unlock-set`, skip the lockfile preference in `ordered-candidate-refs` (`src/solver/pubgrub.lisp:225-238`); otherwise behave as today. Pass the unlock set in via `solve`'s keyword args.

**Acceptance criteria**

- Selective update changes only the targeted systems unless the resolver is forced to move others to satisfy constraints.
- Error if a named system isn't in the lockfile.
- New test in `test/` covers: targeted update, untargeted system is held, untargeted system is bumped because constraints forced it.

---

## Build & runtime

### #007 — `[x]` `P1` `build` `placeholder` Wire `check-native-deps` to real metadata

`src/build/orchestrator.lisp:279-295` iterates the lockfile but hardcodes `(native-deps nil)` with the comment "For now this is a placeholder". Meanwhile `release-metadata-native-requires` is parsed off disk (`src/registry/git.lisp:42, 371`) and exported (`src/packages.lisp:244`) but never reaches the build step.

**Expected behaviour**

- Each `locked-system` (or, more precisely, each release referenced in the lockfile) carries its native-requires through to `check-native-deps`.
- For every entry `(:pkg-config "libfoo")` / `(:brew "openssl")` / `(:apt "libssl-dev")`, the existing `check-native-dep` helper (`src/build/orchestrator.lisp:297-320`) is called, and failures raise `clpm-missing-native-dep-error` with install hints.

**Implementation notes**

- The lockfile struct doesn't carry native-requires today. Two options:
  1. Add a `:native-requires` field to `locked-release` and persist it during resolve.
  2. Re-load release metadata from registries in `check-native-deps` and look it up by `(name, version)`.
- Option 1 keeps the build deterministic offline; prefer it.

**Acceptance criteria**

- Lockfile round-trips native-requires.
- Build fails with a clear error and install hints when a declared native dep is missing.
- Existing `test/doctor-test.lisp` and a new orchestrator test cover both the present and missing cases.

---

### #008 — `[ ]` `P1` `manifest` `silent-noop` Honor `:optional` dependencies

`dependency-optional-p` is parsed (`src/project.lisp:106`), serialized (`src/project.lisp:388`), and exported (`src/packages.lisp:150`), but the solver, fetcher, and builder never consult it. A user writing `:optional t` gets no error and no effect — the dep is treated as required.

Pick semantics and implement them. Suggested:

- Optional deps don't add a root constraint by default.
- A new flag `--with <feature>` / `--with-optional <system>` opts them in.
- `clpm install` respects the lockfile if a previous run opted in.

**Acceptance criteria**

- Solver skips optional roots unless explicitly opted in.
- `clpm tree` and `clpm why` distinguish optional vs. required edges in output.
- Test covers opt-in and default-skip paths.

Related: #009.

---

### #009 — `[ ]` `P2` `manifest` `silent-noop` Honor `:features` on dependencies

`dependency-features` is parsed and serialized symmetrically with `:optional` (`src/project.lisp:107, 391`) but nothing consumes it. Decide whether features are a real concept in CLPM or remove the field:

- **Keep it:** define semantics — e.g., features map to ASDF system flavors or to optional sub-systems within a release, and feature selection propagates through the solver as additional dependency edges.
- **Drop it:** remove parsing, serialization, and the export, and document that CLPM has no feature model.

**Acceptance criteria**

- Either: features have a documented semantic, the solver propagates feature edges, and tests demonstrate transitive feature activation.
- Or: the field is gone from the manifest schema and the README.

Depends on #008 for shared opt-in plumbing if both stay.

---

### #010 — `[ ]` `P2` `config` `merge` Extend `merge-project-config` beyond registries and build

`src/config.lisp:97-109` merges only `:registries` and `:build`. The project struct supports `:lisp`, `:scripts`, `:test`, `:run`, `:package`, etc., but a user can't set defaults for any of these in `~/.config/clpm/config.sxp`.

The most useful overrides are probably `:lisp` (default implementation for projects that don't pin one) and `:build` (already covered). Audit the other slots: `:scripts`, `:test`, `:run`, `:package` are project-specific and probably should not be merged.

**Acceptance criteria**

- Global config can specify `:defaults (:lisp "ccl" :build (:debug nil :speed 3 :safety 1) …)` and projects without an explicit `:lisp` pick up the global default.
- Per-project values still override.
- `clpm doctor` reports the effective merged config.
- Test in `test/config-test.lisp` covers the new merge paths.

---

### #011 — `[ ]` `P2` `build` `package` Support non-SBCL implementations in `cmd-package`

`src/commands.lisp:2034-2037` errors out with "Packaging currently supports SBCL only" because the code uses `sb-ext:save-lisp-and-die`. CCL has `ccl:save-application` and ECL has `compile-file` + `c:build-program`. Implement at least CCL parity since `run`/`test`/`repl` already work cross-impl via `lisp-run-argv`.

**Acceptance criteria**

- `clpm package --lisp ccl` produces a runnable executable on macOS and Linux.
- `clpm package --lisp ecl` produces a runnable executable (or errors with a clear "not yet implemented" message if ECL is deferred).
- Wrapper script handling stays correct for each impl (CCL doesn't need `--end-runtime-options`).
- Existing SBCL package tests still pass; new tests cover at least CCL.

---

## Integrity

### #012 — `[x]` `P1` `integrity` `tree-hash` Capture file modes in `sha256-tree`

`src/crypto/sha256.lisp:235-258` hashes each file as `path \0 "644" \0 size \0 contents`. The mode string is hardcoded, so an executable bit flip on a script doesn't invalidate the cached tree. Any project that ships shell tools or pre/post-build hooks risks reusing a stale build.

**Expected behaviour**

- Read the actual file mode (octal) and include it in the hash.
- On Windows, where execute bits aren't meaningful, fall back to a stable per-extension policy (`.bat`/`.cmd`/`.ps1` → executable).

**Acceptance criteria**

- Toggling `chmod +x` on a file in a source tree changes its `sha256-tree` digest.
- All existing tree-hash tests still pass (regenerate fixture hashes once, document why).
- Cross-platform test on macOS and Linux exercises both states.

**Notes**

- This is a hash-format change; bumping the prefix string in `compute-build-id` (`src/store.lisp:178`) from `clpm-build-v1` to `clpm-build-v2` is appropriate so old builds don't collide.

---

### #013 — `[x]` `P1` `infra` `concurrency` Lock global state files

`src/build/driver.lisp:134-143` reads, mutates, and rewrites `~/.local/share/clpm/projects.sxp` without holding any lock. `src/config.lisp:76-88` does the same for the global config. Two concurrent `clpm install` invocations in different projects can race and lose entries.

**Expected behaviour**

- Both writers acquire an OS-level advisory lock (`flock` on POSIX, file locking on Windows) on a sibling `.lock` file before reading the current contents.
- Writers use the existing tmp-file-then-rename pattern (`src/fetch.lisp:558-565`) so a crash mid-write doesn't truncate the file.

**Acceptance criteria**

- Concurrent `clpm install` runs in two project directories both end up in `projects.sxp` (test: run two installs in parallel via `&` and `wait`, assert both roots present).
- `clpm registry add` and `clpm registry trust set` from concurrent invocations don't lose entries.

---

## CLI ergonomics

### #014 — `[ ]` `P2` `cli` `info` Honor `--all` in `cmd-info` text mode

`src/commands.lisp:837-974` parses `--all` (line 858) but consults `allp` only inside the JSON branch (line 920). Text-mode output ignores it entirely while the help text (`src/commands.lisp:3474-3481`) advertises "Include metadata for all candidates".

**Expected behaviour**

- When `--all` is set, the text-mode candidates section emits license, source kind, and URL/hash/commit for every candidate, not just the selected one.

**Acceptance criteria**

- `clpm info <sys> --all` (text mode) prints the same per-candidate detail the JSON branch produces.
- `clpm info <sys>` (no flag) is unchanged.
- Test extends `test/info-command-test.lisp` to cover the text-mode `--all` path.

---

### #015 — `[ ]` `P2` `cli` `workspace` Add `clpm workspace remove`

`src/commands.lisp:2231-2327` implements `init`, `add`, and `list`. There's no `remove`, so removing a workspace member requires hand-editing `clpm.workspace`.

**Expected behaviour**

- `clpm workspace remove <member>` removes the named member from `:members` and rewrites the workspace file canonically.
- Idempotent: removing a non-existent member errors with a clear message and a list of current members.
- Does not delete the member directory on disk.

**Acceptance criteria**

- Help output and `print-command-help :workspace` (`src/commands.lisp:3539-3546`) list the new subcommand.
- Test in `test/workspace-commands-test.lisp` covers add → remove → list round trip.

---

### #016 — `[ ]` `P2` `cli` `keys` Flesh out `clpm keys`

`src/commands.lisp:2664-2720` only implements `generate`. The help text (`src/commands.lisp:3520-3528`) and README don't mention any other subcommands, but several are obvious gaps:

- `clpm keys list [--keys-dir <dir>]` — list key IDs and fingerprints in a keys directory.
- `clpm keys import --pub <path>` — copy a trusted public key into `~/.config/clpm/keys/`.
- `clpm keys verify --pub <path> --file <path> --sig <path>` — verify a detached signature, mirroring the registry path but exposed to users.

**Acceptance criteria**

- Each subcommand is implemented, has per-subcommand help via `print-command-help`, and has at least one test.
- `generate` continues to work unchanged.

---

### #017 — `[ ]` `P3` `cli` `help` Per-subcommand help beyond `registry`

`print-command-help` (`src/commands.lisp:3401-3647`) only branches on `subcommand` for `:registry` (line 3550). Other commands with subcommands — `workspace`, `keys`, `scripts` — print only the umbrella usage when you run `clpm help workspace add`, etc.

**Acceptance criteria**

- `clpm help workspace add`, `clpm help keys generate`, `clpm help scripts run`, `clpm help registry trust set`, etc., all print a focused, command-specific page.
- Test extends `test/help-output-test.lisp` to cover at least one subcommand page per umbrella.

---

### #018 — `[ ]` `P3` `cli` `clean` Add store cleanup to `cmd-clean`

`src/commands.lisp:2145-2173` removes `.clpm/` and optionally `dist/`. There's no project-local way to evict that project's source / build store entries; users have to run `clpm gc`, which is a global operation.

Two options:

- Add `--store` that removes from the store only the entries reachable from this project's lockfile. Requires walking the lockfile to collect tree-sha256 / artifact-sha256 / build-id sets (similar to `gc-store`'s mark phase, `src/store.lisp:291-352`) and deleting them.
- Surface `--cache` as a convenient alias for `clpm gc` invoked from within a project.

Prefer the first; it's the more useful operation. The risk is that two projects sharing a release would lose the shared store entry, which is exactly what GC roots are supposed to prevent. So the semantics should be "untrack this project's roots from `projects.sxp` and then GC."

**Acceptance criteria**

- `clpm clean --store` removes only store entries no longer reachable from any other registered project.
- Existing `--dist` and default behaviour unchanged.
- Test in `test/clean-command-test.lisp` exercises a two-project sharing scenario.

---

## Output formats

### #019 — `[ ]` `P3` `sbom` `format` Add SBOM formats beyond CycloneDX-JSON

`src/commands.lisp:3211-3397` only accepts `--format cyclonedx-json`. Two useful additions:

- `cyclonedx-xml` — same content, XML serialization. The deterministic CycloneDX serializer is the only piece that needs adding; the data extraction is shared.
- `spdx-json` — SPDX 2.3 JSON. Different schema; needs a separate component-emitter.

**Acceptance criteria**

- `clpm sbom --format cyclonedx-xml` and `clpm sbom --format spdx-json` produce schema-valid output.
- Help text and README updated.
- Tests in `test/sbom-command-test.lisp` cover both new formats and assert byte-for-byte stability across runs.

---

## Fetch

### #020 — `[ ]` `P2` `fetch` `reliability` Retries and timeouts for HTTP fetch

`src/fetch.lisp:7-59` delegates to `curl`/`wget`/PowerShell with no retry, no backoff, no per-request timeout. A flaky network turns into an immediate failure.

**Expected behaviour**

- Default 3 attempts with exponential backoff (1s, 4s, 9s).
- Configurable via env var (`CLPM_FETCH_RETRIES`) and `--fetch-retries` CLI flag.
- Default per-request timeout (e.g. 60s) via curl's `--max-time` / wget's `--timeout`.
- `--offline` short-circuits as today.

**Acceptance criteria**

- Transient network failure in a test (using a flaky local server) succeeds within retry budget; permanent failure exhausts retries and reports the underlying error.
- No change to deterministic-fetch tests (network not invoked).

---

### #021 — `[ ]` `P3` `fetch` `archive` Support `.tar.xz` and `.tar.bz2`

`src/fetch.lisp:225-315` handles `.tar.gz`/`.tgz`/`.tar`/`.zip`. XZ and BZ2 are common for upstream tarballs.

**Acceptance criteria**

- Both formats extract correctly with anti-traversal validation matching the gz path.
- `tool-install-hints` (`src/platform.lisp:300`) advises the right system package when xz/bzip2 isn't available.
- Existing extract tests extended.

---

## Documentation

### #022 — `[ ]` `P2` `docs` Quicklisp constraint model is implicit

The README's constraint table (`README.md:111-118`) advertises semver/exact/git/path. It doesn't say that for Quicklisp-sourced systems, dependencies parsed from `systems.txt` get a `nil` constraint (`src/registry/quicklisp.lisp:407-411`) because Quicklisp doesn't publish version constraints. Users running into surprising resolutions (e.g. "why did CLPM pick this version of bordeaux-threads") need to know.

**Acceptance criteria**

- README has a "Quicklisp caveats" subsection that explains:
  - Versions are derived from `YYYYMMDD` in the QL prefix.
  - Dependencies inherit `nil` (any) constraints because QL doesn't publish them.
  - Mixing Quicklisp and git registries is supported but resolution falls back to "latest" for QL deps.
- `clpm help registry add` mentions the Quicklisp behaviour briefly.

---

### #023 — `[ ]` `P3` `docs` Document the actual solver

Tied to #001. The README claims "Deterministic dependency resolution with PubGrub-style conflict explanations" (`README.md:7`). After #001 lands, update the language to match reality, and document the solver's actual ordering rules (alphabetical system pick, highest-version-first candidate ordering with lockfile preference lifted to the front).

**Acceptance criteria**

- README accurately describes the algorithm and its determinism guarantees.
- A short "How resolution works" section explains the rules a user needs to know to reason about why a particular version was picked.

---

## Cross-cutting

### #024 — `[ ]` `P3` `coupling` `clpm.commands::log-info` is a private symbol leaking into `clpm.registry`

`src/registry/git.lisp:206-207` calls `clpm.commands::log-info` via the internal symbol path. That couples a lower-layer package to a higher-layer package and bypasses the export contract.

**Fix**

- Move `log-info` / `log-verbose` / `log-error` into a small shared `clpm.log` package (or `clpm.io.log`) that lower layers can use.
- Have `clpm.commands` re-export them or alias them for backward compatibility.

**Acceptance criteria**

- No `clpm.commands::` (double-colon) references in `src/` outside the `clpm.commands` package itself.
- All existing tests pass.

---

## Suggested ordering

If picking these up in a single sweep, a reasonable order is:

1. #013 (locking) and #007 (native deps) — both are correctness/integrity items hidden behind quiet bugs.
2. #012 (tree-hash modes) — same category, smaller blast radius.
3. #006 (selective update) — most-requested CLI gap with a TODO already in code.
4. #001 / #002 / #003 — decide and execute the solver direction in one pass.
5. #008 / #009 — manifest fields with no effect.
6. #014 / #015 / #016 / #017 — CLI completeness.
7. Remaining items in any order.

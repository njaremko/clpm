# CLPM Excellence Roadmap (Cargo/uv-like UX, trust, workspaces, publish, multi-lisp)

## 1) Problem outline

### User request
Write a fully specified, phase-gated implementation plan to address the current “rough edges” and missing product surface so CLPM feels excellent (Cargo/uv vibe):
- Reduce “SBCL-only” / “SBCL-centric” limitations
- Make versioning and registry semantics clear (especially Quicklisp’s date-based releases)
- Add missing product commands (search/info/tree/why, scripts, publish, workspace ergonomics)
- Improve trust/provenance/audit story beyond “it downloads stuff”
- Improve onboarding and maintainability so it survives HN-level skepticism

### Acceptance criteria (observable “done” conditions)
AC1. `clpm search <query>` exists and searches across configured registries (git + Quicklisp) with deterministic output ordering.
AC2. `clpm info <system>` exists and prints the selected release(s), available versions, source kind, and metadata fields when present.
AC3. `clpm tree [--package <pkg>]` prints a deterministic dependency tree for the selected project/workspace member.
AC4. `clpm why <system-id>` explains why a system is in the graph (reverse path(s) from root deps).
AC5. Quicklisp registry supports an explicit trust mode:
  - `:trust "tofu"` pins the first-seen `distinfo.txt` SHA-256 into global config, and rejects later changes unless explicitly refreshed.
  - `:trust "sha256:<hex>"` enforces a fixed distinfo SHA-256.
AC6. `clpm registry trust` subcommands exist to inspect/refresh pins (and do not require editing config files manually).
AC7. `clpm add` has an unambiguous constraint UX:
  - `clpm add <sys>` defaults to `:constraint nil` (meaning “any version”); lockfile pins a concrete version.
  - `clpm add <sys>@^<semver>` / `@=<ver>` keeps current behavior for semver-capable registries.
  - `clpm add --registry <name> <sys>` disambiguates when multiple registries provide `<sys>`.
AC8. Workspace support exists:
  - A `clpm.workspace` file defines members.
  - Commands accept `-p/--package <member>` to target a member from workspace root.
  - `clpm install/test/run/package/add/remove` work at workspace root with `-p`.
AC9. Script/task runner exists:
  - `clpm scripts list` lists named scripts from `clpm.project`.
  - `clpm scripts run <name> [-- <args...>]` runs scripts in the activated environment.
AC10. Multi-lisp runner exists (without porting CLPM itself yet):
  - `--lisp sbcl|ccl|ecl` is supported for `repl`, `run`, `test` (and used in build cache keys).
  - When an implementation is missing, CLPM fails with a clear “install <impl> or set --lisp sbcl” message.
AC11. Publishing to git registries is supported end-to-end:
  - `clpm keys generate --out <dir> --id <id>` generates an Ed25519 keypair (file formats specified below).
  - `clpm registry init --dir <path> --key-id <id>` creates a registry layout (`registry/snapshot.sxp`, `registry/keys/<id>.pub`, etc.).
  - `clpm publish --registry <path-or-url> [--project <dir>]` writes a new `registry/packages/<name>/<version>/release.sxp`, updates `snapshot.sxp`, and writes detached `.sig` files using the private key.
AC12. Provenance/audit command exists:
  - `clpm audit` prints a deterministic report covering: registries used (kind/url), trust mode, signature verification results, and flags unsafe sources (git/path) explicitly.
AC13. SBOM generation exists:
  - `clpm sbom --format cyclonedx-json` produces deterministic JSON for resolved dependencies with hashes and licenses when known.
AC14. Onboarding is “one page”:
  - README includes Quicklisp-first quickstart (`clpm registry add --quicklisp`, `clpm new`, `clpm add`, `clpm test/run/package`) and a workspace example.
  - `example/quicklisp-app/README.md` demonstrates the online flow.
AC15. All repo tests remain deterministic and pass; new features have dedicated test coverage.

### Non-goals (explicitly out of scope for this plan)
- Hosting a central public registry service (the plan targets the client + a git-backed registry format).
- OS-level sandboxing (bubblewrap/sandbox-exec) for builds (can be a later security milestone).
- A maintained CVE/vulnerability database integration (we’ll implement provenance/SBOM; not a vuln feed).
- Full portability of CLPM to run under every Common Lisp implementation (we’ll add multi-lisp *execution* first).

### Constraints
- Repo rule: do not use `git` for repo VCS operations; use `jj` for commits/changesets.
- Maintain bootstrap story: CLPM still builds from SBCL + ASDF without Quicklisp.
- File formats remain data-only S-expressions for manifests/lockfiles/config (use `src/io/sexp.lisp`).
- Safety: do not weaken verification defaults; any bypass must be explicit (`--insecure`, `--refresh-trust`).
- Every phase is gated; do not proceed if tests fail.

## 2) State of the world (as-of 2026-02-01)

### What exists now (confirmed in repo)
- Core commands exist and are tested: `new/init`, `add/remove`, `registry add/list/update`, `resolve/fetch/build/install/update`, `repl/run/exec/test/package/clean/gc` (`src/main.lisp`, `src/commands.lisp`, `test/*`).
- Git registry format with Ed25519 verification exists (`src/registry/git.lisp`, `src/crypto/ed25519.lisp`), including release metadata signature checks.
- Quicklisp dist registry support exists (`src/registry/quicklisp.lisp`), including:
  - Parsing `distinfo.txt`, `systems.txt`, `releases.txt`
  - Fetching tarballs
  - Verifying Quicklisp “content SHA-1” after extraction (`src/fetch.lisp`)
- A local, deterministic Quicklisp workflow test exists using a localhost HTTP server (`test/quicklisp-workflow-test.lisp`).
- A live Quicklisp smoke test exists (manual, guarded by `CLPM_LIVE_QUICKLISP=1`) (`test/manual/quicklisp-live-workflow.lisp`).
- An example Quicklisp-backed project exists (`example/quicklisp-app/*`) and is documented.
- Tests are already numerous and run via `sh test/all-tests.sh`.

### What’s still missing / rough (the “HN commenter list”)
- Discovery UX: no `search/info/tree/why` commands yet.
- Quicklisp authenticity is currently “TLS only”; no pinning/TOFU story.
- Constraint UX is confusing for Quicklisp (date-based, semver-like constraints aren’t meaningful).
- Workspace ergonomics are missing (multi-package repos and targeting a member).
- “Scripts/tasks” are present in schema but not first-class UX.
- Multi-lisp support is missing; CLPM orchestrates SBCL child processes for builds.
- No publish flow for creating a signed registry, generating keys, and pushing updates.
- No provenance/audit report and no SBOM export.

### Unknowns (must be explicitly handled)
- Which alternative lisps are available in contributor environments (CCL/ECL/CLISP). Plan includes “ask user to run” gates where needed.
- Whether users want CLPM’s built-in publish to perform git commits/pushes automatically. Plan will implement `--git-commit` and default to “write files only”.

### Surfaces likely touched
- CLI plumbing: `src/main.lisp`, `src/commands.lisp`, `src/packages.lisp`
- Manifest/config/lock schema: `src/project.lisp`, `src/config.lisp`, `src/io/sexp.lisp`
- Registry & trust: `src/registry/git.lisp`, `src/registry/quicklisp.lisp`, `src/fetch.lisp`
- Build/run orchestration: `src/build/driver.lisp`, `src/build/orchestrator.lisp`, `src/store.lisp`, `src/platform.lisp`
- Tests: new `test/*-test.lisp` files per feature
- Docs/examples: `README.md`, `example/*`

### Prior art to reuse in-repo
- Canonical SEXP IO: `clpm.io.sexp:*`
- Deterministic file walking: `clpm.io.fs:walk-files`
- Existing signing verification pipeline: `clpm.registry:verify-*-signature`, `clpm.crypto.ed25519:verify-signature`
- Existing “end-to-end workflow” test patterns: `test/example-workflow-test.lisp`, `test/quicklisp-workflow-test.lisp`

## 3) Justification: phases, steps, and ordering

### Why these phases exist
- We start with read-only UX commands (search/info/tree/why) because they improve usability without destabilizing core resolution/build behavior.
- We then harden Quicklisp authenticity with a TOFU/pinning mechanism because it addresses the biggest “security story” criticism that’s actually solvable client-side.
- Next we add workspace + scripts because they’re major productivity multipliers and unblock real “cargo-like” workflows for multi-package repos.
- Multi-lisp support follows once the project/env abstraction is solid; it touches build/run caches, so it must come after graph/workspace work.
- Publish + key management come late because they introduce new cryptographic and on-disk formats; we only do this once the registry machinery is stable and well-tested.
- Audit/SBOM comes after trust/publish/workspace so it can report meaningful provenance.
- Docs/examples are updated incrementally as features land so onboarding remains accurate throughout the work.

### Why the ordering is optimal
- Dependency graph UX (tree/why) relies on deterministic resolution output; it does not require altering the solver.
- Quicklisp TOFU relies on stable download + hashing logic already present; it’s a narrow change with high security value.
- Workspace support impacts project discovery and command routing; putting it before multi-lisp/publish avoids repeatedly refactoring those features.
- Publishing requires signing; adding key formats and signing logic earlier would be risky without strong integration tests.

### Why this plan is necessary (no redundant work)
- Each phase produces a user-visible capability or a security invariant with dedicated tests.
- Phases avoid broad refactors: changes are localized, and every new surface has a test gate.

### Why this plan is sufficient (completeness)
- The plan introduces discovery, trust, workspace, scripts, multi-lisp execution, publishing, auditing, and SBOM—covering the full “cargo/uv-like” product surface.
- Acceptance criteria map to specific phases/steps and tests (see Traceability Matrix).

### Why the proposed solution is correct (invariants + validation)
Key invariants, preserved/extended by the phases:
- I1 Determinism: lockfile & derived outputs (tree/why, sbom) are stable across runs given same inputs.
- I2 Integrity: when a hash exists in metadata, it is always verified (no silent skips).
- I3 Authenticity: git registries require Ed25519 verification by default; Quicklisp requires a pinned `distinfo.txt` SHA-256 when configured for trust.
- I4 Transparency: unsafe sources (`:git`, `:path`) are surfaced in `audit` output and SBOM.
- I5 Cache correctness: build cache keys include implementation identity (sbcl/ccl/ecl), version, platform, and compile options.

---

## Phase 1: Baseline UX spec + golden CLI tests

### Goal
- Lock down command surface and outputs for new commands via deterministic tests.

### Files to read into context
- `src/main.lisp`
- `src/commands.lisp`
- `test/help-output-test.lisp`
- `test/cli-test.lisp`

### Steps

#### Step 1.1: Add help stubs for upcoming commands
- [ ] Read the phase context files listed above
- [ ] Add placeholder command entries (help text only) for: `search`, `info`, `tree`, `why`, `scripts`, `audit`, `sbom`, `keys`, `publish`, `workspace`
- [ ] Ensure `clpm --help` and `clpm help <cmd>` include the placeholders with usage lines
- [ ] Add/extend `test/help-output-test.lisp` to assert the new command names appear in help output
- [ ] Sanity-check: `sbcl --script test/help-output-test.lisp`

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed (paste/record the output)
- [ ] Commit: `jj commit -m "feat: reserve command surface for discovery/workspace/publish"`

### Exit criteria
- Help output includes the new command placeholders
- Tests lock in the surface so later phases can’t silently regress UX

## Phase 2: `clpm search` (registry discovery)

### Goal
- Implement `clpm search <query>` across configured registries with deterministic ordering.

### Files to read into context
- `src/main.lisp`
- `src/commands.lisp`
- `src/config.lisp`
- `src/registry/git.lisp`
- `src/registry/quicklisp.lisp`
- `test/registry-cmd-test.lisp`

### Steps

#### Step 2.1: Implement `cmd-search`
- [ ] Read the phase context files listed above
- [ ] Add `clpm.commands:cmd-search` with signature `(defun cmd-search (&rest args) ...)`
- [ ] CLI: `clpm search <query> [--limit N] [--json]`
- [ ] Search algorithm:
  - [ ] Load merged registries (global + project)
  - [ ] For each registry, collect candidate system IDs matching case-insensitive substring of `<query>`
  - [ ] Sort results by `(system-id, registry-name)` lexicographically
  - [ ] Apply `--limit` after sorting
- [ ] Output:
  - [ ] Default: one line per result: `<system-id>\t<registry-name>\t<package@version>`
  - [ ] `--json`: stable JSON array of objects with keys `system`, `registry`, `release`
- [ ] Add parsing/dispatch in `src/main.lisp` for `:search`

#### Step 2.2: Add deterministic test
- [ ] Read the phase context files listed above
- [ ] Add `test/search-command-test.lisp`:
  - [ ] Create a temp git registry (using `jj git init`) with a small snapshot + package metadata
  - [ ] Add a temp Quicklisp dist served by localhost (reuse pattern from `test/quicklisp-workflow-test.lisp`)
  - [ ] Configure both registries in temp `CLPM_HOME`
  - [ ] Assert `clpm:run-cli (list "search" "ql-")` returns 0 and output contains expected sorted lines

### Gate: tests must pass
- [ ] Run tests for `clpm`: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m "feat: add registry search command"`

### Exit criteria
- `clpm search` works across git + Quicklisp registries
- `test/search-command-test.lisp` passes deterministically

## Phase 3: `clpm info` (package/system details)

### Goal
- Implement `clpm info <system-id>` with clear “what will be selected” and metadata.

### Files to read into context
- `src/main.lisp`
- `src/commands.lisp`
- `src/registry/git.lisp`
- `src/registry/quicklisp.lisp`
- `src/solver/version.lisp`
- `test/search-command-test.lisp`

### Steps

#### Step 3.1: Implement `cmd-info`
- [ ] Read the phase context files listed above
- [ ] CLI: `clpm info <system-id> [--json] [--all]`
- [ ] Behavior:
  - [ ] Resolve candidate releases per registry via `clpm.registry:find-system-candidates`
  - [ ] Sort versions using existing version comparator (treat Quicklisp versions as already normalized strings)
  - [ ] Print “selected by default” candidate (highest version across registries) and list available candidates
  - [ ] Include source kind and URL/commit/path where available
  - [ ] Include metadata fields for git registries (`license`, `homepage`, `description`) when present
- [ ] Add dispatch in `src/main.lisp`

#### Step 3.2: Add test
- [ ] Read the phase context files listed above
- [ ] Add `test/info-command-test.lisp`:
  - [ ] Build a temp registry with two versions of same system
  - [ ] Assert `clpm info` prints deterministic selection + sorted version list

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m "feat: add info command for systems"`

### Exit criteria
- `clpm info` exists, is deterministic, and is covered by tests

## Phase 4: Dependency graph introspection (`tree` + `why`)

### Goal
- Add deterministic graph printing commands based on the lockfile graph.

### Files to read into context
- `src/main.lisp`
- `src/commands.lisp`
- `src/project.lisp`
- `test/solver-explain-test.lisp`
- `test/example-workflow-test.lisp`

### Steps

#### Step 4.1: Implement `cmd-tree`
- [ ] Read the phase context files listed above
- [ ] CLI: `clpm tree [--package <member>] [--depth N]`
- [ ] Load `clpm.lock` and print a tree rooted at project deps (depends + test-depends when `--depth` includes)
- [ ] Determinism rules:
  - [ ] Children sorted lexicographically by system-id
  - [ ] Stable indentation (2 spaces per depth)

#### Step 4.2: Implement `cmd-why`
- [ ] Read the phase context files listed above
- [ ] CLI: `clpm why <system-id> [--package <member>]`
- [ ] Print all distinct shortest paths from root deps to `<system-id>` (bounded to 10 paths, stable ordering)

#### Step 4.3: Add tests
- [ ] Add `test/tree-why-test.lisp` using a small synthetic lockfile written to temp project root
- [ ] Assert exact output lines (or stable regex matches) for both commands

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m \"feat: add tree/why dependency introspection\"`

### Exit criteria
- `clpm tree` and `clpm why` exist and are tested/deterministic

## Phase 5: Quicklisp trust model (TOFU + pinned distinfo SHA-256)

### Goal
- Add a meaningful authenticity story for Quicklisp: pin `distinfo.txt` digest and reject unexpected changes.

### Files to read into context
- `src/config.lisp`
- `src/project.lisp`
- `src/registry/quicklisp.lisp`
- `src/fetch.lisp`
- `test/quicklisp-workflow-test.lisp`
- `test/config-test.lisp`

### Steps

#### Step 5.1: Extend registry trust parsing for Quicklisp
- [ ] Read the phase context files listed above
- [ ] Define trust schemes for Quicklisp registries (stored in existing `:trust` string):
  - [ ] `nil` / missing: current behavior (TLS-only; allowed, but `audit` will warn later)
  - [ ] `"tofu"`: on first successful update, record `sha256:<distinfo-sha256>` into config
  - [ ] `"sha256:<hex>"`: enforce exact distinfo SHA-256
- [ ] Implement helper in `src/registry/quicklisp.lisp`:
  - [ ] `%distinfo-sha256-hex` over downloaded `distinfo.txt`
  - [ ] Enforce trust rule before fetching indexes (systems/releases)
  - [ ] For `"tofu"`: write back updated trust to global config via `clpm.config:write-config`

#### Step 5.2: Add `--refresh-trust` option for `clpm registry update`
- [ ] Read the phase context files listed above
- [ ] CLI: `clpm registry update <name> [--refresh-trust]`
- [ ] When `--refresh-trust` is set and registry kind is Quicklisp:
  - [ ] Allow distinfo SHA change and update pinned value in config

#### Step 5.3: Add deterministic test
- [ ] Add `test/quicklisp-trust-tofu-test.lisp`:
  - [ ] Serve a fake distinfo via local server (reuse `start-file-http-server`)
  - [ ] First update with `tofu` writes pinned sha256 into config
  - [ ] Mutate served distinfo content; update fails without `--refresh-trust`; succeeds with it

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m \"feat: add Quicklisp distinfo TOFU/pinning\"`

### Exit criteria
- Quicklisp trust is configurable and enforced with tests

## Phase 6: `clpm registry trust` subcommands

### Goal
- Avoid “edit config by hand”: provide CLI to inspect and refresh trust pins/keys.

### Files to read into context
- `src/commands.lisp`
- `src/config.lisp`
- `src/project.lisp`
- `test/registry-cmd-test.lisp`

### Steps

#### Step 6.1: Add subcommand group
- [ ] Read the phase context files listed above
- [ ] Implement:
  - [ ] `clpm registry trust list` (prints registry name, kind, trust string)
  - [ ] `clpm registry trust set <name> <trust>` (updates config trust string)
  - [ ] `clpm registry trust refresh <name>` (equivalent to update with `--refresh-trust` for Quicklisp; error for git)

#### Step 6.2: Add tests
- [ ] Add `test/registry-trust-cmd-test.lisp` covering list/set/refresh behavior under temp `CLPM_HOME`

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m \"feat: add registry trust management commands\"`

### Exit criteria
- Trust pins/keys can be managed entirely via CLI

## Phase 7: Make `clpm add` constraints unambiguous (default to `nil`)

### Goal
- Reduce “version story” confusion (especially for Quicklisp) and make defaults predictable.

### Files to read into context
- `src/commands.lisp`
- `src/project.lisp`
- `src/solver/pubgrub.lisp`
- `test/add-remove-test.lisp`
- `test/quicklisp-workflow-test.lisp`

### Steps

#### Step 7.1: Change default add behavior
- [ ] Read the phase context files listed above
- [ ] Update `cmd-add` so `clpm add <sys>` writes `:constraint nil` (instead of auto `^<max>`)
- [ ] Preserve existing behavior when user specifies `@...`
- [ ] Add flags:
  - [ ] `--any` (explicitly set constraint to nil)
  - [ ] `--caret` (force `^<max>` behavior for semver-capable registries)
- [ ] Update help text and usage strings

#### Step 7.2: Add `--registry <name>` disambiguation
- [ ] Read the phase context files listed above
- [ ] When multiple registries provide `<sys>`, require `--registry` and error with a deterministic list of candidates
- [ ] Store chosen registry name in lockfile’s resolved registry list as already done; no schema change

#### Step 7.3: Update tests
- [ ] Update/add tests in `test/add-remove-test.lisp` to assert new default constraint is nil
- [ ] Add a new test `test/add-disambiguate-registry-test.lisp` with two registries providing same system-id

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m \"feat: make clpm add default constraint nil and support --registry\"`

### Exit criteria
- `clpm add` is predictable and Quicklisp-friendly by default

## Phase 8: Workspace file format + discovery

### Goal
- Introduce `clpm.workspace` and workspace-aware root discovery.

### Files to read into context
- `src/project.lisp`
- `src/commands.lisp`
- `src/io/sexp.lisp`
- `test/new-command-test.lisp`

### Steps

#### Step 8.1: Define `clpm.workspace` schema and parser
- [ ] Read the phase context files listed above
- [ ] Add `src/workspace.lisp` with:
  - [ ] `(:workspace :format 1 :members ("path1" "path2" ...))`
  - [ ] `read-workspace-file`, `write-workspace-file`
  - [ ] `find-workspace-root` and `find-project-or-workspace-root`
- [ ] Update `clpm.asd` to include `src/workspace.lisp` in load order
- [ ] Update `src/packages.lisp` exports for `clpm.workspace:*` as needed

#### Step 8.2: Tests
- [ ] Add `test/workspace-discovery-test.lisp`:
  - [ ] Create temp workspace with two member projects
  - [ ] Assert root detection and member resolution are correct and deterministic

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m \"feat: add clpm.workspace format and discovery\"`

### Exit criteria
- Workspace file exists as first-class concept with tests

## Phase 9: Workspace targeting (`-p/--package`) across core commands

### Goal
- Make multi-project repos ergonomic like Cargo workspaces.

### Files to read into context
- `src/main.lisp`
- `src/commands.lisp`
- `src/workspace.lisp`
- `test/example-workflow-test.lisp`

### Steps

#### Step 9.1: Add global `-p/--package` option
- [ ] Read the phase context files listed above
- [ ] Extend `src/main.lisp` parsing to accept `-p/--package <member>` and pass into command layer via a new special (e.g., `clpm.commands:*package*`)
- [ ] Update `clpm.project:find-project-root` (or add a new helper) to:
  - [ ] If in workspace root and `--package` provided, operate in that member dir
  - [ ] If in workspace root and no `--package`, error with a list of members for commands that require a project

#### Step 9.2: Extend commands to honor `--package`
- [ ] Update `cmd-install`, `cmd-test`, `cmd-run`, `cmd-package`, `cmd-add`, `cmd-remove`, `cmd-resolve`, `cmd-fetch`, `cmd-build`, `cmd-clean`
- [ ] Ensure project-local files go into the member directory (`member/.clpm`, `member/clpm.lock`, `member/dist/`)

#### Step 9.3: Tests
- [ ] Add `test/workspace-commands-test.lisp` exercising `clpm new`, `clpm add`, `clpm test` from workspace root with `-p`

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m \"feat: workspace member targeting with -p/--package\"`

### Exit criteria
- Workspace root can drive member workflows without `cd`-ing

## Phase 10: Workspace scaffolding (`clpm new --workspace` and `--member`)

### Goal
- Make creating multi-package repos a first-class, guided flow.

### Files to read into context
- `src/commands.lisp`
- `src/workspace.lisp`
- `test/new-command-test.lisp`

### Steps

#### Step 10.1: Add scaffold commands/options
- [ ] Read the phase context files listed above
- [ ] Add:
  - [ ] `clpm new <name> --workspace --dir <path>` creates:
    - [ ] `<path>/<name>/clpm.workspace` with empty members list
    - [ ] `<path>/<name>/README.md` with usage
  - [ ] `clpm new <name> --bin|--lib --member-of <workspace-dir>` creates project inside workspace and appends member path to `clpm.workspace`

#### Step 10.2: Tests
- [ ] Add `test/workspace-new-test.lisp` verifying:
  - [ ] workspace creation
  - [ ] member addition updates `clpm.workspace` deterministically

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m \"feat: workspace scaffolding commands\"`

### Exit criteria
- One-command workspace creation and member onboarding exists and is tested

## Phase 11: Scripts/task runner (`clpm scripts`)

### Goal
- Make `:scripts` in `clpm.project` useful (uv/cargo-style task runner).

### Files to read into context
- `src/project.lisp`
- `src/commands.lisp`
- `src/main.lisp`
- `test/run-exec-test.lisp`

### Steps

#### Step 11.1: Define scripts schema (no ambiguity)
- [ ] Read the phase context files listed above
- [ ] Define `:scripts` as a list of forms:
  - [ ] `(:script :name "fmt" :type :shell :command ("sh" "-c" "…"))`
  - [ ] `(:script :name "repl" :type :lisp :system "my-app" :function "my-app::main")`
- [ ] Update `src/project.lisp` parsing to store raw list (existing), and add validation helpers in `src/commands.lisp`

#### Step 11.2: Implement `clpm scripts list/run`
- [ ] Read the phase context files listed above
- [ ] `clpm scripts list` prints script names sorted
- [ ] `clpm scripts run <name> [-- <args...>]`:
  - [ ] Ensures activation (`ensure-project-activated`)
  - [ ] For `:shell`: runs via `clpm.platform:run-program` in project root
  - [ ] For `:lisp`: runs via SBCL with config + dependency preload (reuse `cmd-run` machinery)

#### Step 11.3: Tests
- [ ] Add `test/scripts-command-test.lisp`:
  - [ ] Create temp project with scripts
  - [ ] Assert list output and that `run` returns expected exit codes

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m \"feat: add scripts task runner\"`

### Exit criteria
- Scripts are first-class and tested

## Phase 12: Multi-lisp selection (execution layer)

### Goal
- Add `--lisp` selection for `repl/run/test` and make cache keys include the selected lisp.

### Files to read into context
- `src/platform.lisp`
- `src/store.lisp`
- `src/build/driver.lisp`
- `src/build/orchestrator.lisp`
- `src/commands.lisp`
- `test/package-command-test.lisp`

### Steps

#### Step 12.1: Implement lisp implementation abstraction
- [ ] Read the phase context files listed above
- [ ] Add `src/lisp.lisp` with:
  - [ ] `find-lisp (kind)` where kind is `:sbcl`, `:ccl`, `:ecl`
  - [ ] `lisp-version (kind)` by invoking `<impl> --version` (exact argv per impl)
  - [ ] `lisp-run-argv (kind &key load-files eval-forms noinform noninteractive)` returning full argv
- [ ] Update `src/packages.lisp` and `clpm.asd` accordingly

#### Step 12.2: Add CLI and project config knobs
- [ ] Add global option `--lisp <sbcl|ccl|ecl>` parsed in `src/main.lisp` and passed via `clpm.commands:*lisp*`
- [ ] Add project `:lisp` setting in `clpm.project` (string) with precedence: CLI > project > default `sbcl`

#### Step 12.3: Wire into run/test/repl and build cache key
- [ ] Update:
  - [ ] `cmd-repl`, `cmd-run`, `cmd-test` to use `lisp-run-argv`
  - [ ] `clpm.store:compute-build-id` to include `lisp-kind` + `lisp-version` instead of `sbcl-version` only
  - [ ] Build driver generation: keep SBCL-specific driver for now, but gate non-SBCL builds with a clear error message (“build cache only supports SBCL in this phase”)

#### Step 12.4: Tests
- [ ] Add `test/lisp-selection-test.lisp`:
  - [ ] Create temp PATH containing stub executables `sbcl`/`ccl`/`ecl` (shell scripts) to test detection and argv generation deterministically
  - [ ] Assert `--lisp` selects correct program and errors when missing

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m \"feat: add --lisp selection for repl/run/test\"`

### Exit criteria
- Users can select a Lisp implementation for execution flows; cache keys are prepared for multi-lisp builds

## Phase 13: Multi-lisp builds (CCL/ECL build driver support)

### Goal
- Enable dependency builds under `--lisp ccl` and `--lisp ecl` (execution in child processes), with per-impl cache keys.

### Files to read into context
- `src/build/driver.lisp`
- `src/build/orchestrator.lisp`
- `src/store.lisp`
- `src/lisp.lisp`
- `test/build-parallel-test.lisp`

### Steps

#### Step 13.1: Build driver scripts per implementation
- [ ] Read the phase context files listed above
- [ ] Extend `generate-build-driver-script`:
  - [ ] Remove `sb-ext:exit` usage from driver by generating implementation-specific exit where possible:
    - [ ] SBCL: keep `sb-ext:exit`
    - [ ] CCL: use `(ccl:quit <code>)`
    - [ ] ECL: use `(ext:quit <code>)`
- [ ] Update orchestrator to invoke the selected lisp program instead of hard-coded `"sbcl"`

#### Step 13.2: Tests (no external CCL/ECL required)
- [ ] Add `test/build-driver-generation-test.lisp`:
  - [ ] Assert generated script contains correct quit form per selected impl
  - [ ] Assert orchestrator uses `src/lisp.lisp` argv builder

#### Step 13.3: Optional manual gate (if user has CCL/ECL)
- [ ] Document optional manual run:
  - [ ] `clpm --lisp ccl install` in `example/quicklisp-app` (expect failure if CCL not installed)

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m \"feat: support non-SBCL build drivers (ccl/ecl)\"`

### Exit criteria
- CLPM can build deps using selected Lisp implementation (when installed)
- Build cache keys remain correct and deterministic

## Phase 14: Clarify packaging support per implementation

### Goal
- Make `clpm package` behavior explicit and helpful under non-SBCL `--lisp`.

### Files to read into context
- `src/commands.lisp`
- `src/main.lisp`
- `test/package-command-test.lisp`

### Steps

#### Step 14.1: Gate packaging by implementation
- [ ] Read the phase context files listed above
- [ ] If `--lisp` is not `sbcl`, `clpm package` exits with code 1 and prints:
  - [ ] “Packaging currently supports SBCL only; re-run with `--lisp sbcl`”
- [ ] Add test ensuring error message and exit code

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m \"docs: clarify package supports sbcl only\"`

### Exit criteria
- Users are not surprised by packaging limitations

## Phase 15: Key generation (Ed25519 signing support)

### Goal
- Add Ed25519 signing (not just verification) and deterministic key file formats.

### Files to read into context
- `src/crypto/ed25519.lisp`
- `src/crypto/sha512.lisp`
- `src/io/sexp.lisp`
- `src/commands.lisp`
- `test/ed25519-verify-test.lisp`

### Steps

#### Step 15.1: Implement RFC8032 Ed25519 signing
- [ ] Read the phase context files listed above
- [ ] Add functions in `src/crypto/ed25519.lisp`:
  - [ ] `derive-public-key-from-seed` (32-byte seed → 32-byte public key)
  - [ ] `sign (message seed32) -> sig64` per RFC8032 (pure CL)
- [ ] Add private key file format (no ambiguity):
  - [ ] `<id>.key`: ASCII hex 32-byte seed (64 hex chars) + optional newline
  - [ ] `<id>.pub`: ASCII hex 32-byte public key (existing format)

#### Step 15.2: Add CLI `clpm keys generate`
- [ ] Implement `clpm keys generate --out <dir> --id <id>`
  - [ ] Reads 32 bytes from `/dev/urandom` (and Windows equivalent via `clpm.platform`)
  - [ ] Writes `<dir>/<id>.key` (0600) and `<dir>/<id>.pub` (0644)

#### Step 15.3: Tests
- [ ] Add `test/ed25519-sign-test.lisp`:
  - [ ] Generate a fixed seed (hard-coded bytes)
  - [ ] Sign a fixed message
  - [ ] Verify signature via existing `verify-signature`

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m \"feat: add ed25519 signing and keygen\"`

### Exit criteria
- CLPM can generate keys and sign data deterministically (given seed)

## Phase 16: Registry initialization (`clpm registry init`)

### Goal
- Create a new git registry directory with correct layout and initial signed snapshot.

### Files to read into context
- `src/registry/git.lisp`
- `src/io/sexp.lisp`
- `src/commands.lisp`
- `README.md`

### Steps

#### Step 16.1: Implement `clpm registry init`
- [ ] Read the phase context files listed above
- [ ] CLI: `clpm registry init --dir <path> --key-id <id> --keys-dir <dir>`
- [ ] Create:
  - [ ] `<path>/registry/snapshot.sxp` with `:releases ()`, `:provides ()`, `:generated-at` RFC3339
  - [ ] `<path>/registry/snapshot.sig` signed with `<keys-dir>/<id>.key`
  - [ ] `<path>/registry/keys/<id>.pub` copied from `<keys-dir>/<id>.pub`
  - [ ] `<path>/registry/packages/` directory
- [ ] Do not run any VCS commands (write files only). Document optional `jj git init <path>` in README.

#### Step 16.2: Tests
- [ ] Add `test/registry-init-test.lisp`:
  - [ ] Create temp dir, run `clpm registry init`
  - [ ] Assert files exist
  - [ ] Verify snapshot signature with the embedded pub key via existing verification path

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m \"feat: add registry init command\"`

### Exit criteria
- Anyone can create a valid signed registry locally with CLPM

## Phase 17: Publishing (`clpm publish`) to git registry

### Goal
- Add end-to-end “publish a project into a registry” flow with signed metadata.

### Files to read into context
- `src/registry/git.lisp`
- `src/project.lisp`
- `src/commands.lisp`
- `src/fetch.lisp`
- `src/io/fs.lisp`
- `test/registry-sig-test.lisp`

### Steps

#### Step 17.1: Define release metadata content rules
- [ ] Read the phase context files listed above
- [ ] Release metadata fields:
  - [ ] `:name` = project name
  - [ ] `:version` = project version
  - [ ] `:source` = `(:tarball :url <url> :sha256 <sha256>)` or `(:git ...)` (publish supports tarball first)
  - [ ] `:artifact-sha256` duplicated for convenience
  - [ ] `:systems` from `:systems` in `clpm.project`
  - [ ] `:system-deps` computed from ASDF system dependencies by loading `.asd` in a controlled temp build (exact algorithm: use `asdf:system-depends-on` after `asdf:load-asd` for each system)
  - [ ] Optional `:license`, `:homepage`, `:description` sourced from new optional keys in `clpm.project` (add to schema explicitly)

#### Step 17.2: Implement `clpm publish`
- [ ] CLI: `clpm publish --registry <dir|url> --key-id <id> --keys-dir <dir> [--project <dir>] [--tarball-url <url>]`
- [ ] Implementation (fully specified):
  - [ ] Read project manifest from `<dir>/clpm.project`
  - [ ] Create tarball from project root excluding `.clpm/`, `dist/`, `clpm.lock` (use `tar` via `clpm.platform:find-tar`)
  - [ ] Compute archive SHA-256
  - [ ] Require `--tarball-url` for now (publishing hosting is out-of-scope); use it as `:url` in release metadata
  - [ ] Write `registry/packages/<name>/<version>/release.sxp` canonical
  - [ ] Write `release.sig` detached signature using `<id>.key`
  - [ ] Update `registry/snapshot.sxp`:
    - [ ] Add `<name>@<version>` to `:releases` (sorted unique)
    - [ ] Add provides entries for each system in `:systems` (system-id → `<name>@<version>`, sorted unique)
  - [ ] Rewrite and re-sign `registry/snapshot.sig`
- [ ] Provide optional `--git-commit`:
  - [ ] If set, run `git add` and `git commit -m "publish: <name>@<version>"` inside registry dir (this is repo-external; allowed for CLPM functionality)

#### Step 17.3: Tests
- [ ] Add `test/publish-command-test.lisp`:
  - [ ] Create temp registry via `clpm registry init`
  - [ ] Create a temp project via `clpm new --lib`, add minimal metadata
  - [ ] Run `clpm publish` with a dummy tarball URL (no network)
  - [ ] Verify:
    - [ ] release files exist
    - [ ] snapshot updated
    - [ ] signature verification passes when registry is cloned/loaded by CLPM registry loader

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m \"feat: add publish command for signed git registries\"`

### Exit criteria
- CLPM can produce a signed registry update for a local project

## Phase 18: `clpm audit` (provenance report)

### Goal
- Provide a clear “what did you trust and what did you run” report.

### Files to read into context
- `src/commands.lisp`
- `src/project.lisp`
- `src/registry/git.lisp`
- `src/registry/quicklisp.lisp`
- `test/registry-sig-test.lisp`

### Steps

#### Step 18.1: Implement `cmd-audit`
- [ ] Read the phase context files listed above
- [ ] CLI: `clpm audit [--json]`
- [ ] Report includes:
  - [ ] Project name/version, lockfile generated-at
  - [ ] Registries: name/kind/url + trust string
  - [ ] For git registries: whether snapshot sig verified (and signature sha256 if recorded in lockfile)
  - [ ] For Quicklisp: whether distinfo pin is present and what it is
  - [ ] Dependency sources summary: counts of tarball/git/path
  - [ ] Warnings:
    - [ ] any `:path` deps
    - [ ] any `:git` deps not pinned to commit (should not happen; if it does, warn)
    - [ ] quicklisp registries with no trust configured

#### Step 18.2: Tests
- [ ] Add `test/audit-command-test.lisp` using the existing fake registry tests to assert stable output and warnings.

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m \"feat: add audit provenance report\"`

### Exit criteria
- Users can answer “what did CLPM do and what did it trust” with one command

## Phase 19: `clpm sbom` (CycloneDX JSON)

### Goal
- Provide a deterministic SBOM export for the resolved dependency set.

### Files to read into context
- `src/commands.lisp`
- `src/project.lisp`
- `src/registry/git.lisp`
- `src/io/sexp.lisp`
- `test/audit-command-test.lisp`

### Steps

#### Step 19.1: Implement CycloneDX JSON generator
- [ ] Read the phase context files listed above
- [ ] CLI: `clpm sbom --format cyclonedx-json [--output <path>]`
- [ ] Output requirements (fully specified):
  - [ ] JSON object with:
    - [ ] `bomFormat="CycloneDX"`, `specVersion="1.5"`, `version=1`
    - [ ] `metadata.tools` identifies CLPM version
    - [ ] `components` array sorted by `name`:
      - [ ] `name`, `version`, `purl` (`pkg:cl/<name>@<version>`)
      - [ ] `hashes` includes `SHA-256` (artifact-sha256 when known) and `SHA-1` (quicklisp content sha1 when present)
      - [ ] `licenses` when registry metadata provides it
- [ ] Determinism: stable key order and stable component ordering (implement via manual JSON writer, not printer randomization).

#### Step 19.2: Tests
- [ ] Add `test/sbom-command-test.lisp`:
  - [ ] Use small fake graph
  - [ ] Assert exact JSON (or canonicalized JSON) matches expected

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m \"feat: add sbom cyclonedx-json export\"`

### Exit criteria
- Deterministic SBOM export exists and is test-covered

## Phase 20: Tighten Quicklisp UX in docs + examples

### Goal
- Make the “online flow” first-class and easy to follow.

### Files to read into context
- `README.md`
- `example/quicklisp-app/README.md`
- `test/manual/quicklisp-live-workflow.lisp`

### Steps

#### Step 20.1: Update README Quicklisp-first path
- [ ] Read the phase context files listed above
- [ ] Add a “Quicklisp (online)” section:
  - [ ] `clpm registry add --quicklisp`
  - [ ] `clpm registry trust set quicklisp tofu`
  - [ ] `clpm new`, `clpm add`, `clpm test/run/package`

#### Step 20.2: Ensure examples match current behavior
- [ ] Ensure `example/quicklisp-app/.gitignore` ignores `dist/`, `.clpm/`, `clpm.lock`
- [ ] Update `example/quicklisp-app/README.md` if any command names/options changed

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Optional manual verification: `CLPM_LIVE_QUICKLISP=1 sbcl --script test/manual/quicklisp-live-workflow.lisp`
- [ ] Commit: `jj commit -m \"docs: improve quicklisp onboarding\"`

### Exit criteria
- Documentation matches the real online workflow and explains trust settings

## Phase 21: Workspace example project

### Goal
- Provide a “cargo workspace”-like example in `example/workspace/` that exercises member targeting.

### Files to read into context
- `example/workspace/README.md`
- `src/workspace.lisp`
- `test/workspace-commands-test.lisp`

### Steps

#### Step 21.1: Create example workspace
- [ ] Read the phase context files listed above
- [ ] Add `example/workspace/clpm.workspace` with two members:
  - [ ] `apps/hello-app` (bin)
  - [ ] `libs/hello-lib` (lib)
- [ ] Configure `hello-app` to depend on `hello-lib` via `--path` and add a Quicklisp dep for realism (alexandria)
- [ ] Add README instructions including `clpm -p` usage

#### Step 21.2: Add deterministic test
- [ ] Add `test/example-workspace-workflow-test.lisp`:
  - [ ] Create a temp workspace via `clpm new --workspace` and `--member-of`
  - [ ] Run `clpm -p <member> test` and `run` and assert exit codes

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m \"docs: add workspace example\"`

### Exit criteria
- A concrete workspace example exists and the workflow is tested

## Phase 22: Hardening: better error messages for common UX footguns

### Goal
- Improve “HN perceived quality”: errors should be actionable and consistent.

### Files to read into context
- `src/errors.lisp`
- `src/commands.lisp`
- `test/doctor-test.lisp`
- `test/cli-test.lisp`

### Steps

#### Step 22.1: Normalize user-facing errors
- [ ] Read the phase context files listed above
- [ ] Add specific error types/messages for:
  - [ ] “No clpm.project found” (include searched paths)
  - [ ] “Workspace root requires --package” (list members)
  - [ ] “System provided by multiple registries” (list candidates and how to choose)
  - [ ] “Missing tool tar/git/<lisp>” (include install hints per OS)

#### Step 22.2: Tests
- [ ] Add `test/ux-errors-test.lisp` asserting error messages contain required substrings

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m \"ux: improve actionable error messages\"`

### Exit criteria
- Common failure modes point directly to fixes

## Phase 23: Performance & caching polish for “uv feel”

### Goal
- Make common commands feel fast: fewer redundant resolves, smarter caching, better progress.

### Files to read into context
- `src/commands.lisp`
- `src/solver/pubgrub.lisp`
- `src/store.lisp`
- `test/fetch-parallel-test.lisp`

### Steps

#### Step 23.1: Avoid redundant resolves where safe
- [ ] Read the phase context files listed above
- [ ] Implement `clpm resolve` short-circuit:
  - [ ] If `clpm.project` canonical form hash unchanged and registries unchanged, skip resolve and reuse lockfile
  - [ ] Otherwise resolve as today
- [ ] Update tests to ensure determinism and no behavior regressions

#### Step 23.2: Progress output stability
- [ ] Ensure parallel fetch/build progress remains deterministic and non-garbled (single-writer logging)
- [ ] Add/extend tests to cover ordering where possible (or ensure no crashes)

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m \"perf: reduce redundant resolve and stabilize progress\"`

### Exit criteria
- Repeated `install/test/run` on unchanged projects is noticeably faster and stable

## Phase 24: “Excellent” final sweep (docs + command matrix + release checklist)

### Goal
- Ensure the repo reads like a product, not a prototype.

### Files to read into context
- `README.md`
- `src/main.lisp`
- `test/all-tests.sh`
- `example/quicklisp-app/README.md`
- `example/workspace/README.md`

### Steps

#### Step 24.1: Documentation parity
- [ ] Read the phase context files listed above
- [ ] Update README command table to include: `search/info/tree/why/scripts/audit/sbom/keys/publish/workspace`
- [ ] Add a “trust & provenance” section with concrete examples
- [ ] Add a “publish to registry” walkthrough with exact commands

#### Step 24.2: Release checklist file
- [ ] Add `RELEASING.md`:
  - [ ] Build CLPM binary (`sbcl --script clpm-bootstrap.lisp install-local .` or documented build)
  - [ ] Run tests (`sh test/all-tests.sh`)
  - [ ] Optional live Quicklisp smoke (`CLPM_LIVE_QUICKLISP=1 ...`)
  - [ ] Update version in `src/main.lisp`

### Gate: tests must pass
- [ ] Run tests: `sh test/all-tests.sh`
- [ ] Confirm: all tests passed
- [ ] Commit: `jj commit -m \"docs: finalize clpm excellence roadmap deliverables\"`

### Exit criteria
- Repo documentation matches actual behavior and provides a clear path for users and contributors

---

## Traceability matrix (acceptance criteria → phase/step → validation)

- AC1 (`search`): Phase 2 → `test/search-command-test.lisp` → `sh test/all-tests.sh`
- AC2 (`info`): Phase 3 → `test/info-command-test.lisp` → `sh test/all-tests.sh`
- AC3 (`tree`): Phase 4 → `test/tree-why-test.lisp` → `sh test/all-tests.sh`
- AC4 (`why`): Phase 4 → `test/tree-why-test.lisp` → `sh test/all-tests.sh`
- AC5 (Quicklisp TOFU/pin): Phase 5 → `test/quicklisp-trust-tofu-test.lisp` → `sh test/all-tests.sh`
- AC6 (registry trust cmds): Phase 6 → `test/registry-trust-cmd-test.lisp` → `sh test/all-tests.sh`
- AC7 (add UX + --registry): Phase 7 → `test/add-remove-test.lisp`, `test/add-disambiguate-registry-test.lisp` → `sh test/all-tests.sh`
- AC8 (workspace): Phase 8–10 → `test/workspace-discovery-test.lisp`, `test/workspace-commands-test.lisp`, `test/workspace-new-test.lisp` → `sh test/all-tests.sh`
- AC9 (scripts): Phase 11 → `test/scripts-command-test.lisp` → `sh test/all-tests.sh`
- AC10 (multi-lisp exec): Phase 12–13 → `test/lisp-selection-test.lisp`, `test/build-driver-generation-test.lisp` → `sh test/all-tests.sh`
- AC11 (publish): Phase 15–17 → `test/ed25519-sign-test.lisp`, `test/registry-init-test.lisp`, `test/publish-command-test.lisp` → `sh test/all-tests.sh`
- AC12 (audit): Phase 18 → `test/audit-command-test.lisp` → `sh test/all-tests.sh`
- AC13 (sbom): Phase 19 → `test/sbom-command-test.lisp` → `sh test/all-tests.sh`
- AC14 (onboarding): Phase 20–21 → docs review + optional manual Quicklisp test → `CLPM_LIVE_QUICKLISP=1 ...`
- AC15 (all tests pass): Every phase gate → `sh test/all-tests.sh`

## Risks & mitigations

1. Risk: Quicklisp TOFU pinning breaks users who expect “just update”.
   - Mitigation: Implement `clpm registry trust refresh <name>` and document it prominently; include clear error messages with next steps.
2. Risk: Multi-lisp support complicates cache keys and increases code complexity.
   - Mitigation: Add `src/lisp.lisp` as a narrow interface; gate build support per impl explicitly; keep packaging SBCL-only with clear message.
3. Risk: Implementing Ed25519 signing incorrectly creates false security.
   - Mitigation: Add known-answer tests (KAT) for signing/verification; cross-check against an external implementation in a manual test (documented) before release.
4. Risk: Workspace support introduces confusing root detection behavior.
   - Mitigation: Deterministic errors when `--package` is required; add targeted tests for all root detection cases.
5. Risk: Publish flow requires hosting tarballs (out of scope), leading to incomplete UX.
   - Mitigation: Make `--tarball-url` required for now and document recommended hosting options; keep registry update fully functional offline.

## Out of scope (explicit)
- Central hosted registry service.
- Vulnerability feed integration (OSV/CVE).
- OS sandboxing for builds.
- Full portability of CLPM to run under all CL implementations (only multi-lisp orchestration in this plan).


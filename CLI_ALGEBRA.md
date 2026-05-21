# CLPM CLI Algebra

## Work Mode

Mode:

- Design specification and implementation derivation for the public command
  surface.

Implementation allowed?

- Yes, for edits that derive the target surface in this document.
- No, for adding compatibility aliases, fallback command names, or extra
  convenience verbs that are not denoted here.

## Intent

This algebra models the public `clpm` command language as a small set of
resource-oriented operations over projects, dependency graphs, registries,
execution environments, the content store, and the persistent repl.

It deliberately ignores implementation names such as `cmd-install`,
temporary cache layout, process-launch mechanics, and the internal
representation of project or lockfile structs.

Primary users:

1. Common Lisp developers managing a local project or workspace.
2. Registry maintainers publishing signed CLPM metadata.
3. Agents debugging Common Lisp code through a persistent project image.

Representative examples:

1. `clpm deps sync` realizes the current project through activation.
2. `clpm deps sync --to lock` writes only the lockfile.
3. `clpm registry key import --pub registry.pub --id main` installs a trust
   anchor.
4. `clpm run test` executes configured test systems in the activated project.
5. `clpm repl eval FORM --debug` evaluates in the persistent project
   image and exposes debugger state.

Edge cases and failure modes:

1. A workspace root without `-p/--package` is not a project target.
2. Offline source realization fails when a locked artifact is not already in
   the store.
3. Registry trust updates must not silently weaken signature or hash checks.
4. Kept repl debugger sessions, watches, traces, and throwaway workers
   are observable repl state and must be explicitly cleaned up.

## Carrier Types

```haskell
data Invocation
data World
data Context
data Outcome
data Observation

data Project
data Workspace
data DependencyIntent
data Lockfile
data RegistryConfig
data RegistrySnapshot
data Store
data Activation
data Runtime
data ReplImage
```

Associated semantic types:

```haskell
data World =
  World { projects   :: Map ProjectRoot Project
        , workspaces :: Map WorkspaceRoot Workspace
        , config     :: RegistryConfig
        , locks      :: Map ProjectRoot Lockfile
        , registries :: Map RegistryName RegistrySnapshot
        , store      :: Store
        , active     :: Set ProjectRoot
        , runtime    :: Runtime
        , repls      :: Map ProjectRoot ReplImage
        }

data Outcome =
    Failed ExitCode Diagnostic
  | Succeeded World Observation

data Observation =
    Silent
  | HumanText Text
  | Json Value
  | Files (Set Path)
  | ProcessExit ExitCode
```

Implementation-shaped types intentionally excluded:

- Function names in `src/commands.lisp`.
- Whether a pipeline step is implemented by one function call or several.
- Cache keys and pid/socket file names, except where they are user-observable
  repl cleanup state.

## Observations

Primitive observations:

```haskell
help        :: CommandSchema -> Selector -> HumanText
doctor      :: Context -> World -> HumanText
search      :: Query -> World -> HumanText | Json
info        :: SystemId -> World -> HumanText | Json
tree        :: ProjectTarget -> World -> HumanText
why         :: ProjectTarget -> SystemId -> World -> HumanText
audit       :: ProjectTarget -> World -> HumanText | Json
sbom        :: ProjectTarget -> Format -> World -> HumanText | Files
replCall    :: ProjectTarget -> CallMethod -> Params -> World -> Outcome
```

Derived observations:

```haskell
registry list       = project config.registries
registry trust list = project config.registries.trust
registry key list   = project key-directory-public-keys
run scripts         = project current-project.scripts.names
store gc --dry-run  = project unreachable-store-entries
```

Operational non-semantics:

- Log line wording is not semantic except where tests lock command help or
  machine-readable output.
- Progress messages do not change equality.
- The order of unordered sets is non-semantic, but rendered output must sort
  deterministically.

## Chosen Denotation

Candidate A: flat verb table.

```haskell
denoteA :: Invocation -> World -> Outcome
```

Pros:

- Matches the previous implementation directly.

Cons:

- Makes `resolve`, `fetch`, `build`, and `install` appear independent even
  though they are projections of one realization pipeline.
- Places `keys` and `publish` beside project commands even though they are
  registry operations.
- Places `exec`, `test`, and `scripts` beside unrelated resource operations
  even though they all run inside a project environment.
- Treats an ordinary terminal REPL as a public peer of the persistent repl,
  splitting one REPL/debug meaning across two protocols.
- Has no law explaining why some nouns are top-level and some are nested.

Candidate B: one `project` command containing everything.

```haskell
denoteB :: ProjectInvocation -> ProjectTarget -> World -> Outcome
```

Pros:

- Few top-level commands.

Cons:

- Complects registry configuration, global key management, store garbage
  collection, and repl daemon state with a project manifest.
- Hides important non-project resources.

Chosen denotation: resource algebra.

```haskell
denote :: Invocation -> Context -> World -> Outcome

Invocation =
    ProjectOp ProjectOperation
  | DepsOp DependencyOperation
  | RegistryOp RegistryOperation
  | RunOp RunOperation
  | StoreOp StoreOperation
  | ReplOp ReplOperation
  | Doctor
  | Help Selector
  | Skill
```

Why this is the simplest precise model:

- Each top-level command names one semantic carrier.
- Pipeline prefixes become parameters of `deps sync`, not separate top-level
  verbs.
- Execution modes become `run` operations when they denote bounded
  one-shot project execution.
- REPL/debugging belongs only to `repl`, because persistent package
  state, debugger continuations, workers, watches, traces, and inspection are
  protocol state rather than a one-shot process launch.
- Registry keys and publishing move under `registry` because they mutate or
  observe registry trust and registry contents.
- Store cleanup moves under `store` because it operates on shared store
  reachability, not dependency intent.
- `repl` remains a top-level carrier because its long-lived image,
  workers, debugger sessions, watches, traces, and cleanup state are not the
  same resource as a one-shot project execution.

Values excluded from the model or represented by a restricted semantic domain:

- Unknown command names.
- Invalid project targets.
- Invalid registry trust strings.
- Invalid sync stages.
- Invalid repl RPC methods or parameter schemas.

Partiality, errors, strictness, ordering, nondeterminism:

- Every operation returns `Outcome`; expected user failures are semantic
  failures, not unspecified behavior.
- Outputs representing sets are sorted before rendering.
- Network and process effects are part of `World`; nondeterminism is confined
  to operations that explicitly consult registries, download sources, or run
  programs.

## Equality

Semantic equality is observational equality over the chosen resources:

```haskell
Law: "no public distinction after equal denotation"
forall i j ctx world.
  denote i ctx world = denote j ctx world
  => forall obs. observe obs i ctx world = observe obs j ctx world
```

The command surface may keep derived operations only when they buy essential
ergonomics without adding a new semantic primitive. A retained derived
operation must have a written expansion law.

## Target Surface

```text
clpm [options]
clpm [options] help [command [subcommand ...]]
clpm [options] doctor
clpm [options] skill

clpm [options] project new <name> --workspace [--dir <path>]
clpm [options] project new <name> --bin|--lib [--dir <path>]
clpm [options] project new <name> --bin|--lib --member-of <workspace-dir>
clpm [options] project init [name]
clpm [options] project workspace init|add|remove|list ...
clpm [options] project package

clpm [options] deps add [--dev|--test] [--any|--caret] ... <system>...
clpm [options] deps remove [--dev|--test] <system>
clpm [options] deps sync [--to lock|source|build|active]
clpm [options] deps update [system ...]
clpm [options] deps search <query> [--limit N] [--json]
clpm [options] deps info <system> [--json] [--all]
clpm [options] deps tree [--depth N]
clpm [options] deps why <system>
clpm [options] deps audit [--json]
clpm [options] deps sbom --format <format> [--out <path>]

clpm [options] registry list|add|update|trust|init ...
clpm [options] registry key generate|list|import|verify ...
clpm [options] registry publish ...

clpm [options] run [-- <args...>]
clpm [options] run exec -- <cmd...>
clpm [options] run test
clpm [options] run script <name> [-- <args...>]
clpm [options] run scripts

clpm [options] store clean [--dist] [--store]
clpm [options] store gc [--dry-run]

clpm [options] repl daemon [--detach] [--no-load] [--status [--json]] [--stop]
clpm [options] repl eval FORM [--package P] [--worker W] [--debug] ...
clpm [options] repl call METHOD [--params-json JSON] [--PARAM VALUE]...
```

Bare `clpm [options]` denotes `clpm [options] help`.
`repl call METHOD` excludes the daemon's `eval` RPC; public evaluation is
`repl eval FORM`.
`--offline` is accepted only where artifact/cache state can affect the
operation: `deps sync` beyond the lock stage and `deps sbom`.
`--jobs` is accepted only where dependency realization can perform parallel
source fetch or build work: `deps sync` beyond the lock stage.
`--lisp` is accepted only where CLPM chooses a Lisp implementation: build or
active dependency realization, project packaging, and `run` operations that
execute Lisp entrypoints/tests/scripts.
`-p/--package` is accepted only where CLPM resolves project state from a
workspace root: project packaging, dependency operations, registry publish,
run, repl, and project-local store clean.
Optional dependency flags are accepted only by dependency resolution:
`deps sync` and `deps update`.
Fetch tuning flags are accepted only by CLPM-managed network fetch
operations: `deps sync`, `deps update`, `deps search`, `deps info`,
`deps sbom`, `registry update`, and `registry trust refresh`.

## Current Surface Classification

| Current command | Class | Target command | Reason |
| --- | --- | --- | --- |
| `help` | Observation | `help` | Observation over command schema. |
| `doctor` | Observation | `doctor` | Environment observation, not project-specific. |
| `skill` | Observation | `skill` | Emits agent instructions. |
| `new` | Primitive constructor | `project new` | Creates a project/workspace resource. |
| `init` | Primitive constructor | `project init` | Creates a project manifest. |
| `workspace` | Primitive/lifecycle | `project workspace` | Workspace is project topology. |
| `add` | Primitive constructor | `deps add` | Adds dependency intent to the manifest. |
| `remove` | Primitive constructor | `deps remove` | Removes dependency intent from the manifest. |
| `resolve` | Derived pipeline prefix | `deps sync --to lock` | Lockfile realization. |
| `fetch` | Derived pipeline prefix | `deps sync --to source` | Source-store realization. |
| `build` | Derived pipeline prefix | `deps sync --to build` | Build-store realization. |
| `install` | Derived default pipeline | `deps sync` | Activation realization. |
| `update` | Lifecycle constructor | `deps update` | Re-resolve with an unlock set and registry refresh. |
| `search` | Observation | `deps search` | Observes dependency providers. |
| `info` | Observation | `deps info` | Observes dependency candidates. |
| `tree` | Observation | `deps tree` | Observes resolved dependency graph. |
| `why` | Observation | `deps why` | Observes reachability in dependency graph. |
| `audit` | Observation | `deps audit` | Observes lockfile provenance and trust. |
| `sbom` | Observation/artifact | `deps sbom` | Renders lockfile dependency components. |
| `registry` | Primitive/lifecycle | `registry` | Owns registry config, trust, and snapshots. |
| `keys` | Accidental top-level wrapper | `registry key` | Keys are registry trust material. |
| `publish` | Accidental top-level wrapper | `registry publish` | Publishing writes registry metadata. |
| `run` | Primitive execution | `run` | Runs the configured project entrypoint. |
| `exec` | Derived execution | `run exec` | Executes an arbitrary command in the project environment. |
| `test` | Derived execution | `run test` | Executes configured test systems. |
| `run repl` | Rejected duplicate protocol | `repl` | Ordinary REPL/debugging is the same semantic carrier as the persistent repl and is not public separately. |
| `scripts` | Derived execution/listing | `run script`, `run scripts` | Scripts are named project executions. |
| `package` | Artifact constructor | `project package` | Builds the artifact configured by project metadata. |
| `clean` | Store/project cleanup | `store clean` | Removes generated project/store reachability. |
| `gc` | Store cleanup | `store gc` | Garbage-collects unreachable store entries. |
| `repl` | Primitive carrier | `repl` | Persistent image has independent lifecycle state. |

## Output Contract Inventory

Semantic equality includes exit status, stdout/stderr class, files written,
and persisted state. Exact human wording is semantic only where tests lock it,
but output kind and machine-readable shape are semantic.

| Surface | Success stdout | Success stderr | File/state effects | Machine mode | Evidence |
| --- | --- | --- | --- | --- | --- |
| `help`, bare `clpm`, `--help` | Human command schema | Empty | None | None | `test/help-output-test.lisp`, `test/cli-test.lisp` |
| `doctor` | Human `ok/warn/error` checks and final status | Empty in normal checks | None | None | `test/doctor-test.lisp` |
| `skill` | SKILL.md markdown | Empty | None | None | `test/skill-command-test.lisp` |
| `project new` | Human creation lines | Errors only | Creates project/workspace files | None | `test/new-command-test.lisp`, `test/workspace-new-test.lisp` |
| `project init` | Human creation lines | Errors only | Writes `clpm.project` | None | `test/project-roundtrip-test.lisp` |
| `project workspace init/add/remove` | Human mutation lines | Errors only | Writes `clpm.workspace` | None | `test/workspace-commands-test.lisp` |
| `project workspace list` | Sorted member names, one per line | Errors only | None | None | `test/workspace-subcommand-test.lisp` |
| `project package` | Human packaging lines | Errors/child failures | Writes distributable executable and metadata | None | `test/package-command-test.lisp` |
| `deps add/remove` | Human mutation lines | Errors only | Rewrites `clpm.project` dependency intent | None | `test/add-remove-test.lisp` |
| `deps sync --to lock` | Human resolver lines | Errors only | Writes `clpm.lock` | None | `test/resolve-short-circuit-test.lisp`, `test/update-selective-test.lisp` |
| `deps sync --to source` | Human resolver/fetch lines | Errors only | Writes lock/source hashes and store sources | None | `test/path-dep-test.lisp`, `test/git-dep-test.lisp` |
| `deps sync --to build` | Human resolver/fetch/build lines | Errors/build log pointers | Writes build store entries | None | `test/build-parallel-test.lisp` |
| `deps sync --to active` / default | Human resolver/fetch/build/activation lines | Errors only | Writes activation config under `.clpm/` | None | `test/example-workflow-test.lisp` |
| `deps update [system...]` | Human update lines | Errors only | Rewrites lockfile for unlock set | None | `test/update-selective-test.lisp` |
| `deps search` | Tab-separated human rows | Errors only | None | `--json` array/object rows | `test/search-command-test.lisp` |
| `deps info` | Human selected/candidate sections | Errors only | None | `--json` object | `test/info-command-test.lisp` |
| `deps tree` | Human dependency tree lines | Errors only | None | None | `test/tree-why-test.lisp` |
| `deps why` | Human reachability path lines | Errors only | None | None | `test/tree-why-test.lisp` |
| `deps audit` | Human audit report | Errors only | None | `--json` object | `test/audit-command-test.lisp` |
| `deps sbom` | SBOM document when `--out` absent | Errors only | Writes `--out` path when supplied | `cyclonedx-json`, `cyclonedx-xml`, `spdx-json` formats | `test/sbom-command-test.lisp` |
| `registry add/list/update` | Human registry rows/status | Errors only | Updates config and/or registry snapshots | None | `test/registry-cmd-test.lisp`, `test/registry-init-test.lisp` |
| `registry trust list/set/refresh` | Human trust rows/status | Errors only | Updates trust config/pins | None | `test/registry-trust-cmd-test.lisp`, `test/quicklisp-trust-tofu-test.lisp` |
| `registry key generate/import` | Human key file lines | Errors only | Writes key files | None | `test/keys-subcommand-test.lisp` |
| `registry key list` | Human key/fingerprint rows | Errors only | None | None | `test/keys-subcommand-test.lisp` |
| `registry key verify` | Human verification status | Errors only | None | None | `test/keys-subcommand-test.lisp` |
| `registry init` | Human generated file lines | Errors only | Writes registry snapshot/signature/key files | None | `test/registry-init-test.lisp` |
| `registry publish` | Human tarball/release/snapshot lines | Errors only | Writes tarball/release/snapshot/signature files | None | `test/publish-command-test.lisp` |
| `run` | Child/project entrypoint stdout | Child/project stderr/errors | May realize activation before running | Child process/lisp exit status | `test/run-exec-test.lisp` |
| `run exec` | Child stdout | Child stderr/errors | May realize activation before running | Child exit status | `test/run-exec-test.lisp` |
| `run test` | Human test/load lines from test driver | Errors only | May realize activation before testing | Lisp test outcome exit status | `test/test-command-test.lisp` |
| `run script` | Script stdout | Script stderr/errors | May realize activation before running | Script outcome exit status | `test/scripts-command-test.lisp` |
| `run scripts` | Script names, one per line | Errors only | None | None | `test/scripts-command-test.lisp` |
| `store clean` | Human deletion/untracking lines | Errors only | Removes `.clpm/`, `dist/`, optional store entries, and GC roots | None | `test/clean-command-test.lisp` |
| `store gc` | Human deletion summary | Errors only | Deletes unreachable store entries unless `--dry-run` | Dry-run is human observation only | `test/gc-roots-test.lisp` |
| `repl daemon` | Foreground server blocks; detach returns status line | Launch errors only | Writes pid/socket/log lifecycle files | None | `test/repl-cli-test.lisp` |
| `repl daemon --status` | Human status | Errors only | Cleans stale pid/socket files | `--json` status object | `test/repl-cli-test.lisp` |
| `repl daemon --stop` | Human stop/not-running status | Errors only | Removes daemon lifecycle state | None | `test/repl-cli-test.lisp` |
| `repl eval` | Human `=> value` plus captured output | Debug/errors on stderr | Mutates persistent image/worker state | `--json` raw eval response | `test/repl-cli-test.lisp`, `test/repl-cli-subcommands-test.lisp` |
| `repl call` | Raw JSON response/event frames | Transport/usage errors | Mutates only the called method's existing REPL resource; never autostarts | Always JSON | `test/repl-cli-subcommands-test.lisp`, `test/repl-methods-test.lisp` |

## Hostile Reduction Ledger

### Iteration 1: Inventory as Denotation

- Commands deleted: none; this pass inventories meaning before cutting.
- Commands merged: none yet.
- Commands derived instead of exposed:
  - `resolve`, `fetch`, `build`, and `install` are projections of the
    realization pipeline.
  - `keys` and `publish` are registry trust/release operations.
  - `exec`, `test`, and `scripts` are project execution operations.
- Commands that survived and why:
  - `project`, `deps`, `registry`, `run`, `store`, and `repl`
    each name a distinct semantic carrier.
  - `doctor`, `help`, and `skill` survive as observations over environment,
    schema, and agent instructions.
- Laws/protocol invariants added:
  - `help` is a schema projection.
  - `deps sync --to STAGE` denotes a prefix of one realization pipeline.
  - `registry key` and `registry publish` operate under registry scope.
- Remaining discomfort:
  - The first pass still tolerated a public ordinary REPL as execution even
    though it shared meaning with the persistent repl.

### Iteration 2: Delete Derived Top-Level Verbs

- Commands deleted:
  - Top-level `new`, `init`, `workspace`, `add`, `remove`, `resolve`,
    `fetch`, `build`, `install`, `update`, `search`, `info`, `tree`, `why`,
    `audit`, `sbom`, `keys`, `publish`, `exec`, `test`, `scripts`,
    `package`, `clean`, and `gc`.
- Commands merged:
  - Project constructors under `project`.
  - Dependency intent, realization, and dependency observations under `deps`.
  - Registry configuration, trust, keys, and publishing under `registry`.
  - Project execution modes under `run`.
  - Project/store cleanup under `store`.
- Commands derived instead of exposed:
  - Pipeline stages derive from `deps sync --to lock|source|build|active`.
  - Registry key and publish operations derive from registry scope.
  - Script listing and execution derive from `run scripts` and `run script`.
- Commands that survived and why:
  - The six resource carriers survive because deleting one loses a distinct
    carrier or mixes independent protocol state.
- Laws/protocol invariants added:
  - Parser/help morphism: accepted commands are exactly documented public
    commands.
  - Removed top-level verbs are rejected, not aliased.
- Remaining discomfort:
  - `run repl` still exposed a second REPL/debug protocol and kept the repl
    from being the single controlled protocol hook.

### Iteration 3: Collapse REPL/Debug Protocol

- Commands deleted:
  - `run repl` and the ordinary REPL implementation entry point.
- Commands merged:
  - Ordinary REPL/debugging is merged into `repl`; users evaluate
    forms with `eval`, manage lifecycle with `daemon`, and use controlled
    protocol hooks through `call`.
- Commands derived instead of exposed:
  - "Start a project REPL" is no longer a public constructor. For interactive
    state, use `repl daemon --detach`; for one form, use
    `repl eval`; for debugger and image operations, use
    `repl call METHOD`.
- Commands that survived and why:
  - `run` survives only for bounded project execution: entrypoint, `exec`,
    `test`, `script`, and `scripts`.
  - `repl` survives as the controlled MOP-style protocol layer:
    `daemon` is lifecycle glue, `eval` is the ergonomic interface, and
    `call` is the generic intercessory/introspective hook.
- Laws/protocol invariants added:
  - REPL/debug uniqueness: no public command outside `repl` may create
    or observe interactive Lisp image/debugger state.
  - REPL cleanup leaves no kept debugger sessions, watches, traces, or
    throwaway workers.
- Remaining discomfort:
  - None sufficient to justify another public command. Another hostile pass
    cannot remove `daemon`, `eval`, or `call` without losing lifecycle
    control, ergonomic one-form evaluation, or controlled protocol access.

### Iteration 4: Attack Empty Invocations and Package Exports

- Commands deleted:
  - No additional CLI tokens. The parser had already stopped accepting the
    derived top-level commands.
- Commands merged:
  - No additional semantic carriers.
- Commands derived instead of exposed:
  - Internal implementation functions such as `cmd-resolve`, `cmd-install`,
    `cmd-keys`, and `cmd-gc` remain implementation details of the resource
    dispatchers, not exported public API.
- Commands that survived and why:
  - Bare `clpm` survives only as `help`, a safe schema observation. Letting
    an empty invocation mutate `clpm.lock`, fetch sources, compile code, or
    activate a project fails the "no surprise effects" test.
  - The exported `clpm.commands` command surface is limited to resource
    dispatchers and observation commands: `cmd-project`, `cmd-deps`,
    `cmd-registry`, `cmd-run`, `cmd-store`, `cmd-repl`, `cmd-skill`,
    `cmd-help`, and `cmd-doctor`.
- Laws/protocol invariants added:
  - Empty invocation is observational: `parse [] = Help Root`.
  - Package export morphism: exported command-handler symbols are exactly the
    public command constructors, not every implementation leaf.
- Remaining discomfort:
  - `--insecure` still exists because explicit debugging sometimes requires
    bypassing trust. A later pass must prove it is scoped to verifier-bearing
    commands rather than tolerated as inert decoration everywhere.

### Iteration 5: Attack Output-Mode Scope

- Commands deleted:
  - No commands. The cut removes a hidden option placement: `clpm repl --json`
    is not a public observation.
- Commands merged:
  - No semantic carriers.
- Commands derived instead of exposed:
  - Resource-level `repl --json` had no denotation. JSON rendering belongs to
    the observation that can produce a machine-readable value:
    `repl eval ... --json` or `repl daemon --status --json`.
- Commands that survived and why:
  - `repl daemon --status --json` survives because daemon status is a useful
    machine observation.
  - `repl eval ... --json` survives because eval responses are already typed
    RPC responses.
  - `repl call METHOD` remains JSON by construction; it is the raw protocol
    escape hatch and does not need an output-mode flag.
- Laws/protocol invariants added:
  - Output mode is scoped to observations, not resource dispatchers:
    `parse ["repl", "--json"] = Error`.
  - JSON daemon status is an observation:
    `parse ["repl", "daemon", "--status", "--json"] =
     Repl (Daemon (Status Json))`.
- Remaining discomfort:
  - `repl call METHOD` is still intentionally broad. It survives only as the
    controlled protocol hook and remains a target for the next RPC-overreach
    attack.

### Iteration 6: Attack Integrity-Override Scope

- Commands deleted:
  - No commands. The cut removes inert `--insecure` placements.
- Commands merged:
  - No semantic carriers.
- Commands derived instead of exposed:
  - `--insecure` is not an observation modifier and not a command-wide mode.
    It denotes exactly one thing: disable signed-registry verification while
    loading registry data for a command that would otherwise verify it.
- Commands that survived and why:
  - `--insecure deps sync`, `--insecure deps update`,
    `--insecure deps search`, `--insecure deps info`, and
    `--insecure registry update` survive because each may load signed
    registry snapshots or release metadata.
  - `--insecure help`, `repl --insecure`, and other inert placements are
    rejected. A dangerous flag with no denotation is worse than syntax noise.
- Laws/protocol invariants added:
  - Integrity overrides are verifier-scoped:
    `parse ["--insecure", "help"] = Error`.
  - A verifier-bearing command may carry the override:
    `parse ["--insecure", "deps", "sync", "--to", "lock"] =
     Right (deps (sync Lock) with IntegrityOverride)`.
  - Invocation option specials are per-run bindings; one `run-cli` call must
    not leak `--insecure` into the next in-process call.
- Remaining discomfort:
  - `repl call METHOD` still accepts all registered non-eval protocol methods.
    The next attack is whether parameter construction is too loose at the CLI
    boundary.

### Iteration 8: Attack Offline Scope

- Commands deleted:
  - No commands. The cut removes inert `--offline` placements.
- Commands merged:
  - No semantic carriers.
- Commands derived instead of exposed:
  - `--offline` denotes an artifact/cache constraint, not a global program
    mode. It is only meaningful for dependency realization stages that may
    fetch sources or build artifacts, and for SBOM registry enrichment from a
    lockfile.
- Commands that survived and why:
  - `--offline deps sync`, `--offline deps sync --to source|build|active`,
    and `--offline deps sbom` survive because they can consult cached source,
    build, or registry-enrichment state.
  - `--offline help`, `repl --offline`, and `deps sync --to lock --offline`
    are rejected because they do not perform artifact/cache realization.
- Laws/protocol invariants added:
  - Offline is cache-scoped:
    `parse ["--offline", "help"] = Error`.
  - Lock-only resolution is not an offline artifact operation:
    `parse ["--offline", "deps", "sync", "--to", "lock"] = Error`.
- Remaining discomfort:
  - `--jobs`, `--lisp`, optional-dependency flags, and fetch tuning are still
    accepted as broad parser options. Each needs the same denotational audit.

### Iteration 9: Attack Parallelism Scope

- Commands deleted:
  - No commands. The cut removes inert `--jobs` placements.
- Commands merged:
  - No semantic carriers.
- Commands derived instead of exposed:
  - `--jobs` denotes the parallelism budget for dependency realization. It is
    not a process-wide scheduler option and not a REPL/run/store/registry
    option.
- Commands that survived and why:
  - `--jobs N deps sync`, `--jobs N deps sync --to source`,
    `--jobs N deps sync --to build`, and
    `--jobs N deps sync --to active` survive because source fetching and
    dependency building consume the job budget.
  - `--jobs N deps sync --to lock` is rejected because lockfile resolution is
    not currently parallelized through this option.
  - `--jobs N help` and `repl -j N` are rejected as inert parser decoration.
- Laws/protocol invariants added:
  - Parallelism is realization-scoped:
    `parse ["--jobs", n, "help"] = Error`.
  - Lock-only resolution has no parallel job budget:
    `parse ["--jobs", n, "deps", "sync", "--to", "lock"] = Error`.
- Remaining discomfort:
  - `--lisp`, optional-dependency flags, and fetch tuning are still accepted
    as broad parser options. Each needs the same denotational audit.

### Iteration 10: Attack Lisp-Selection Scope

- Commands deleted:
  - No commands. The cut removes inert `--lisp` placements.
- Commands merged:
  - No semantic carriers.
- Commands derived instead of exposed:
  - `--lisp` denotes Lisp implementation selection for commands where CLPM
    constructs the Lisp process or build driver. It is not a REPL transport
    option and not meaningful for source-only dependency realization.
- Commands that survived and why:
  - `--lisp deps sync`, `--lisp deps sync --to build|active`,
    `--lisp project package`, and `--lisp run ...` survive because they choose
    the Lisp used for dependency building, activation, packaging, tests,
    scripts, or entrypoints.
  - `--lisp deps sync --to source` is rejected because fetching sources does
    not choose a Lisp.
  - `--lisp help` and `--lisp repl` are rejected as inert parser decoration.
- Laws/protocol invariants added:
  - Lisp selection is process-constructor-scoped:
    `parse ["--lisp", impl, "help"] = Error`.
  - Source-only realization has no Lisp implementation:
    `parse ["--lisp", impl, "deps", "sync", "--to", "source"] = Error`.
- Remaining discomfort:
  - Optional-dependency flags and fetch tuning are still accepted as broad
    parser options. Each needs the same denotational audit.

### Iteration 11: Attack Optional-Dependency Scope

- Commands deleted:
  - No commands. The cut removes inert optional-dependency flag placements.
- Commands merged:
  - No semantic carriers.
- Commands derived instead of exposed:
  - `--with-optional` and `--with-all-optional` denote a dependency
    resolution input. They are not global project, help, run, or REPL options.
- Commands that survived and why:
  - `--with-optional SYS deps sync`, `--with-all-optional deps sync`, and the
    same flags on `deps update` survive because they change the effective
    optional dependency set during solving.
  - `--with-optional SYS help` and `--with-all-optional repl` are rejected as
    inert parser decoration.
- Laws/protocol invariants added:
  - Optional opt-ins are solve-scoped:
    `parse ["--with-optional", sys, "help"] = Error`.
  - Dependency sync carries the opt-in set:
    `parse ["--with-optional", sys, "deps", "sync", "--to", "lock"] =
     Right (deps (sync Lock) with OptionalOptIns {sys})`.
- Remaining discomfort:
  - Fetch tuning is still accepted as a broad parser option and needs the same
    denotational audit.

### Iteration 12: Attack Fetch-Tuning Scope

- Commands deleted:
  - No commands. The cut removes inert fetch retry/timeout flag placements.
- Commands merged:
  - No semantic carriers.
- Commands derived instead of exposed:
  - `--fetch-retries` and `--fetch-timeout` denote CLPM-managed network fetch
    policy. They are not general process options, not REPL transport options,
    and not help/output modifiers.
- Commands that survived and why:
  - Fetch tuning survives on `deps sync`, `deps update`, `deps search`,
    `deps info`, `deps sbom`, `registry update`, and
    `registry trust refresh` because those operations may fetch registry or
    dependency metadata/artifacts through CLPM.
  - `--fetch-retries N help` and `--fetch-timeout N repl` are rejected as
    inert parser decoration.
- Laws/protocol invariants added:
  - Fetch tuning is fetch-scoped:
    `parse ["--fetch-retries", n, "help"] = Error`.
  - Fetching observations may carry a retry budget:
    `parse ["--fetch-retries", n, "deps", "search", query] =
     Right (deps (search query) with FetchBudget n)`.
- Remaining discomfort:
  - `repl call METHOD` still accepts all registered non-eval protocol methods.
    The next attack is whether parameter construction is too loose at the CLI
    boundary.

### Iteration 7: Attack Eval-as-Raw-RPC Alias

- Commands deleted:
  - No whole command. The cut removes `clpm repl call eval ...` as a second
    CLI spelling of evaluation.
- Commands merged:
  - Evaluation is merged back into `clpm repl eval FORM`.
- Commands derived instead of exposed:
  - The daemon still has an internal `eval` RPC, but the public CLI projection
    does not expose it through `call`. The derived public constructor is
    `repl eval`, with human output, debug options, package/worker options, and
    JSON output where requested.
- Commands that survived and why:
  - `repl call methods` and `repl call help --method eval` survive because
    schema discovery is an observation over the daemon protocol.
  - Non-eval protocol methods survive under `call` because they operate on
    persistent image/debugger/inspector/watch/trace state that has no smaller
    stable CLI constructor yet.
- Laws/protocol invariants added:
  - Eval has exactly one public CLI constructor:
    `parse ["repl", "call", "eval", "--form", form] = Error`.
  - Discovery is not dispatch:
    `parse ["repl", "call", "help", "--method", "eval"] =
     Right (repl (call Help {"method": "eval"}))`.
- Remaining discomfort:
  - `repl call METHOD` still accepts all registered non-eval protocol methods.
    The next attack is whether parameter construction is too loose at the CLI
    boundary.

### Iteration 13: Attack `repl call` Parameter Overreach

- Commands deleted:
  - No new command tokens. The prior `repl call eval` deletion remains the
    important anti-aliasing cut.
- Commands merged:
  - No semantic carriers.
- Commands derived instead of exposed:
  - One-off wrappers for non-eval RPCs stay derived recipes:
    `repl call METHOD --PARAM VALUE...` or `--params-json OBJECT`.
- Commands that survived and why:
  - `repl call METHOD` survives because the REPL daemon's method registry is
    itself a semantic carrier: `methods` and `help` expose the closed method
    domain, parameter names, parameter types, requiredness, and docs.
  - `--PARAM VALUE` survives as a projection into that typed parameter object,
    not as an untyped escape hatch. The daemon rejects unknown method names,
    unknown parameter names, missing required parameters, non-object params,
    and wrong JSON value types against the same registry that `methods` and
    `help` report.
  - `--params-json OBJECT` survives because array/object/null parameters
    cannot be represented faithfully by scalar shell flags.
- Laws/protocol invariants added:
  - The public call constructor is closed over the registry:
    `method notin registry => denote (repl (call method params)) =
     Failed protocol-error`.
  - Parameters are schema-typed:
    `param notin method.params => Failed protocol-error`.
  - CLI scalar params and `--params-json` are two renderings of the same
    JSON object; later occurrences with the same key overwrite earlier ones.
- Remaining discomfort:
  - The complete command inventory still needs an output-contract audit:
    human text versus JSON versus file writes must be classified for every
    surviving observation.

### Iteration 14: Attack Output-Contract Ambiguity

- Commands deleted:
  - No commands. The cut is semantic: every surviving command now has an
    explicit output class instead of inheriting accidental stdout/stderr
    behavior from its implementation function.
- Commands merged:
  - No semantic carriers.
- Commands derived instead of exposed:
  - Machine output is a leaf observation, never an ambient top-level mode.
    `--json` remains scoped to `deps search`, `deps info`, `deps audit`,
    `repl daemon --status`, and `repl eval`; `repl call` is JSON by
    construction; SBOM machine formats are selected by `--format`.
- Commands that survived and why:
  - Human status outputs survive for mutating commands because they are the
    only direct observation of successful file/state effects.
  - JSON survives where the command denotes a structured observation consumed
    by tools.
  - `--out` survives only for SBOM because that command's denotation is a
    document artifact, not ordinary status text.
- Laws/protocol invariants added:
  - Each public command has a single default output class listed in the
    Output Contract Inventory.
  - Machine-readable output is explicit and leaf-scoped.
  - Success diagnostics go to stdout; usage/semantic failures go to stderr.
- Remaining discomfort:
  - Registry operator surface remains broad. The next attack is whether
    `registry trust`, `registry key`, `registry init`, and `registry publish`
    all have independent user stories or whether some should become
    lower-level/manual operations.

### Iteration 15: Attack Permanent Trust Weakening

- Commands deleted:
  - No command tokens. The cut removes `none`/`nil` as values denoted by
    `registry trust set`.
- Commands merged:
  - No semantic carriers.
- Commands derived instead of exposed:
  - Permanent "no verification" registry state is not a CLI trust mode.
    One-run debugging remains available through scoped `--insecure` on
    verifier-bearing commands.
- Commands that survived and why:
  - `registry trust set NAME ed25519:<key-id>` survives for signed git
    registries because it changes the public-key capability used to verify
    registry snapshots and releases.
  - `registry trust set NAME tofu` and
    `registry trust set NAME sha256:<64-hex-digest>` survive for Quicklisp
    registries because Quicklisp lacks signatures and needs an explicit pin
    or first-use pinning protocol.
  - `registry trust list` survives as the observation of configured trust.
  - `registry trust refresh NAME` survives only for Quicklisp pins; it is an
    explicit re-pin operation for changed distinfo contents.
- Laws/protocol invariants added:
  - Trust values are kind-typed:
    `GitTrust = Ed25519 KeyId`,
    `QuicklispTrust = Tofu | Sha256 Digest`.
  - There is no CLI constructor for `NoTrust`:
    `parse ["registry", "trust", "set", name, "none"] = Error`.
  - Failed trust changes are non-mutating:
    `denote (registry (trustSet name invalidTrust)) ctx world =
     Failed 1 diagnostic` and the registry config in `world` is unchanged.
- Remaining discomfort:
  - `registry key`, `registry init`, and `registry publish` remain broad
    operator/admin functionality. They still need a separate attack for
    whether the CLI should own all of those workflows.

### Iteration 16: Attack Duplicate Trust Refresh

- Commands deleted:
  - `registry update --refresh-trust`.
- Commands merged:
  - Quicklisp pin refresh is only `registry trust refresh NAME`.
- Commands derived instead of exposed:
  - Updating registry snapshots and mutating trust pins are different state
    transitions. `registry update` denotes snapshot refresh; it does not own
    trust mutation flags.
- Commands that survived and why:
  - `registry update [name ...]` survives as the registry snapshot/cache
    update operation.
  - `registry trust refresh NAME` survives as the explicit Quicklisp
    re-pinning operation.
- Laws/protocol invariants added:
  - Trust mutation is trust-scoped:
    `parse ["registry", "update", "--refresh-trust", name] = Error`.
  - Re-pinning uses the trust constructor:
    `parse ["registry", "trust", "refresh", quicklispName] =
     Right (registry (trustRefresh quicklispName))`.
- Remaining discomfort:
  - `registry key`, `registry init`, and `registry publish` remain broad
    operator/admin functionality. `registry publish --git-commit` is the next
    implementation-shaped side effect to remove.

### Iteration 17: Attack Publish VCS Side Effects

- Commands deleted:
  - `registry publish --git-commit`.
- Commands merged:
  - No semantic carriers.
- Commands derived instead of exposed:
  - Version-control staging and commits are lower-level repository operations,
    not CLPM registry artifact construction.
- Commands that survived and why:
  - `registry publish` survives because it writes release metadata, release
    signatures, snapshot metadata, snapshot signatures, and optionally the
    tarball artifact. Those are CLPM registry artifacts.
- Laws/protocol invariants added:
  - Publish writes registry artifacts only:
    `parse ["registry", "publish", ..., "--git-commit"] = Error`.
  - CLPM does not run VCS commands as part of publish; callers inspect and
    commit the registry working copy themselves.
- Remaining discomfort:
  - `registry key`, `registry init`, and `registry publish` remain broad
    operator/admin functionality, but their side effects are now CLPM-owned
    registry data rather than ambient VCS state.

### Iteration 18: Attack No-Op Eval Output Alias

- Commands deleted:
  - `repl eval --pretty`.
- Commands merged:
  - Human rendering remains the default `repl eval` observation.
- Commands derived instead of exposed:
  - A flag that denotes the default observation is not a constructor.
    `--json` is the only eval output-mode flag because it changes the
    observation to the raw structured RPC response.
- Commands that survived and why:
  - `repl eval FORM` survives as human one-form evaluation.
  - `repl eval FORM --json` survives as machine-readable eval response.
- Laws/protocol invariants added:
  - No inert output aliases:
    `parse ["repl", "eval", form, "--pretty"] = Error`.
- Remaining discomfort:
  - Root help still groups leaf-scoped options under the top-level usage
    banner. That should be attacked next as help/schema drift.

### Iteration 19: Attack Run Argument Boundary

- Commands deleted:
  - Bare `clpm run ARG...` as an implicit entrypoint-argument form.
- Commands merged:
  - Entrypoint execution has exactly two surface forms: `clpm run` and
    `clpm run -- ARG...`.
- Commands derived instead of exposed:
  - Application arguments are not CLPM subcommands. The `--` separator is the
    only constructor that moves subsequent words from the CLPM command algebra
    into the application argument payload.
- Commands that survived and why:
  - `clpm run` survives as entrypoint execution with empty argv.
  - `clpm run -- ARG...` survives as entrypoint execution with explicit argv.
  - `clpm run exec|test|script|scripts ...` survive as named execution
    operations.
- Laws/protocol invariants added:
  - The first token after `run` is interpreted only by the run algebra:
    `parse ["run", token, args...] = Error` when `token` is not one of
    `--`, `exec`, `test`, `script`, `scripts`, `help`, or `--help`.
  - Entrypoint argv crosses the boundary only after `--`:
    `parse ["run", "--", args...] = Right (run (EntryPoint args))`.
- Remaining discomfort:
  - `run` still combines "execute a Lisp entrypoint" and "execute external
    process" under one resource. The current defense is activation context:
    both are observations of the project activation, not dependency mutation.

### Iteration 20: Attack Root Help Scope Drift

- Commands deleted:
  - No executable command was deleted; the cut deletes root-help claims that
    scoped options are globally meaningful.
- Commands merged:
  - Scoped option documentation now belongs to command-specific help and the
    README's scoped-options section, not the root schema overview.
- Commands derived instead of exposed:
  - Root help is the schema projection for command constructors and true
    top-level controls. It does not derive every leaf option by unioning the
    whole parser.
- Commands that survived and why:
  - `-v`, `-h`, and `--version` survive in root help because they are
    top-level controls.
  - `--offline`, `--insecure`, `--fetch-*`, `--with-*`, `--jobs`, and
    `--lisp` survive only as scoped inputs to commands whose denotation uses
    them.
- Laws/protocol invariants added:
  - Root help does not advertise an option that `parse [option, "help"]`
    rejects as semantically inert.
- Remaining discomfort:
  - `-p/--package` is still a broad context selector. Its exact command domain
    should stay under attack.

### Iteration 21: Attack Workspace Target Scope

- Commands deleted:
  - Inert `-p/--package <member>` placements on `help`, `--version`,
    `doctor`, `skill`, `project init/new/workspace`, non-publishing registry
    operations, and `store gc`.
- Commands merged:
  - Workspace member targeting is no longer a root option in help/README. It is
    a scoped input to commands that resolve a project root.
- Commands derived instead of exposed:
  - `-p member deps search/info` now derives project registry context from the
    selected workspace member instead of falling back to unrelated ambient
    registry state.
- Commands that survived and why:
  - `-p member project package`, `deps ...`, `registry publish`, `run ...`,
    `repl ...`, and `store clean` survive because each command observes or
    mutates a project root.
- Laws/protocol invariants added:
  - No inert workspace target:
    `parse ["-p", member, "help"] = Error`.
  - Workspace target is a project-root selector:
    `parse ["-p", member, op] = Right (op with ProjectTarget member)` only
    when `op` resolves project state.
- Remaining discomfort:
  - `deps search/info` straddle global registry observation and project
    registry observation. They survive under `-p` only because selected member
    registries are now part of their denotation.

### Iteration 22: Attack Package Export Leakage

- Commands deleted:
  - Public exports of option specials (`*offline*`, `*insecure*`, `*jobs*`,
    `*lisp*`, `*target-package*`, `*with-optional*`) from `clpm.commands`.
  - Public exports of logging helpers from `clpm.commands`.
  - Public exports of root option specials from `clpm`.
- Commands merged:
  - Mutable invocation state is now internal binding state, not a public CLI
    algebra constructor.
- Commands derived instead of exposed:
  - Lower layers that need the current invocation state refer to the internal
    binding explicitly; callers do not get a second API for mutating command
    meaning.
- Commands that survived and why:
  - `clpm.commands:cmd-project`, `cmd-deps`, `cmd-registry`, `cmd-run`,
    `cmd-store`, `cmd-repl`, `cmd-skill`, `cmd-help`, and `cmd-doctor`
    survive as the resource dispatcher/observation handler boundary.
  - `clpm:main` and `clpm:run-cli` survive as the executable entry and
    in-process test/embedding entry.
- Laws/protocol invariants added:
  - Exact export schema:
    `externalSymbols("CLPM.COMMANDS") = {cmd-project, cmd-deps,
    cmd-registry, cmd-run, cmd-store, cmd-repl, cmd-skill, cmd-help,
    cmd-doctor}`.
  - Exact root package schema:
    `externalSymbols("CLPM") = {main, run-cli}`.
- Remaining discomfort:
  - Internal specials still carry invocation context dynamically. That is
    acceptable inside the implementation boundary, but it should not become
    public API again.

### Iteration 23: Attack Generated Guidance Drift

- Commands deleted:
  - README omissions that hid surviving public forms such as `run scripts`,
    `store clean --store`, `store gc --dry-run`, `deps search --json`, and
    workspace-targeted dependency observations.
  - Agent skill wording that described `-p/--package` as a global option.
- Commands merged:
  - README and `clpm skill` now describe the same scoped-option model as root
    help and command-specific help.
- Commands derived instead of exposed:
  - Documentation is a projection of the command algebra. It does not get to
    invent old names, broader option scope, or narrower command forms.
- Commands that survived and why:
  - The generated skill remains because agents need an operational checklist,
    but its command examples must remain law-abiding.
- Laws/protocol invariants added:
  - Agent guidance is not allowed to widen option scope:
    `skillText` does not contain `global -p`.
- Remaining discomfort:
  - README is still hand-maintained Markdown rather than generated from the
    command schema. Tests pin the highest-risk generated skill drift; README
    needs continued manual review after each CLI cut.

### Iteration 24: Attack Duplicate Workspace Selectors

- Commands deleted:
  - `clpm deps tree --package <member>`.
  - `clpm deps why <system> --package <member>`.
- Commands merged:
  - Workspace member selection is expressed only by the scoped pre-command
    `-p/--package <member>` option.
- Commands derived instead of exposed:
  - Dependency graph observations use the same `ProjectTarget` context as
    every other project-scoped operation. They do not get local selector
    syntax.
- Commands that survived and why:
  - `clpm -p <member> deps tree [--depth N]` and
    `clpm -p <member> deps why <system>` survive because they observe a
    selected member's lockfile graph from a workspace root.
  - `repl eval --package <name>` survives because it denotes a Common Lisp
    package override, not a workspace member selector.
- Laws/protocol invariants added:
  - Workspace target has one spelling:
    `parse ["deps", "tree", "--package", member] = Error`.
  - Lisp package overrides are not project targets:
    `parse ["repl", "eval", form, "--package", pkg] =
     Right (repl (eval form with LispPackage pkg))`.
- Remaining discomfort:
  - The shared word "package" still carries two concepts depending on
    position. The parser boundary keeps them distinct, but help must keep
    explaining the pre-command workspace selector separately from
    `repl eval --package`.

### Iteration 25: Attack Workspace REPL Directory Leakage

- Commands deleted:
  - No command tokens. This cut removes an accidental launch-context leak.
- Commands merged:
  - Foreground and detached daemon startup now have the same project image
    context: socket, pid/log files, protocol `project_root`, and pathname
    defaults all resolve to the selected project root. A standalone daemon
    process starts with its cwd at that root too.
- Commands derived instead of exposed:
  - Workspace member selection remains a `ProjectTarget`; it derives every
    daemon identity field rather than only the socket path.
- Commands that survived and why:
  - `clpm -p <member> repl daemon` survives because a workspace member can
    own a foreground daemon just like a detached daemon.
- Laws/protocol invariants added:
  - Project REPL image identity includes path context:
    `denote (repl daemon) (ProjectTarget root) world =
     startImage { socketRoot = root, projectRoot = root,
                  pathnameDefaults = root, processCwd = root }`.
- Remaining discomfort:
  - Multiple foreground daemons embedded in one host Lisp still share the
    operating-system cwd because the OS process owns that state. In-process
    tests therefore validate per-daemon Lisp pathname defaults as the useful
    isolation boundary for relative Lisp file operations.

### Iteration 26: Attack `repl call` Lifecycle Leakage

- Commands deleted:
  - Hidden `repl call --no-autostart` as a semantic option. `call` now has
    no autostart path, so the flag denoted nothing useful.
- Commands merged:
  - Daemon creation remains only in `repl daemon --detach` and the ergonomic
    `repl eval` autostart path.
- Commands derived instead of exposed:
  - `methods` and `help` remain daemon RPCs, not local schema fallbacks. They
    observe the running daemon's actual method registry.
- Commands that survived and why:
  - `repl call METHOD` survives as the generic request constructor for an
    existing project image.
- Laws/protocol invariants added:
  - `daemonAbsent root => denote (repl (call method params)) root world =
     Failed no-daemon`.
  - `repl call` must not add `root` to `world.repls`; only `repl daemon` and
    `repl eval` may do that.
- Remaining discomfort:
  - `time-eval` and `profile-eval` are still eval-shaped RPC aliases. They
    should be attacked as either `repl eval` options or deleted recipes.

### Iteration 27: Attack JSON Boolean Null

- Commands deleted:
  - No command tokens. This removes `null` as an accepted value for boolean
    RPC parameters.
- Commands merged:
  - No semantic carriers.
- Commands derived instead of exposed:
  - Optional boolean absence remains the only "not supplied" state.
- Commands that survived and why:
  - Boolean RPC parameters survive as two-valued JSON booleans: `true` and
    `false`.
  - `break_on` keeps its explicit disable spellings: `"none"`, `"nil"`, and
    `false`.
- Laws/protocol invariants added:
  - `param.type = boolean => param.value in {true,false}`.
  - `param.absent` is distinct from `param.value = null`; `null` fails schema
    decoding unless a method explicitly declares a nullable type.
- Remaining discomfort:
  - The method schema still uses string type names. A future pass should
    split them into closed type constructors rather than accepting ad hoc
    string unions like `string-or-boolean`.

## Constructors

Terminal constructors:

```haskell
doctor       :: Invocation
skill        :: Invocation
help         :: Selector -> Invocation
replDaemon   :: DaemonAction -> Invocation
```

Inductive constructors:

```haskell
project      :: ProjectOperation -> Invocation
deps         :: DependencyOperation -> Invocation
registry     :: RegistryOperation -> Invocation
run          :: RunOperation -> Invocation
store        :: StoreOperation -> Invocation
repl         :: ReplOperation -> Invocation
```

Derived constructors:

```haskell
defaultInvocation = help Root
run script name args = run (Script name args)
registry key op = registry (Key op)
registry publish args = registry (Publish args)
```

Constructor responsibility audit:

| Constructor | One semantic responsibility | Denotation law present? | Derived instead? |
| --- | --- | --- | --- |
| `project` | Construct or mutate project/workspace metadata | Yes | No |
| `deps` | Construct, realize, or observe dependency state | Yes | No |
| `registry` | Configure, update, trust, key, or publish registry state | Yes | No |
| `run` | Execute inside project activation | Yes | No |
| `store` | Clean generated and unreachable store state | Yes | No |
| `repl` | Operate on persistent project image state | Yes | No |
| old `resolve/fetch/build/install` | Pipeline prefixes | Yes | Yes, `deps sync --to ...` |
| old `keys/publish` | Registry operations | Yes | Yes, `registry key/publish` |
| old `exec/test/scripts` | Execution modes | Yes | Yes, `run ...` |
| ordinary `repl` / `run repl` | Duplicate REPL/debug protocol | Yes | No, deleted in favor of `repl`. |

## Denotation Laws

```haskell
Law: "default is help"
  denote (parse []) ctx world = denote (help Root) ctx world

Law: "sync/lock"
  denote (deps (sync Lock)) ctx world =
    resolve ctx world

Law: "sync/source"
  denote (deps (sync Source)) ctx world =
    fetch ctx (world' where Succeeded world' _ = resolve ctx world)

Law: "sync/build"
  denote (deps (sync Build)) ctx world =
    build ctx (world'' where
      Succeeded world' _ = resolve ctx world
      Succeeded world'' _ = fetch ctx world')

Law: "sync/active"
  denote (deps (sync Active)) ctx world =
    activate ctx (world''' where
      Succeeded world' _ = resolve ctx world
      Succeeded world'' _ = fetch ctx world'
      Succeeded world''' _ = build ctx world'')

Law: "update"
forall unlockSet.
  denote (deps (update unlockSet)) ctx world =
    resolveWithUnlock unlockSet ctx (refreshRegistries ctx world)

Law: "registry/key"
forall keyOp.
  denote (registry (key keyOp)) ctx world =
    operateOnRegistryKeys keyOp ctx world

Law: "registry/publish"
forall release.
  denote (registry (publish release)) ctx world =
    writeSignedRegistryRelease release ctx world

Law: "run/exec"
forall command.
  denote (run (exec command)) ctx world =
    execute command (activationEnv ctx world) world

Law: "run/test"
  denote (run test) ctx world =
    executeConfiguredTests (activationEnv ctx world) world

Law: "repl/debug uniqueness"
forall invocation.
  createsOrObservesInteractiveImageState invocation
  => invocation = repl replOperation

Law: "store/clean"
forall opts.
  denote (store (clean opts)) ctx world =
    removeProjectOutputsAndMaybeReachability opts ctx world

Law: "store/gc"
forall dryRun.
  denote (store (gc dryRun)) ctx world =
    collectUnreachableStoreEntries dryRun ctx world
```

Failure propagation law:

```haskell
Law: "pipeline short-circuit"
forall a b ctx world.
  denote a ctx world = Failed code diagnostic
  => denote (a >>> b) ctx world = Failed code diagnostic
```

## Observation Laws

```haskell
Law: "observations do not mutate resource state"
forall obs ctx world.
  obs in {help, doctor, search, info, tree, why, audit}
  => case denote obs ctx world of
       Succeeded world' _ -> world' = world
       Failed _ _         -> True

Law: "json observations are stable"
forall obs ctx world.
  jsonSupported obs
  => renderJson obs ctx world = renderJson obs ctx world

Law: "json is leaf-scoped"
  parse ["repl", "--json"] = Error
  parse ["repl", "daemon", "--status", "--json"] =
    Right (repl (daemon (status Json)))

Law: "eval has one public CLI constructor"
  parse ["repl", "call", "eval", "--form", form] = Error
  parse ["repl", "eval", form] = Right (repl (eval form))

Law: "repl call is registry-closed"
  method notin replMethodRegistry
  => denote (repl (call method params)) ctx world = Failed 1 protocolError
  param notin paramsFor(method)
  => denote (repl (call method params)) ctx world = Failed 1 protocolError
  type(params[param]) /= typeOf(method.param)
  => denote (repl (call method params)) ctx world = Failed 1 protocolError

Law: "insecure is verifier-scoped"
  parse ["--insecure", "help"] = Error
  parse ["repl", "--insecure"] = Error
  parse ["--insecure", "deps", "sync", "--to", "lock"] =
    Right (deps (sync Lock) with IntegrityOverride)

Law: "offline is cache-scoped"
  parse ["--offline", "help"] = Error
  parse ["repl", "--offline"] = Error
  parse ["--offline", "deps", "sync", "--to", "lock"] = Error
  parse ["--offline", "deps", "sync", "--to", "source"] =
    Right (deps (sync Source) with OfflineCacheOnly)

Law: "jobs are realization-scoped"
  parse ["--jobs", n, "help"] = Error
  parse ["--jobs", n, "deps", "sync", "--to", "lock"] = Error
  parse ["--jobs", n, "deps", "sync", "--to", "build"] =
    Right (deps (sync Build) with ParallelJobs n)

Law: "lisp selection is process-constructor-scoped"
  parse ["--lisp", impl, "help"] = Error
  parse ["--lisp", impl, "repl"] = Error
  parse ["--lisp", impl, "deps", "sync", "--to", "source"] = Error
  parse ["--lisp", impl, "project", "package"] =
    Right (project package with LispImplementation impl)

Law: "optional dependency flags are solve-scoped"
  parse ["--with-optional", sys, "help"] = Error
  parse ["--with-all-optional", "repl"] = Error
  parse ["--with-optional", sys, "deps", "sync", "--to", "lock"] =
    Right (deps (sync Lock) with OptionalOptIns {sys})

Law: "fetch tuning is fetch-scoped"
  parse ["--fetch-retries", n, "help"] = Error
  parse ["--fetch-timeout", n, "repl"] = Error
  parse ["--fetch-retries", n, "deps", "search", query] =
    Right (deps (search query) with FetchBudget n)
  parse ["--fetch-timeout", n, "registry", "update"] =
    Right (registry update with FetchTimeout n)

Law: "trust values are kind-typed and non-clearing"
  parse ["registry", "trust", "set", gitName, "ed25519:key"] =
    Right (registry (trustSet gitName (GitTrust (Ed25519 "key"))))
  parse ["registry", "trust", "set", quicklispName, "tofu"] =
    Right (registry (trustSet quicklispName QuicklispTofu))
  parse ["registry", "trust", "set", name, "none"] = Error
  parse ["registry", "trust", "set", name, "nil"] = Error

Law: "trust refresh is trust-scoped"
  parse ["registry", "update", "--refresh-trust", name] = Error
  parse ["registry", "trust", "refresh", quicklispName] =
    Right (registry (trustRefresh quicklispName))

Law: "publish has no VCS side effects"
  parse ["registry", "publish", args..., "--git-commit"] = Error
  denote (registry (publish args)) ctx world =
    writeSignedRegistryArtifacts args ctx world

Law: "eval has no default-output alias"
  parse ["repl", "eval", form, "--pretty"] = Error
  parse ["repl", "eval", form] = Right (repl (eval form Human))
  parse ["repl", "eval", form, "--json"] = Right (repl (eval form Json))

Law: "run entrypoint args require an explicit boundary"
  parse ["run"] = Right (run (EntryPoint []))
  parse ["run", "--", args...] = Right (run (EntryPoint args))
  token notin {"--", "exec", "test", "script", "scripts", "help", "--help"}
    => parse ["run", token, args...] = Error

Law: "help is schema projection"
forall selector ctx world.
  denote (help selector) ctx world =
    Succeeded world (HumanText (render selector commandSchema))

Law: "root help mentions only root controls"
forall scoped.
  scoped in {Offline, Insecure, FetchRetries, FetchTimeout,
             WithOptional, WithAllOptional, Jobs, Lisp, PackageTarget}
  => render Root commandSchema does not mention scoped
```

## Algebraic and Interaction Laws

```haskell
Law: "workspace members form a sorted set"
forall ws member.
  project (workspace add member) (project (workspace add member) ws)
  = project (workspace add member) ws

Law: "dependency add is last-writer-wins by section and system"
forall section dep constraint1 constraint2.
  deps (add section dep constraint2)
    (deps (add section dep constraint1) project)
  = deps (add section dep constraint2) project

Law: "dependency remove is inverse for absent dependency intent"
forall section dep project.
  dep notin section project
  => deps (remove section dep) project = Failed 1 diagnostic

Law: "store gc dry-run is observational"
forall ctx world.
  denote (store (gc DryRun)) ctx world =
    Succeeded world (HumanText deletedSet)

Law: "repl cleanup leaves no kept operational state"
forall image.
  cleanup image =
    image { debugSessions = empty
          , watches = empty
          , traces = empty
          , throwawayWorkers = empty
           }
```

Conditions moved into types:

- `SyncStage = Lock | Source | Build | Active`.
- `RunOperation = EntryPoint Args | Exec Command | Test | Script Name Args
  | Scripts`.
- `RegistryOperation` contains `KeyOperation` and `PublishOperation`; they
  are not top-level commands.

## Interface Morphism Checks

Claimed project-local interface: parser/rendered help schema.

```haskell
Law: "parse/help schema"
forall invocation selector.
  parse invocation = Right command
  => help selector commandSchema mentions command iff command is public

Law: "package/export schema"
  exportedCommandHandlers(clpm.commands)
  = { cmd-project, cmd-deps, cmd-registry, cmd-run, cmd-store, cmd-repl,
      cmd-skill, cmd-help, cmd-doctor }

Law: "root/export schema"
  exportedSymbols(clpm) = { main, run-cli }

Law: "skill guidance does not widen option scope"
  "global `-p" notin skillText
  "scoped `-p <member>`" in skillText
```

Rejected instances:

| Instance | Smallest counterexample | Left denotation | Right model method | Design response |
| --- | --- | --- | --- | --- |
| Flat top-level command monoid | `resolve` and `install` | Both operate on same realization pipeline at different prefixes | No single resource identity | Collapse to `deps sync --to ...`. |
| `keys` as top-level resource | `keys import` | Mutates trust key material | Not independent of registry trust | Move to `registry key`. |
| `scripts` as project metadata operation | `scripts run fmt` | Executes command in activation | Not a manifest constructor | Move execution to `run script`. |
| ordinary REPL under `run` | `run repl` | Creates interactive image/debugger state | Same carrier as persistent repl state | Delete; use `repl`. |
| `repl` under `run` | `repl daemon --status` | Observes/cleans daemon lifecycle state | Not one-shot execution | Keep top-level carrier. |

## Quality Gate

| Criterion | Score | Evidence |
| --- | --- | --- |
| Denotational fit | 4 | Every current public operation maps to a resource operation or a derived pipeline/execution form. |
| Simplicity | 4 | Top-level surface is reduced to semantic carriers. |
| Compositionality | 4 | Pipeline, execution, registry-key, and cleanup laws define compound behavior. |
| Semantic equality and abstraction safety | 3 | Equality is observational; log wording remains non-semantic except tested help/JSON. |
| Closure | 4 | Invalid stages, trust forms, targets, and method names fail as outcomes. |
| Power | 4 | Existing workflows remain expressible under the target algebra. |
| Parsimony | 4 | Accidental top-level wrappers are removed rather than aliased. |
| Orthogonality | 4 | Project, deps, registry, run, store, and repl have separate carriers. |
| Law quality | 3 | Pipeline and grouping laws are precise; some lower-level effects remain implementation-defined. |
| Interface morphisms | 3 | Help schema preservation is explicit; no external typeclass instances are claimed. |
| Generality | 4 | Resource grouping is independent of current Lisp function names. |
| Implementation independence | 4 | Spec survives alternate command implementation or storage layout. |

Gate result:

- Pass for implementing the target surface above.

Repairs required before implementation:

1. Update parser, dispatcher, usage, help, README, and agent skill output to
   expose only the target public surface.
2. Update focused command tests to use the new surface.
3. Add coverage that removed top-level wrappers are no longer public.
4. Add coverage that removed leaf handlers are not exported from
   `clpm.commands`.

## Reference Implementation Derivation

Reference representation:

- Keep existing internal command functions as implementation operations where
  they already denote a target operation.
- Add resource dispatchers for `project`, `deps`, `run`, and `store`.
- Extend `registry` to own `key` and `publish`.
- Remove old top-level dispatch cases, top-level usage rows, README rows, and
  agent skill examples.

Primitive constructors:

- `cmd-project`
- `cmd-deps`
- `cmd-registry`
- `cmd-run` as the execution resource dispatcher
- `cmd-store`
- `cmd-repl`
- `cmd-skill`
- `cmd-help`
- `cmd-doctor`

Derived constructors:

- `deps sync --to lock/source/build/active`
- `registry key ...`
- `registry publish ...`
- `run exec/test/script/scripts`

Observation implementation:

- Preserve existing JSON and human rendering for observations after the
  command path is changed.

Transported instances through `denote`/`reify`:

- Existing internal functions may remain if neither the public parser nor
  package exports expose their old top-level names.
- No compatibility aliases are added.

## Property and Command Tests

Generators:

- Small command vectors for public and removed command paths.

Denotation properties:

- Bare `clpm` and `clpm help` return the same safe schema observation.
- `deps sync --to lock` writes/refreshes `clpm.lock` and does not require
  activation.
- `store gc --dry-run` does not mutate store reachability.

Observation properties:

- `help` only advertises public commands.
- Root `help` advertises only top-level controls; scoped dependency,
  registry, fetch, and Lisp-selection options live on command-specific help
  and README scoped-option documentation.
- `deps search/info/audit/sbom --json` remain stable.

Failed-counterexample regressions:

- `clpm add`, `clpm install`, `clpm keys`, `clpm publish`, `clpm test`, and
  `clpm gc` are unknown top-level commands after the refinement.
- `clpm run repl` and `clpm help run repl` reject the obsolete ordinary REPL
  surface and point at `clpm repl`.
- `clpm.commands:cmd-install`, `clpm.commands:cmd-keys`,
  `clpm.commands:cmd-gc`, and other leaf handlers are not external symbols.
- `clpm repl --json` is rejected; `--json` is not a resource-level output
  mode.
- `clpm --insecure help` and `clpm repl --insecure` are rejected;
  `--insecure` is not an inert global decoration.
- `clpm repl call eval --form FORM` is rejected; public evaluation goes
  through `clpm repl eval FORM`.
- `clpm --offline help`, `clpm repl --offline`, and
  `clpm --offline deps sync --to lock` are rejected; `--offline` is only for
  artifact/cache operations.
- `clpm --jobs 4 help`, `clpm -j 4 repl`, and
  `clpm --jobs 2 deps sync --to lock` are rejected; `--jobs` is only for
  parallel dependency realization.
- `clpm --lisp sbcl help`, `clpm --lisp sbcl repl`, and
  `clpm --lisp sbcl deps sync --to source` are rejected; `--lisp` is only for
  CLPM-owned Lisp process construction.
- `clpm --package app help`, `clpm --package app doctor`, and
  `clpm --package app store gc` are rejected; workspace member targeting is
  only for commands that resolve project state.
- `clpm --with-optional foo help` and
  `clpm --with-all-optional repl` are rejected; optional opt-ins are only for
  dependency solving.
- `clpm --fetch-retries 2 help` and `clpm --fetch-timeout 3 repl` are
  rejected; fetch tuning is only for CLPM-managed fetch operations.
- `clpm registry trust set main none` and
  `clpm registry trust set main nil` are rejected; permanent trust clearing
  is not a CLI trust value.
- `clpm registry update --refresh-trust quicklisp` is rejected; Quicklisp
  pin refresh is `clpm registry trust refresh quicklisp`.
- `clpm registry publish --git-commit ...` is rejected; publish does not run
  VCS commands.
- `clpm repl eval FORM --pretty` is rejected; human output is the default and
  has no flag alias.
- `clpm run bare args` is rejected; entrypoint arguments require
  `clpm run -- bare args`.

Reference versus optimized equivalence:

- Existing command-specific tests are rewritten to the target paths and must
  continue to pass.

## Optimization Direction

Optimized representation:

- The implementation may later collapse duplicated parsing code behind the
  resource dispatchers, but only after tests demonstrate identical denotation.

Law-backed normalizations:

- Sort workspace members, dependency lists, registry trust listings, search
  results, and SBOM components.

Indexes/caches that remain unobservable:

- Registry indexes, source fetch cache layout, ASDF output paths, and repl
  transport details.

Performance risks:

- `deps sync --to source/build` now denotes a prefix from project intent. Avoid
  redundant solver or fetch work where existing lock hashes prove inputs are
  unchanged.

## Open Semantic Decisions

No open semantic decisions block this implementation. Naming, grouping, and
derived-command expansion are fixed by the target surface above.

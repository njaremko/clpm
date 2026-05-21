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
4. Kept repl debugger sessions, watches, traces, worker histories, and
   throwaway workers are observable repl state and must be explicitly cleaned
   up or scoped to their owning project image.

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
data ReplWorkerName
data ReplHistory
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

data ReplImage =
  ReplImage { workers :: Map ReplWorkerName ReplWorkerState
            , traces  :: Set TraceSpec
            }

data ReplWorkerState =
  ReplWorkerState { package :: PackageName
                  , history :: ReplHistory
                  , redefinitions :: Set Redefinition
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
clpm [options] registry add --quicklisp [--name quicklisp] [--url <dist-url>] [--trust tofu|sha256:<64-hex-digest>]
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
clpm [options] repl eval FORM [--package P] [--worker W] [--no-autostart] [--json]
clpm [options] repl eval FORM [--package P] [--worker W] [--no-autostart] --debug [debug-options]
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
  - `repl call methods` survives because callable schema discovery is an
    observation over the public daemon call surface.
  - Non-eval callable protocol methods survive under `call` because they operate on
    persistent image/debugger/inspector/watch/trace state that has no smaller
    stable CLI constructor yet.
- Laws/protocol invariants added:
  - Eval has exactly one public CLI constructor:
    `parse ["repl", "call", "eval", "--form", form] = Error`.
  - `eval` is not a callable discovery entry:
    `parse ["repl", "call", "help", "--method", "eval"] =
     Failed unknown-method`.
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
  - None for eval output mode. Later iterations moved leaf-scoped option
    documentation out of root help.

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
  - Eval-shaped RPC aliases should be deleted rather than repaired outside
    the worker eval algebra.

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

### Iteration 28: Delete Eval-Shaped Timing/Profile RPCs

- Commands deleted:
  - `repl call time-eval`.
  - `repl call profile-eval`.
- Commands merged:
  - Timing and profiling forms are ordinary project-image evaluation work.
    They belong under `repl eval` or explicit Lisp forms inside the image,
    not as separate RPC constructors.
- Commands derived instead of exposed:
  - Timing can be derived from `repl eval` output and Common Lisp/SBCL timing
    forms. Profiling can be run explicitly in the image with the profiler the
    project chooses.
- Commands that survived and why:
  - `trace`, `untrace`, and `list-traced` survive because they mutate or
    observe trace state without evaluating an arbitrary form through a second
    eval path.
- Laws/protocol invariants added:
  - Every public arbitrary-form evaluation must route through `repl eval`.
  - `method in {"time-eval","profile-eval"} => Failed unknown-method`.
- Remaining discomfort:
  - Trace state must remain daemon-local even in embedded hosts that run more
    than one project daemon in one Lisp image.

### Iteration 29: Delete Duplicate Debug Abort Flag

- Commands deleted:
  - `repl eval FORM --debug --abort`.
- Commands merged:
  - Plain `repl eval FORM --debug` already observes the first debugger stop
    and aborts it when no restart, frame action, or keep request is supplied.
- Commands derived instead of exposed:
  - Abort is the default terminal action of the debug observation path, not a
    separate CLI constructor.
- Commands that survived and why:
  - `--keep` survives because it changes the denotation: the debugger stop is
    retained as server-owned session state for later `repl call debug-*`
    operations.
  - `--restart`, `--frame`, and `--frame-eval` survive because each selects a
    different continuation action at the observed stop.
- Laws/protocol invariants added:
  - `debug(form)` with no selected continuation denotes `debugStop(form)`
    followed by `debugAbort`.
  - `debug(form, keep)` denotes `debugStop(form)` plus retained session state;
    it must not be equivalent to abort.
- Remaining discomfort:
  - The debug option parser still accumulates loosely related selectors in a
    plist. A future pass should split the selected continuation into a closed
    state before dispatch.

### Iteration 30: Attack Public Transport Parameters

- Commands deleted:
  - `repl call METHOD --project-root VALUE`.
  - `repl call METHOD --token VALUE`.
  - The same fields supplied through `--params-json`.
- Commands merged:
  - No user-facing operation is lost. The selected project root is already the
    resource identity of `clpm repl`, and TCP tokens are transport
    credentials read by the client.
- Commands derived instead of exposed:
  - `project_root` is derived from the effective project before the request is
    sent.
  - `token` is derived from the daemon port file for TCP transports.
- Commands that survived and why:
  - Method-local params still survive because they are checked by the daemon
    method schema.
  - `explain` remains a protocol observation knob; it emits a plan event and
    does not select or authenticate a different daemon.
- Laws/protocol invariants added:
  - `repl call` public params exclude transport fields.
  - `denote(repl call method params)` is independent of user-supplied
    transport credentials because such params fail before dispatch.
- Remaining discomfort:
  - The raw protocol still has implicit dispatch fields. That is appropriate
    for protocol framing, but the boundary should stay narrow and documented.

### Iteration 31: Attack Open Help Selectors

- Commands deleted:
  - Invalid selector fallbacks like `help deps bogus`.
  - Overlong selector fallbacks like `help registry trust set extra`.
- Commands merged:
  - No valid help pages were removed. Umbrella pages remain only for exact
    selectors such as `help deps`, `help registry trust`, and
    `help project workspace`.
- Commands derived instead of exposed:
  - Resource-local `--help` still derives the same closed selector as
    `clpm help ...`.
  - Exact resource-root `COMMAND help` aliases for `project`, `deps`,
    `registry`, `run`, and `store` derive the same selector as
    `clpm help COMMAND`; longer `COMMAND help ...` forms are not extra help
    grammar.
- Commands that survived and why:
  - `help run repl` survives as an error page because the help schema
    deliberately points old ordinary-REPL users to `clpm repl`.
- Laws/protocol invariants added:
  - `selector notin helpSchema => parse ["help", selector...] = Error`.
  - Help rendering is a total function only on the finite command-schema
    selector set.
- Remaining discomfort:
  - Resource-local help aliases remain as derived syntax. They are useful, but
    the canonical schema projection is still `clpm help ...`.

### Iteration 32: Delete Protocol-Only `repl call` Methods

- Commands deleted:
  - `repl call shutdown`.
  - `repl call query-response`.
- Commands merged:
  - Daemon shutdown is merged into the resource lifecycle constructor
    `repl daemon --stop`.
  - `query-response` remains only as a continuation frame on an existing
    in-flight evaluation, not as a shell command.
- Commands derived instead of exposed:
  - Protocol clients may still send raw `shutdown` and `query-response` frames
    where the protocol requires them. The CLI `call` projection is narrower
    than the wire protocol.
- Commands that survived and why:
  - `debug-*` methods survive because kept debugger sessions need explicit
    operator actions after `repl eval --debug --keep`.
- Laws/protocol invariants added:
  - `repl call` excludes lifecycle aliases already represented by
    `repl daemon`.
  - `repl call` excludes continuation reply frames that have meaning only
    inside an active request.
- Remaining discomfort:
  - Discovery still needs a separate pass to stop advertising protocol-only
    methods. Iteration 33 closes that leak.

### Iteration 33: Close REPL Discovery Over Protocol Internals

- Commands deleted:
  - `repl call methods` no longer reports `shutdown` or `query-response`.
  - `repl call help --method shutdown`.
  - `repl call help --method query-response`.
- Commands merged:
  - Shutdown documentation belongs to `help repl daemon`.
  - Query-response remains documented only as protocol behavior in lower-level
    bridge docs and tests, not as a public `call` method.
- Commands derived instead of exposed:
  - Raw protocol clients can still use `shutdown` and `query-response` where
    required. They are registered for dispatch/schema validation, but not
    projected through public discovery.
- Commands that survived and why:
  - Callable method help survives only for methods that `repl call METHOD`
    may actually construct.
- Laws/protocol invariants added:
  - `methods = discoverable(methodRegistry)`, not `methodRegistry`.
  - `method in undiscoverableMethods => help(method) = Failed unknown-method`.
- Remaining discomfort:
  - None for protocol-only lifecycle and continuation methods. Iteration 39
    closes the remaining `eval` discovery leak.

### Iteration 34: Reject Inert Daemon Action Flags

- Commands deleted:
  - `repl daemon --status --detach`.
  - `repl daemon --status --no-load`.
  - `repl daemon --stop --detach`.
  - `repl daemon --stop --no-load`.
- Commands merged:
  - No lifecycle action was removed. `--detach` and `--no-load` remain
    start-only modifiers for the bare daemon start action.
- Commands derived instead of exposed:
  - Status and stop are terminal daemon actions; they do not derive from
    start options.
- Commands that survived and why:
  - `repl daemon --status --json` survives because `--json` changes the
    observation format of the status action.
- Laws/protocol invariants added:
  - `daemonAction in {status, stop} => startOptions = empty`.
  - Inert start options on non-start daemon actions fail before project state
    is observed or mutated.
- Remaining discomfort:
  - `registry add --quicklisp --trust` was accepted by the parser but hidden
    from help, leaving a useful trust constructor in a half-public state.

### Iteration 35: Defend `repl eval --no-autostart`

- Commands deleted:
  - None. The option survives the attack.
- Commands merged:
  - No lifecycle action was merged into eval. The option is a precondition on
    eval, not a daemon lifecycle constructor.
- Commands derived instead of exposed:
  - `repl eval FORM --no-autostart` is `repl eval FORM` with the daemon
    creation transition removed from the admissible world transitions.
- Commands that survived and why:
  - `repl eval --no-autostart` survives because it is the only ergonomic way
    for scripts and tests to say "talk to the selected existing project image
    or fail." Without it, `repl eval` can silently create a new daemon and
    destroy the observation needed to prove project isolation.
- Laws/protocol invariants added:
  - If no daemon exists for the selected project root,
    `denote(repl eval form --no-autostart) = Failed no-daemon`.
  - `repl eval --no-autostart` never adds an entry to `world.repls`.
  - If a daemon exists, `repl eval FORM --no-autostart` and
    `repl eval FORM` observe the same selected daemon.
- Remaining discomfort:
  - None. The option is now defended as an eval precondition modifier, not as
    public lifecycle policy.

### Iteration 36: Expose Quicklisp Add Trust

- Commands deleted:
  - No command token. The hidden parser surface becomes documented instead of
    accidental.
- Commands merged:
  - `registry trust set quicklisp tofu` is not needed after
    `registry add --quicklisp --trust tofu`; add-time trust is the constructor
    for the initial registry config.
- Commands derived instead of exposed:
  - `registry add --quicklisp` derives to
    `registry add --quicklisp --trust tofu` with the default Quicklisp dist
    URL and name.
- Commands that survived and why:
  - `registry add --quicklisp --trust tofu|sha256:<digest>` survives because
    trust is part of the Quicklisp registry value, not a later optional
    decoration. Add-time `sha256:` lets a known distinfo pin be installed
    without a temporary TOFU state.
- Laws/protocol invariants added:
  - Quicklisp add trust domain is closed:
    `trust in {tofu} union sha256Digest`.
  - Git registry add trust domain is disjoint:
    `trust = ed25519:keyId`.
  - Help output must advertise every accepted add-time registry trust domain.
- Remaining discomfort:
  - None for registry add trust. Future attacks should check whether README
    examples reintroduce redundant `trust set quicklisp tofu` flows.

### Iteration 37: Bind Daemon Lifecycle to Project Identity

- Commands deleted:
  - No command token. The cut removes a false observation: a pid/socket pair
    under one project no longer proves that project's daemon exists.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - `repl daemon --status` and `repl daemon --stop` derive daemon identity
    from the selected project root, then validate the endpoint by `ping`
    before reporting or stopping it.
- Commands that survived and why:
  - `repl daemon --status` survives as lifecycle observation, but only when
    the endpoint authenticates and accepts the selected canonical
    `project_root`.
  - `repl daemon --stop` survives as lifecycle mutation, but it refuses to
    signal a pid unless the endpoint first proves it belongs to the selected
    project image.
- Laws/protocol invariants added:
  - `status(root)` may report running only if
    `authenticatedPing(endpoint(root), root)` succeeds.
  - `stop(root)` may send shutdown only after the same identity proof.
  - If pid/socket files point at another project's daemon, status and stop
    clean the selected project's stale lifecycle files and do not mutate the
    other daemon or its process.
- Remaining discomfort:
  - Trace state still needs an isolation pass.

### Iteration 38: Make Trace State Server-Local

- Commands deleted:
  - No command token. The cut removes delegation to process-global
    implementation trace state.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - `repl call trace`, `untrace`, and `list-traced` now operate on the
    selected daemon's trace set, not the host Lisp's global trace list.
- Commands that survived and why:
  - `trace` survives because a debugger can need call observation without
    evaluating a second arbitrary form.
  - `list-traced` survives because daemon-local trace state is observable REPL
    state, just like watches and debugger sessions.
- Laws/protocol invariants added:
  - `trace(rootA, sym)` does not add `sym` to `traces(rootB)`.
  - `list-traced(root)` returns only traces owned by `root`'s daemon.
  - `eval(rootB, call(sym))` does not emit trace output from
    `trace(rootA, sym)`.
  - Shutting down a daemon removes that daemon's trace registrations without
    untracing symbols still registered by another daemon.
- Remaining discomfort:
  - None for REPL trace isolation. The trace wrapper is intentionally simpler
    than implementation-native tracing; it records calls, not implementation
    internals.

### Iteration 39: Hide Raw Eval From Call Discovery

- Commands deleted:
  - `repl call methods` no longer reports `eval`.
  - `repl call help --method eval`.
- Commands merged:
  - Eval schema documentation moves to `clpm repl eval` CLI help and the
    raw protocol tests. The public call constructor no longer advertises a
    method it rejects.
- Commands derived instead of exposed:
  - The daemon still dispatches raw `eval` frames for the `clpm repl eval`
    client, but callable discovery denotes only the subset constructible by
    `repl call METHOD`.
- Commands that survived and why:
  - `repl call methods` survives as callable-method discovery.
  - `repl call help --method METHOD` survives for callable methods such as
    `gc`, `watch`, `trace`, `inspect`, and debugger session actions.
- Laws/protocol invariants added:
  - `eval notin callableMethodRegistry`.
  - `methods = discoverable(callableMethodRegistry)`.
  - `help(eval) = Failed unknown-method`.
  - Raw protocol dispatch still satisfies
    `dispatch(eval, params) = replEval(params)`.
- Remaining discomfort:
  - None for `eval` discovery. Iteration 41 closes the schema type-name
    problem.

### Iteration 40: Pin README to Callable REPL Methods

- Commands deleted:
  - Stale README names `find-definitions` and `who-calls`.
- Commands merged:
  - Source navigation prose now names the callable method constructors:
    `call find-definition` and `call xref`.
- Commands derived instead of exposed:
  - No runtime command. The documentation observation is now checked by
    `test/readme-docs-test.lisp` instead of trusted by review memory.
- Commands that survived and why:
  - README remains a human overview, but it must not introduce method names
    outside the callable registry.
- Laws/protocol invariants added:
  - `README_REPL_METHODS subset callableMethodRegistry`.
  - Removed discovery examples such as `help --method eval` do not reappear
    in README prose.
- Remaining discomfort:
  - README is still hand-maintained Markdown; this adds a targeted guard for
    stale REPL method names, not a generator.

### Iteration 41: Close Method Parameter Type Algebra

- Commands deleted:
  - No command token. The cut removes open string type names from the daemon's
    internal method schema.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - JSON schema strings are now derived at the wire boundary from closed
    keyword variants such as `:string`, `:boolean`, and
    `:string-or-boolean`.
- Commands that survived and why:
  - `repl call methods` and `repl call help --method METHOD` still render
    string type names because JSON clients need ordinary data, not Lisp
    keywords.
- Laws/protocol invariants added:
  - `paramType(method, param) in MethodParamType`.
  - `renderParamType : MethodParamType -> JsonString`.
  - Unknown schema type variants are impossible in registered method specs and
    are guarded by `test/repl-methods-test.lisp`.
- Remaining discomfort:
  - None for method parameter type closure. The parser still accumulates
    debug eval options in a loose plist; that is a separate eval-option
    algebra problem.

### Iteration 42: Make REPL History Worker-Local

- Commands deleted:
  - No command token. The cut deletes process-global CL history mutation as
    the meaning of `repl eval`.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - The CL dynamic bindings `*`, `**`, `***`, `+`, `++`, `+++`, `/`, `//`,
    and `///` are now a view of the selected worker's `ReplHistory` while the
    form is evaluated.
- Commands that survived and why:
  - Named workers survive because they denote independent REPL sessions inside
    one project daemon.
  - The history payload in `repl eval` survives because it is a useful
    observation of the selected worker's session state.
- Laws/protocol invariants added:
  - `eval(workerA, x); eval(workerB, y); eval(workerA, *) = x`.
  - `history(eval(worker, form)) = worker.history after form`.
  - No eval in one worker mutates another worker's package, history, or
    redefinition log.
  - Project daemon isolation is stronger than socket identity: one host Lisp
    process may own at most one active project daemon, so process-global CL
    state cannot become an accidental inter-project bus.
- Remaining discomfort:
  - None for REPL history isolation. The remaining broad attack is the loose
    eval options plist named in Iteration 41.

### Iteration 43: Reject Unknown Pre-Command Options

- Commands deleted:
  - `clpm --json` as a silent alias for bare help.
  - Any unknown pre-command flag as an invisible command argument that falls
    through to bare help.
- Commands merged:
  - None. Unknown options have no denotation.
- Commands derived instead of exposed:
  - Command-local flags remain parsed by their owning command after the command
    token is selected.
- Commands that survived and why:
  - The closed global option set survives: `-v`, `--verbose`, `-h`, `--help`,
    `--version`, and the explicitly scoped pre-command knobs already defended
    by the option-scope laws.
- Laws/protocol invariants added:
  - `flag notin GlobalOption => parse([flag]) = Error`.
  - `parse(["--json"]) = Error`; JSON is only a leaf observation mode where a
    command explicitly documents it.
  - Unknown leading flags cannot be observationally equal to bare `clpm`.
- Remaining discomfort:
  - None for pre-command option closure.

### Iteration 44: Make Terminal Root Options Terminal

- Commands deleted:
  - `clpm --version EXTRA...` as a silent alias for `clpm --version`.
  - `clpm --help SELECTOR...` as a silent alias for root help.
  - `clpm COMMAND --version` as a process-wide version escape after a command
    token has already been selected.
- Commands merged:
  - Selector help remains `clpm help SELECTOR...`.
  - Resource-local `COMMAND ... --help` remains derived syntax for the exact
    selector before `--help`; trailing tokens after `--help` have no meaning.
- Commands derived instead of exposed:
  - None. Terminal root options are observations, not constructors that accept
    residual argv.
- Commands that survived and why:
  - `clpm --version` survives as the root version observation.
  - `clpm --help` and `clpm -h` survive as exact aliases of bare help.
- Laws/protocol invariants added:
  - `parse(["--version"]) = Version`.
  - `xs /= [] => parse(["--version"] ++ xs) = Error`.
  - `xs /= [] => parse(["--help"] ++ xs) = Error`.
  - `parse(command ++ ["--version"])` is delegated to the command parser, not
    intercepted as root `Version`.
- Remaining discomfort:
  - Resolved by Iteration 45.

### Iteration 45: Make Global Options Pre-Command Only

- Commands deleted:
  - `clpm help -v` as a silent verbose root-help observation.
  - `clpm deps sync --offline`, `clpm deps sync --jobs 4`, and
    `clpm run --lisp sbcl` as root-option forms after the command token.
  - `clpm repl --insecure` and `clpm repl --offline` as post-command global
    flags that reach option-scope validation.
- Commands merged:
  - None. This is a parser ownership law, not a new command surface.
- Commands derived instead of exposed:
  - `COMMAND ... --help` remains the only post-command root-derived syntax,
    and it is terminal selector sugar for `clpm help COMMAND ...`.
- Commands that survived and why:
  - Pre-command global options survive because they modify the invocation
    context before the resource constructor is selected.
  - Command-local flags survive inside their owning command parsers.
- Laws/protocol invariants added:
  - `parse(globalOptions ++ [command] ++ args)` may produce root options.
  - `parse([command] ++ argsWithGlobalLookingToken)` delegates every token in
    `argsWithGlobalLookingToken` to the command parser except terminal
    `--help`.
  - `globalOptionPosition(opt, argv) = PrefixOnly` for every non-terminal
    global option.
  - `--help` after a command is resource-local selector syntax and must be the
    final token.
- Remaining discomfort:
  - Resolved by Iteration 46.

### Iteration 46: Document Prefix-Only Scoped Options

- Commands deleted:
  - No command tokens. The deleted behavior is ambiguous documentation that
    left scoped root options looking like command-local flags.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - None.
- Commands that survived and why:
  - Prefix scoped options survive as invocation-context constructors.
  - Command-local options survive as resource-operation parameters documented
    under their owning command.
- Laws/protocol invariants added:
  - Root help, README, and generated `clpm skill` output all state the same
    placement rule: scoped options must appear before the command token.
  - No docs may imply that `clpm deps sync --offline` is equivalent to
    `clpm --offline deps sync`.
- Remaining discomfort:
  - None for prefix-only option documentation.

### Iteration 47: Document Repl Eval Debug Selectors

- Commands deleted:
  - No command tokens. The deleted behavior is hidden accepted syntax on
    `clpm help repl eval`.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - None.
- Commands that survived and why:
  - `repl eval FORM --debug --break-on TYPE` survives because it changes which
    conditions enter the continuation-aware debugger.
  - `repl eval FORM --debug --timeout-ms N` survives because it bounds one eval
    request and reports resource exhaustion instead of leaving a runaway image
    task unbounded.
- Laws/protocol invariants added:
  - Every accepted `repl eval` debug selector with user-facing denotation is
    named by `clpm help repl eval`.
  - Hidden eval flags are parser bugs unless they are transport-private and
    rejected before command dispatch.
- Remaining discomfort:
  - Resolved by Iteration 48.

### Iteration 48: Close Repl Eval Debug Continuation Selection

- Commands deleted:
  - `repl eval FORM --debug --frame N` without `--frame-eval`.
  - `repl eval FORM --debug --frame-eval FORM2` without `--frame`.
  - `repl eval FORM --debug --arg ARG` without `--restart`.
  - Any `repl eval FORM --debug` invocation that selects more than one
    continuation action among `--keep`, `--restart`, and
    `--frame/--frame-eval`.
- Commands merged:
  - None. The surviving actions are distinct semantic continuations.
- Commands derived instead of exposed:
  - Plain `--debug` still derives "show the first stop, then abort" when no
    explicit continuation action is selected.
- Commands that survived and why:
  - `--keep` survives because it persists the debugger stop for later
    `debug-*` calls.
  - `--restart NAME [--arg FORM]...` survives because it invokes a named
    restart at the stop.
  - `--frame N --frame-eval FORM` survives because it observes one frame-local
    expression and then aborts the stop.
- Laws/protocol invariants added:
  - `DebugAction = AbortDefault | Keep | Restart name args | FrameEval n form`.
  - `--arg` is meaningful only inside `Restart`.
  - `--frame` and `--frame-eval` are a product constructor; neither component
    is valid alone.
  - `DebugAction` is single-valued: invocations selecting two actions fail at
    parse time and do not contact the daemon.
- Remaining discomfort:
  - The implementation still represents the closed action as validated locals
    rather than an explicit struct. The public algebra is closed; a future
    internal cleanup can reify the type.

### Iteration 49: Delete Duplicate SBOM Output Spelling

- Commands deleted:
  - `deps sbom --output PATH`.
- Commands merged:
  - The duplicate file-output spelling is merged into `deps sbom --out PATH`.
- Commands derived instead of exposed:
  - None. `--out` is the single file observation constructor.
- Commands that survived and why:
  - `deps sbom --out PATH` survives because SBOMs are often consumed as files
    by scanners and release tooling.
  - Omitting `--out` survives as the stdout observation.
- Laws/protocol invariants added:
  - `parse ["deps", "sbom", "--format", fmt, "--output", path] = Error`.
  - The usage text for `deps sbom` advertises only `--out`.
  - `--out` remains unique to SBOM file output; it is not an alias family.
- Remaining discomfort:
  - None for SBOM output spelling.

### Iteration 50: Treat Foreign REPL Endpoints as Absent

- Commands deleted:
  - No command tokens. The cut removes the observable state where one
    project's `repl call` or `repl eval` can address a socket that belongs to
    another project image.
- Commands merged:
  - A daemon endpoint whose `project_root` differs from the selected project
    is merged into the existing "no daemon for this project" outcome.
- Commands derived instead of exposed:
  - Stale lifecycle cleanup remains derived from project identity validation;
    users should not need a separate repair command before evaluating.
- Commands that survived and why:
  - `repl daemon --status` and `--stop` still clean stale lifecycle files.
  - `repl eval` survives as the autostarting path: after deleting a foreign
    endpoint it starts the selected project's daemon.
  - `repl call` survives as non-autostarting RPC and fails as no-daemon when
    the only reachable endpoint is foreign.
- Laws/protocol invariants added:
  - `endpoint.projectRoot != selectedProjectRoot => endpoint = Absent`.
  - `replCall selectedProjectRoot method` must never observe another
    project's daemon result.
  - `replEval selectedProjectRoot form --no-autostart` fails with no-daemon
    when the endpoint belongs to a different project.
  - `replEval selectedProjectRoot form` may clean a foreign endpoint and
    autostart only the selected project's image.
- Remaining discomfort:
  - The command layer still recognizes project mismatch through the daemon's
    protocol error text. A future internal cleanup can make that a typed
    transport result without changing the CLI algebra.

### Iteration 51: Delete Removed Trust Alias from Error Recovery Text

- Commands deleted:
  - No new parser surface. The deleted surface is the recovery instruction
    "Use --refresh-trust" in Quicklisp SHA-256 mismatch errors.
- Commands merged:
  - All Quicklisp trust-refresh recovery text is merged into
    `clpm registry trust refresh <name>`.
- Commands derived instead of exposed:
  - None. Trust refresh is a registry-trust operation, not an update option.
- Commands that survived and why:
  - `registry trust refresh <name>` survives because Quicklisp trust pins are
    persistent verifier state and need an explicit operator action to rotate.
  - `registry update <name>` survives as a registry snapshot update that must
    fail when existing trust pins are contradicted.
- Laws/protocol invariants added:
  - A user-facing error must not name a command or flag that the parser
    rejects, except as an explicit "is rejected" counterexample.
  - Quicklisp distinfo/systems/releases SHA mismatch recovery is:
    `Run clpm registry trust refresh <name>`.
  - `parse ["registry", "update", "--refresh-trust", name] = Error` remains
    the only public mention of the removed alias.
- Remaining discomfort:
  - The lower layer still uses the internal keyword `:refresh-trust` for the
    implementation parameter. That name is not a CLI constructor.

### Iteration 52: Delete Full-RPC Discovery Claim

- Commands deleted:
  - No parser surface. The deleted surface is the documentation claim that
    `repl call methods` lists every RPC.
- Commands merged:
  - Internal wire frames (`eval`, `shutdown`, `query-response`) remain in the
    daemon protocol but are not merged into the public `repl call` discovery
    observation.
- Commands derived instead of exposed:
  - Full protocol introspection is not a CLI observation. The public
    observation is the callable-method schema that `repl call METHOD` can
    construct.
- Commands that survived and why:
  - `repl call methods` survives as public callable RPC discovery.
  - `repl call help --method METHOD` survives only for those discoverable
    callable methods.
- Laws/protocol invariants added:
  - `methods = publicCallableMethods(methodRegistry)`.
  - `internalMethod in {eval, shutdown, query-response} =>
    internalMethod notin methods`.
  - Help and README must describe `methods` as public callable discovery, not
    full internal RPC discovery.
- Remaining discomfort:
  - The daemon still has internal registered frames for transport and
    lifecycle. They are implementation protocol, not CLI algebra.

### Iteration 53: Delete Ambiguous Trust-Refresh No-Op Claim

- Commands deleted:
  - No parser surface. The deleted surface is help text claiming
    non-Quicklisp `registry trust refresh` might be a no-op.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - None. Quicklisp trust refresh remains the only trust-pin refresh
    constructor.
- Commands that survived and why:
  - `registry trust refresh <quicklisp-name>` survives because Quicklisp
    stores unsigned dist hash pins that require explicit rotation.
- Laws/protocol invariants added:
  - `registry.kind != Quicklisp => registry trust refresh registry =
    Failed unsupported-registry-kind`.
  - Help must state the closed domain rather than advertise a no-op branch
    that the implementation does not denote.
- Remaining discomfort:
  - Git registry trust rotation is still modeled as `registry trust set`,
    not as refresh. That is a separate trust operation and remains outside
    this constructor.

### Iteration 54: Expose All `project new` Constructors in README

- Commands deleted:
  - No parser surface. The deleted surface is README table compression that
    hid workspace and workspace-member constructors.
- Commands merged:
  - None. Workspace creation, standalone project creation, and workspace
    member creation are distinct constructors.
- Commands derived instead of exposed:
  - None.
- Commands that survived and why:
  - `project new <name> --workspace [--dir <path>]` survives as the workspace
    resource constructor.
  - `project new <name> --bin|--lib [--dir <path>]` survives as standalone
    project scaffolding.
  - `project new <name> --bin|--lib --member-of <workspace-dir>` survives as
    project scaffolding plus workspace membership mutation.
- Laws/protocol invariants added:
  - Public command tables must enumerate distinct constructor shapes when
    their effects differ.
  - README must contain every accepted `project new` constructor class from
    the target grammar.
- Remaining discomfort:
  - The README table is still maintained by hand. Tests pin the high-risk
    constructor rows so drift is at least observable.

### Iteration 55: Pin Exact Package Export Schema

- Commands deleted:
  - No source change. The deleted risk is partial export sampling that could
    let command wrappers or invocation globals become public API again.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - Internal command helpers remain behind the `clpm.commands` package
    boundary.
- Commands that survived and why:
  - `clpm.commands:cmd-project`, `cmd-deps`, `cmd-registry`, `cmd-run`,
    `cmd-store`, `cmd-repl`, `cmd-skill`, `cmd-help`, and `cmd-doctor`
    survive as the resource dispatchers.
  - `clpm:main` and `clpm:run-cli` survive as executable/test entry points.
- Laws/protocol invariants added:
  - The tests now check exact external symbol sets for `CLPM.COMMANDS` and
    `CLPM`, not only selected internal/external examples.
- Remaining discomfort:
  - This is evidence hardening, not a semantic expansion.

### Iteration 56: Remove Implementation-History Solver Aside

- Commands deleted:
  - No parser surface. The deleted surface is README prose about "legacy
    comments" in the solver section.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - None.
- Commands that survived and why:
  - The deterministic depth-first solver description survives because it
    explains observable resolution order.
- Laws/protocol invariants added:
  - User documentation describes current command semantics and observations,
    not obsolete implementation commentary.
- Remaining discomfort:
  - None for solver prose.

### Iteration 57: Advertise Nested Help Selectors at Root

- Commands deleted:
  - No parser surface. The deleted surface is the root-help row
    `help [cmd]`, which under-described nested help selectors.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - Resource-local `COMMAND ... --help` remains derived from the same help
    selector algebra.
- Commands that survived and why:
  - `help [cmd ...]` survives as the schema observation for command and
    subcommand help.
- Laws/protocol invariants added:
  - Root help must advertise nested selectors because
    `help project workspace add`, `help registry trust refresh`, and other
    leaf pages are public observations.
- Remaining discomfort:
  - None.

### Iteration 58: Name REPL Eval Machine Observation in Leaf Help

- Commands deleted:
  - No executable surface. The deleted surface is an undocumented accepted
    output mode in the `clpm help repl eval` Options list.
- Commands merged:
  - None. `--json` remains the single machine-readable eval observation; the
    human mode still has no `--pretty` alias.
- Commands derived instead of exposed:
  - Human rendering remains the default derived presentation of the same eval
    response.
- Commands that survived and why:
  - `repl eval FORM --json` survives because it exposes the typed daemon eval
    response for tools.
- Laws/protocol invariants added:
  - Every accepted leaf-local output mode with a distinct observation must be
    named by the corresponding leaf help page.
- Remaining discomfort:
  - None.

### Iteration 59: Make Umbrella Help Homomorphic to Leaf Help

- Commands deleted:
  - No executable surface. Deleted only under-specified help observations:
    `clpm help help` with a one-level selector grammar, project umbrella help
    that omitted `--member-of`, registry add help with an untyped Ed25519
    placeholder, REPL umbrella help without daemon status JSON, and eval
    usage without `--json`.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - Umbrella help remains a summary observation, but it must preserve the
    constructors of the leaf grammar it summarizes.
- Commands that survived and why:
  - Nested `help`, workspace-member `project new`, typed registry trust,
    `repl daemon --status --json`, and `repl eval --json` all survive because
    each has a distinct user-visible denotation.
- Laws/protocol invariants added:
  - If an umbrella help page names a constructor family, it must not erase a
    required argument constructor or alter a typed placeholder from the leaf
    page.
- Remaining discomfort:
  - None.

### Iteration 60: Keep Ping Method Counts in the Public RPC Algebra

- Commands deleted:
  - No method is removed. The deleted observation is hidden wire-method names
    leaking through `ping.method_counts`.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - Raw eval traffic remains observable through `eval_count`, not through the
    callable RPC method-count map.
- Commands that survived and why:
  - `repl call ping` survives as liveness and health observation.
  - `method_counts` survives as counters over the same public callable method
    domain as `repl call methods`.
- Laws/protocol invariants added:
  - `keys(ping.method_counts) subset keys(repl call methods)`.
  - Hidden transport or lifecycle methods (`eval`, `shutdown`,
    `query-response`) are not observations of the public call algebra.
- Remaining discomfort:
  - None.

### Iteration 61: Reject Registry-Provided Trust Roots

- Commands deleted:
  - The implicit fallback from a missing local Ed25519 key to
    `registry/keys/<key-id>.pub` in the registry being verified.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - Embedded registry keys remain registry metadata for distribution and
    inspection, but they do not denote trust by themselves.
- Commands that survived and why:
  - `registry key import` survives as the explicit operation that moves a
    public key into the local trust root.
  - Signed git registry verification survives as `trust key id + local public
    key + valid detached signatures`.
- Laws/protocol invariants added:
  - `verify(snapshot, ed25519:k)` may read only local trusted key material for
    `k`; it must not derive the verifying key from the untrusted registry
    contents being checked.
  - A registry that carries a valid self-signature with only an embedded public
    key is still untrusted.
- Remaining discomfort:
  - None.

### Iteration 62: Preserve Quicklisp Inner Pins on Update

- Commands deleted:
  - The accidental first-use path where `registry update` forgot configured
    Quicklisp `systems.txt` / `releases.txt` pins after the local registry
    cache was removed.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - Local registry metadata is a cache of configured trust state, not the
    source of that trust state.
- Commands that survived and why:
  - `registry update <quicklisp>` survives as ordinary refresh that enforces
    all configured pins.
  - `registry trust refresh <quicklisp>` survives as the only operation that
    may intentionally replace Quicklisp pins.
- Laws/protocol invariants added:
  - Configured Quicklisp pins travel with the registry reference into every
    clone/update operation.
  - Removing `CLPM_HOME/registries/<name>/` must not weaken or reset
    configured Quicklisp trust.
- Remaining discomfort:
  - None.

### Iteration 63: Store Identities Are Digests, Not Paths

- Commands deleted:
  - The accidental interpretation of lockfile/store identity strings as
    pathname fragments.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - Source, artifact, and build store paths are derived only from canonical
    SHA-256 digest identities.
- Commands that survived and why:
  - `store gc`, `store clean`, `deps sync`, `deps fetch`, and build commands
    survive because they operate over content identities, not filesystem
    selectors.
- Laws/protocol invariants added:
  - Store identity inputs are 64-character hexadecimal SHA-256 digests before
    any pathname is constructed.
  - Invalid lockfile/store identities fail as user/configuration errors; they
    are not cache misses and cannot escape the store namespace.
- Remaining discomfort:
  - None.

### Iteration 64: Explain Events Expose Method Params Only

- Commands deleted:
  - The accidental observation that `repl` explain plan frames echoed
    transport and dispatch fields (`project_root`, `token`, `explain`).
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - Transport authentication and project routing are CLPM-owned envelopes;
    they are not part of a method's user parameter algebra.
- Commands that survived and why:
  - `repl call <method> --explain true` survives as a method-plan
    observation.
  - TCP REPL transport survives with token authentication, but tokens remain
    unobservable in explain output.
- Laws/protocol invariants added:
  - `plan.params` is the original request params minus
    `+implicit-method-params+`.
  - `project_root`, `token`, and `explain` never appear in `plan.params`.
- Remaining discomfort:
  - None.

### Iteration 65: Machine Output Is Not a Diagnostic Channel

- Commands deleted:
  - The accidental stdout prefix where `--insecure` registry warnings appeared
    before `deps search --json` results.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - Diagnostics are process observations on stderr, not members of the JSON
    result value.
- Commands that survived and why:
  - `deps search --json` survives as exactly one JSON value on stdout.
  - `--insecure` survives only as an explicit trust-bypass flag with a stderr
    warning.
- Laws/protocol invariants added:
  - Machine stdout is parseable as exactly one encoded result value.
  - Trust warnings go to stderr and never prefix JSON stdout.
- Remaining discomfort:
  - None.

### Iteration 66: Daemon Liveness Is Endpoint Liveness

- Commands deleted:
  - The accidental lifecycle state where a live unrelated PID in
    `.clpm/repl.pid` without the advertised socket could make a project look
    like it had a daemon.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - Pidfiles are implementation metadata for the selected project endpoint;
    they are not an independent daemon identity.
- Commands that survived and why:
  - `repl daemon --status` survives as the observation of selected-project
    lifecycle state.
  - `repl daemon --stop` survives as the lifecycle mutation for a proven
    selected-project daemon.
- Laws/protocol invariants added:
  - `running(root)` requires a live pidfile, an existing endpoint, and a
    successful authenticated daemon ping whose params select `root`.
  - A pidfile that names a live process but has no endpoint denotes stale
    lifecycle metadata, not an unresponsive daemon.
  - Cleaning stale lifecycle metadata must not signal or kill the pid named in
    the stale pidfile.
- Remaining discomfort:
  - None.

### Iteration 67: Store Existence Means Complete Digest Object

- Commands deleted:
  - The accidental store state where a path at a digest location counted as a
    stored object even when the copy failed halfway or the artifact bytes did
    not hash to the path's digest.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - Store paths are implementation indexes derived from digest identities; the
    path alone is not an observation.
- Commands that survived and why:
  - `store gc` and dependency realization survive unchanged; they now observe
    only complete store entries.
  - `source-exists-p`, `artifact-exists-p`, `get-source-path`, and
    `get-artifact-path` survive as implementation observations over completed
    digest objects.
- Laws/protocol invariants added:
  - `artifactExists sha` requires an artifact file whose SHA-256 is `sha`.
  - `sourceExists sha` requires a `src/` directory and a `meta.sxp` completion
    marker whose `:tree-sha256` is `sha`.
  - `storeSource` writes source metadata last and repairs partial or
    mismatched entries before returning success.
  - `storeArtifact` publishes through a temporary file under the digest lock
    and repairs corrupt entries before returning success.
- Remaining discomfort:
  - Source tree metadata proves completion, not post-publication immutability
    against external mutation. Local VCS control directory leakage is closed
    by Iteration 68.

### Iteration 68: Source Identity Excludes Local VCS Control State

- Commands deleted:
  - The accidental source identity where `.jj` control metadata affected tree
    hashes, source-store copies, and publish tarballs.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - VCS control directories are local workspace machinery, not project source
    content.
- Commands that survived and why:
  - Path/git/tarball source realization survives; it now realizes only project
    source files after the standard local-control exclusions.
  - `registry publish` survives as a source-content publication operation, not
    an archive of the caller's checkout machinery.
- Laws/protocol invariants added:
  - Mutating `.git`, `.hg`, `.svn`, `.jj`, or `.clpm` does not change default
    `sha256-tree`.
  - Default source walking and store copies exclude `.git`, `.hg`, `.svn`,
    `.jj`, and `.clpm`.
  - Publish tarballs exclude `.git`, `.hg`, `.svn`, `.jj`, `.clpm`, `dist`,
    and `clpm.lock`.
- Remaining discomfort:
  - None for local VCS control directories.

### Iteration 69: Update Requires Fresh Registries

- Commands deleted:
  - The accidental `deps update` path that logged a registry refresh failure,
    resolved against stale cached snapshots, wrote `clpm.lock`, and returned
    success.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - Cached registry snapshots are inputs to ordinary resolution, not evidence
    that the update operation refreshed its registry state.
- Commands that survived and why:
  - `deps update [system ...]` survives as the unlock-and-refresh
    dependency operation.
- Laws/protocol invariants added:
  - `deps update` is `refreshRegistries >=> resolveWithUnlock >=> writeLock`.
  - If any configured registry refresh fails, `deps update` fails before solve
    and does not rewrite `clpm.lock`.
- Remaining discomfort:
  - None for registry refresh failure propagation. The SBOM
    metadata-failure erasure hazard is closed by Iteration 70.

### Iteration 70: SBOM Metadata Failure Is Not Absence

- Commands deleted:
  - The accidental SBOM path that swallowed trusted registry load or release
    metadata verification failures and emitted a partial machine document as
    though licenses were merely absent.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - License enrichment is derived from verified registry metadata. Verification
    failure is an `Outcome`, not a nullable license value.
- Commands that survived and why:
  - `deps sbom --format ...` survives as a machine-output observation of the
    lockfile plus verified registry metadata.
- Laws/protocol invariants added:
  - `metadataMissing(package, version)` may render an SBOM component without a
    license.
  - `metadataLoadFailed(registry)` and
    `metadataVerificationFailed(registry, package, version)` fail the SBOM
    command before any SBOM is written.
  - Machine SBOM stdout is either one complete document or empty on command
    failure.
- Remaining discomfort:
  - None for trusted registry metadata erasure.

### Iteration 71: REPL Identity Is Project Identity

- Commands deleted:
  - The accidental lifecycle path where a live pidfile plus any socket file
    meant "this project's daemon exists" before proving the daemon's project
    identity.
  - The accidental in-process sharing of `COMMON-LISP-USER` as the default
    eval package for every project daemon.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - A daemon's default eval namespace is a private package owned by that
    daemon. Public observations map it back to `COMMON-LISP-USER`; the private
    package name and project path are not part of the CLI algebra.
- Commands that survived and why:
  - `repl daemon`, `repl eval`, and `repl call` survive as the lifecycle,
    ergonomic eval, and RPC constructors for one project-scoped image.
- Laws/protocol invariants added:
  - `daemonExists(project)` requires both live lifecycle metadata and
    an authenticated ping whose params select `canonical(project)`.
  - A Lisp process hosts at most one active project daemon. Different projects
    get separate daemon processes, not separate threads in one CL image.
  - Every public REPL observer reports the selected daemon's private default
    package as `COMMON-LISP-USER` and never exposes a root-derived
    `CLPM.REPL.USER.*` name.
  - Shutting down a project daemon releases CLPM's owned default package, so a
    later foreground daemon in the same Lisp process does not inherit stopped
    REPL bindings as a hidden cache.
  - Explicit package selection remains explicit: `--package` and `set-package`
    may choose a shared package by name, but the default constructor never does.
- Remaining discomfort:
  - None for project daemon identity. ASDF and package registries remain
    process-global, so the law is process isolation rather than multi-project
    hosting inside one Lisp image.

### Iteration 72: Public Schema Must Be Bound and Concrete

- Commands deleted:
  - None at the command level.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - Public package exports are schema observations. A name that is not backed
    by a real binding is not a compatibility surface; it is deleted.
- Commands that survived and why:
  - `help`, `--help`, README, and `skill` survive as observations of the
    command algebra, not independent hand-maintained surfaces.
- Laws/protocol invariants added:
  - Every exported `clpm.project` lockfile accessor must be backed by an
    actual struct accessor or constructor contract.
  - Nested help selectors are variadic everywhere user-facing schema text
    describes them: `clpm help <command> [subcommand ...]`.
  - Example trust values must be typed placeholders such as
    `ed25519:<key-id>`, not fake key material.
  - Every umbrella REPL eval usage names `--json`, because machine-readable
    eval output is a surviving observation mode.
- Remaining discomfort:
  - None for this schema-honesty slice.

### Iteration 73: REPL Endpoint Authority Is Token Plus Project Acceptance

- Commands deleted:
  - The accidental unauthenticated Unix transport. File permissions are useful
    containment, not a protocol proof.
  - The accidental `ping.project_root` observation. Echoed request identity is
    not authority and leaks selected project paths.
  - Root-derived private package names in public REPL observations.
  - `repl daemon --status --json` disclosure of socket and log paths.
- Commands merged:
  - Missing/wrong endpoint token and project-root rejection are both stale
    lifecycle metadata for the selected project, not "an unresponsive daemon"
    and not proof that another project's image is usable.
- Commands derived instead of exposed:
  - Unix endpoints derive their shared token from `endpoint.token`; TCP
    endpoints derive it from the advertised port file. Clients inject the token
    and the selected `project_root`; users cannot supply either field through
    `repl call`.
  - `COMMON-LISP-USER` in a project daemon denotes that daemon's private
    default package, not the process-global CL-USER package.
- Commands that survived and why:
  - `repl eval` survives because it is the ergonomic constructor for one
    selected-project image.
  - `repl call ping` survives as a liveness observation, but only after the
    endpoint has authenticated and accepted the selected project root.
  - `repl daemon --status --json` survives as machine lifecycle state, not as a
    filesystem topology dump.
- Laws/protocol invariants added:
  - `project_root` is identity carried by the client; authority is
    `token(endpoint) && acceptsProject(endpoint, project_root)`.
  - `ping(root)` success means the endpoint's token matched and its project
    guard accepted `root`; the result does not contain `project_root`.
  - `statusJson(root)` contains lifecycle facts such as state, pid, Lisp, and
    eval count, but not socket path, log path, or project path.
  - A forged Unix request with only `project_root` and no token is rejected.
  - A wrong-token TCP endpoint for the selected project is stale lifecycle
    metadata and is cleaned by status/stop/eval autostart paths.
  - A second active project daemon in one Lisp process is rejected before it can
    share packages, ASDF state, debugger sessions, workers, or loaded systems.
- Remaining discomfort:
  - None for cross-project REPL visibility. A user with filesystem access to
    another project's token can intentionally target that daemon, but the
    daemon still rejects requests whose selected project root differs.

### Iteration 74: Fixed-Arity Constructors Reject Residual Tokens

- Commands deleted:
  - Silent residual-argument acceptance for `doctor`, `registry list`,
    `registry trust list`, and `registry trust set`.
- Commands merged:
  - None.
- Commands derived instead of exposed:
  - No residual tokens are interpreted as comments, ignored payload, or future
    compatibility space. A command vector is a closed constructor application.
- Commands that survived and why:
  - `doctor` survives as a nullary environment observation.
  - `registry list` survives as a nullary registry-config observation.
  - `registry trust list` survives as a nullary trust observation.
  - `registry trust set NAME TRUST` survives as an exact binary trust
    mutation.
- Laws/protocol invariants added:
  - `parse ["doctor", extra...] = Error` when `extra...` is non-empty.
  - `parse ["registry", "list", extra...] = Error` when `extra...` is
    non-empty.
  - `parse ["registry", "trust", "list", extra...] = Error` when `extra...`
    is non-empty.
  - `parse ["registry", "trust", "set", name, trust, extra...] = Error`
    when `extra...` is non-empty, and the registry config is unchanged.
  - Successful fixed-arity constructors consume the entire command vector.
- Remaining discomfort:
  - Other fixed-arity leaves should keep being attacked in later loops. This
    slice closes the counterexamples found in the registry trust/operator
    surface and the top-level nullary observation.

### Iteration 75: REPL Liveness Requires Project Proof

- Commands deleted:
  - The accidental endpoint state where a token-valid daemon without a project
    identity could satisfy `repl call` for the selected project.
- Commands merged:
  - Daemon liveness and project ownership are one observation. "The socket is
    reachable" is not a useful public fact unless the daemon proves it denotes
    the selected project image.
- Commands derived instead of exposed:
  - `project_id` is an opaque fingerprint of the daemon's canonical project
    root. It is a proof field for clients and lifecycle code, not a user
    selector and not a replacement for `project_root`.
- Commands that survived and why:
  - `repl call METHOD` survives as a non-autostarting RPC constructor, but it
    can only dispatch after the endpoint proves the selected project identity.
  - `repl eval FORM` survives as the autostarting constructor; an unscoped or
    foreign endpoint is cleaned before autostart.
  - `repl call ping` survives as the daemon health observation and now carries
    the opaque identity proof without exposing the project path.
- Laws/protocol invariants added:
  - `daemonExists(root)` requires live lifecycle metadata, endpoint-token
    authentication, accepted `project_root`, and
    `ping.project_id = sha256(canonical(root))`.
  - `tokenMatches(endpoint) && missing ping.project_id => endpoint = Absent`
    for a project-scoped CLI command.
  - `replCall root method` preflights the endpoint with the same project proof
    as `daemon --status` and `repl eval`; it cannot observe workers,
    debugger sessions, packages, or eval history from an unscoped daemon.
  - `ping.project_id` is not `project_root`: public output may expose an opaque
    digest proof, but not the selected project path.
- Remaining discomfort:
  - Existing raw in-process test daemons still exist for protocol tests. They
    remain outside the project CLI algebra unless they carry the project proof.

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
  eval notin callableMethodRegistry
  help(eval) = Failed unknown-method

Law: "repl call is registry-closed"
  method notin callableMethodRegistry
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

Law: "scalar root options are single-valued"
  parse ["--jobs", n, "--jobs", m, "deps", "sync"] = Error
  parse ["--fetch-retries", n, "--fetch-retries", m, "deps", "search", q] = Error
  parse ["--fetch-timeout", n, "--fetch-timeout", m, "registry", "update"] = Error

Law: "sync stage selection is single-valued"
  parse ["deps", "sync", "--to", a, "--to", b] = Error
  parse ["--offline", "deps", "sync", "--to", a, "--to", b] = Error
  parse ["--jobs", n, "deps", "sync", "--to", a, "--to", b] = Error
  parse ["--lisp", impl, "deps", "sync", "--to", a, "--to", b] = Error

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
  parse ["repl", "eval", form, "--debug", "--json"] = Error

Law: "run entrypoint args require an explicit boundary"
  parse ["run"] = Right (run (EntryPoint []))
  parse ["run", "--", args...] = Right (run (EntryPoint args))
  token notin {"--", "exec", "test", "script", "scripts", "help", "--help"}
    => parse ["run", token, args...] = Error

Law: "run exec argv boundary before project lookup"
  parse ["run", "exec", "--", prog, args...] =
    Right (run (Exec (prog, args)))
  xs in {[], ["--"], [prog, args...]} =>
    denote (parse (["run", "exec"] ++ xs)) ctx world = FailedUsage
  -- the failure is independent of project discovery or activation state.

Law: "leaf argv validation before state lookup"
  denote (parse ["deps", "remove"]) ctx world = FailedUsage
  denote (parse ["deps", "remove", "--bogus"]) ctx world = FailedUsage
  denote (parse ["run", "script"]) ctx world = FailedUsage
  -- these failures are independent of project discovery, registry state,
  -- lockfile state, activation state, and manifest contents.

Law: "leaf singleton options are values"
  duplicate opt in {
    ["deps", "search", query, "--limit"],
    ["deps", "tree", "--depth"],
    ["deps", "sbom", "--format"],
    ["project", "new", name, opt] where opt in {"--dir", "--member-of"},
    ["project", "workspace", "list", "--dir"],
    ["registry", "add", "--name"],
    ["registry", "init", opt] where opt in {"--dir", "--key-id", "--keys-dir"},
    ["registry", "key", "generate", opt] where opt in {"--out", "--id"},
    ["registry", "key", "list", "--keys-dir"],
    ["registry", "key", "import", opt] where opt in {"--pub", "--id", "--keys-dir"},
    ["registry", "key", "verify", opt] where opt in {"--pub", "--file", "--sig"},
    ["registry", "publish", opt] where opt in {
      "--registry", "--key-id", "--keys-dir", "--project",
      "--tarball-url", "--tarball-out"
    },
    ["repl", "eval", form, "--worker"]
  } => denote (parse argv) ctx world = FailedUsage

Law: "help is schema projection"
forall selector ctx world.
  denote (help selector) ctx world =
    Succeeded world (HumanText (render selector commandSchema))

Law: "exact resource-root help alias"
forall command in {project, deps, registry, run, store}.
  denote (parse [command, "help"]) ctx world =
    denote (parse ["help", command]) ctx world
  xs /= [] => denote (parse ([command, "help"] ++ xs)) ctx world = Failed

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

Law: "implementation hooks are not public exports"
  status("CLPM.BUILD", "BUILD-SPEC") = internal
  status("CLPM.REPL", "USER-INTERRUPT") = internal
  status("CLPM.FETCH", "*FETCH-BACKOFF-BASE*") = internal
  status("CLPM.FETCH", "*FETCH-SLEEP-FN*") = internal
  status("CLPM.FETCH", "*TEST-FETCHER*") = internal

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
- `repl eval` in one named worker does not affect another named worker's
  history bindings.

Observation properties:

- `help` only advertises public commands.
- Root `help` advertises only top-level controls; scoped dependency,
  registry, fetch, and Lisp-selection options live on command-specific help
  and README scoped-option documentation.
- Exact `project help`, `deps help`, `registry help`, `run help`, and
  `store help` produce the same successful schema observation as
  `clpm help project`, `clpm help deps`, `clpm help registry`,
  `clpm help run`, and `clpm help store`.
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
- `clpm --json` is rejected; an unknown pre-command flag cannot silently denote
  bare help.
- `clpm --version --json`, `clpm --help deps`, and
  `clpm deps --version` are rejected; terminal root observations do not mask
  residual arguments or command-local tokens.
- `clpm help -v`, `clpm repl --insecure`, `clpm deps sync --offline`,
  `clpm deps sync --jobs 4`, and `clpm run --lisp sbcl` are rejected by the
  owning command parser; global options cannot be placed after the command
  token.
- `clpm run exec`, `clpm run exec --`, and `clpm run exec sh -c true`
  reject with `run exec` usage before any project discovery error can mask the
  malformed argv.
- `clpm deps remove`, `clpm deps remove --bogus`, and `clpm run script`
  reject with command-local usage/option errors before any project discovery
  error can mask the malformed argv.
- Duplicate singleton leaf options reject instead of silently overwriting:
  `deps search --limit`, `deps tree --depth`, `deps sbom --format`,
  `project new --dir/--member-of`, `project workspace list --dir`,
  `registry add --name`,
  `registry init` value flags, `registry key generate/list/import/verify`
  value flags, `registry publish` value flags, and `repl eval --worker`.
- Root help, README, and generated `clpm skill` output state that scoped
  options must appear before the command token.
- BRIDGE.md and `.claude/skills/clpm-repl-bridge.md` describe the surviving
  `clpm repl` surface, human-default eval output, and `--json` machine output;
  they do not advertise `clpm repl-bridge`, `--pretty`, raw-JSON defaults, or
  hidden eval method help.
- `clpm help repl eval` lists accepted debug selectors including
  `--break-on` and `--timeout-ms`.
- `clpm repl eval FORM --debug` rejects incomplete or conflicting
  continuation selectors: `--frame` without `--frame-eval`,
  `--frame-eval` without `--frame`, `--arg` without `--restart`, and multiple
  actions selected together.
- `clpm deps sbom --output PATH` is rejected; `--out` is the only SBOM
  file-output spelling.
- A stale REPL endpoint in project B that points at project A is treated as
  absent for B; `repl call`, `repl eval --no-autostart`, and debug eval do
  not surface project A's daemon.
- A token-valid REPL endpoint with no project identity proof is treated as
  absent for a project-scoped `repl call`; it cannot expose raw daemon workers
  to the selected project.
- `clpm repl daemon` and manifest repl autostart authenticate a live endpoint
  and prove selected-project acceptance before deciding that daemon already
  exists.
- A forged Unix `ping` that supplies only `project_root` and no token is
  rejected.
- A TCP endpoint whose port is live but whose advertised token is wrong is
  stale lifecycle metadata for the selected project, not an unresponsive
  daemon.
- A second active project daemon in one Lisp process is rejected; different
  project REPLs are isolated by process, not by sharing a CL image.
- `current-package`, `list-workers`, `apropos`, `function-info`,
  `list-redefinitions`, `describe`, `inspect`, `list-traced`, `ping`, and
  `repl daemon --status --json` do not expose root-derived private package
  names, socket paths, log paths, or selected project paths.
- `clpm doctor extra`, `clpm registry list extra`,
  `clpm registry trust list extra`, and
  `clpm registry trust set main ed25519:key extra` are rejected instead of
  ignoring the trailing token; the failed trust-set form leaves configured
  trust unchanged.
- A live unrelated PID in `.clpm/repl.pid` without `.clpm/repl.sock` is stale
  lifecycle metadata; status and stop clean the selected project's files and
  leave the unrelated process alive.
- Corrupt artifact files, source directories without completion metadata, and
  source entries whose metadata names a different digest are not valid store
  objects; storing the correct bytes/tree repairs them.
- `.jj` control state does not affect default source tree hashes, is not copied
  into the source store, and is not included in publish tarballs.
- `deps update` fails before solving and leaves `clpm.lock` unchanged when any
  configured registry cannot refresh.
- `deps sbom` fails with empty machine stdout when trusted registry metadata
  cannot be loaded or verified.
- `clpm --insecure help` is rejected; `--insecure` is not an inert
  pre-command global decoration.
- `clpm repl call eval --form FORM` is rejected; public evaluation goes
  through `clpm repl eval FORM`.
- `clpm --offline help` and `clpm --offline deps sync --to lock` are
  rejected; `--offline` is only for artifact/cache operations.
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
- Duplicate scalar root options such as `--jobs 1 --jobs 2`,
  `--fetch-retries 1 --fetch-retries 2`, and
  `--fetch-timeout 1 --fetch-timeout 2` are rejected before command dispatch.
- `clpm deps sync --to source --to lock`,
  `clpm --offline deps sync --to source --to lock`,
  `clpm --jobs 2 deps sync --to source --to lock`, and
  `clpm --lisp sbcl deps sync --to build --to source` are rejected; sync
  stage selection is a single-valued pipeline selector.
- `clpm registry trust set main none` and
  `clpm registry trust set main nil` are rejected; permanent trust clearing
  is not a CLI trust value.
- `clpm registry update --refresh-trust quicklisp` is rejected; Quicklisp
  pin refresh is `clpm registry trust refresh quicklisp`.
- Quicklisp SHA-256 mismatch errors name
  `clpm registry trust refresh <name>` and do not advertise
  `--refresh-trust`.
- `clpm repl call methods` documentation says public callable RPC discovery,
  not "every RPC" or full internal wire-registry discovery.
- `clpm help registry trust refresh` states that only Quicklisp registries
  support trust refresh and does not describe a no-op for git registries.
- README documents the workspace, standalone project, and workspace-member
  `project new` constructor forms.
- Tests pin the exact public exports of `CLPM.COMMANDS` and `CLPM`.
- `clpm.build:build-spec`, `clpm.repl:user-interrupt`, and fetch test hooks
  are not exported; they are implementation surfaces with no CLI denotation.
- `clpm.project:lockfile-project` is not exported; the surviving lockfile
  project observations are the backed accessors `lockfile-project-name` and
  `lockfile-project-sha256`.
- README solver prose no longer mentions implementation-history comments.
- Root help advertises `help [cmd ...]` rather than only `help [cmd]`.
- Root help, nested help, README, and generated skill output use concrete
  variadic help selectors, typed Ed25519 trust placeholders, and REPL eval
  summaries that include `[--json]`.
- README does not use fake Ed25519 examples such as `ed25519:...`; examples
  name the typed placeholder `ed25519:<key-id>`.
- `clpm registry publish --git-commit ...` is rejected; publish does not run
  VCS commands.
- `clpm repl eval FORM --pretty` is rejected; human output is the default and
  has no flag alias.
- `clpm repl eval FORM --debug --json` is rejected; JSON eval denotes one raw
  eval response, not a debugger event stream plus terminal response.
- `clpm run bare args` is rejected; entrypoint arguments require
  `clpm run -- bare args`.
- A daemon that evaluates `10` in worker `alpha` and `20` in worker `beta`
  must report `* = 10` when evaluating `(values *)` in worker `alpha`.

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

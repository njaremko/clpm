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
execution environments, the content store, and the persistent repl bridge.

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
5. `clpm repl-bridge eval FORM --debug` evaluates in the persistent project
   image and exposes debugger state.

Edge cases and failure modes:

1. A workspace root without `-p/--package` is not a project target.
2. Offline source realization fails when a locked artifact is not already in
   the store.
3. Registry trust updates must not silently weaken signature or hash checks.
4. Kept repl-bridge debugger sessions, watches, traces, and throwaway workers
   are observable bridge state and must be explicitly cleaned up.

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
data Bridge
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
        , bridges    :: Map ProjectRoot Bridge
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
  bridge cleanup state.

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
bridgeCall  :: ProjectTarget -> Method -> Params -> World -> Outcome
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
- Places `repl`, `exec`, `test`, and `scripts` beside unrelated resource
  operations even though they all run inside a project environment.
- Has no law explaining why some nouns are top-level and some are nested.

Candidate B: one `project` command containing everything.

```haskell
denoteB :: ProjectInvocation -> ProjectTarget -> World -> Outcome
```

Pros:

- Few top-level commands.

Cons:

- Complects registry configuration, global key management, store garbage
  collection, and repl-bridge daemon state with a project manifest.
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
  | BridgeOp BridgeOperation
  | Doctor
  | Help Selector
  | Skill
```

Why this is the simplest precise model:

- Each top-level command names one semantic carrier.
- Pipeline prefixes become parameters of `deps sync`, not separate top-level
  verbs.
- Execution modes become `run` operations because they all denote "execute
  something in the project environment".
- Registry keys and publishing move under `registry` because they mutate or
  observe registry trust and registry contents.
- Store cleanup moves under `store` because it operates on shared store
  reachability, not dependency intent.
- `repl-bridge` remains a top-level carrier because its long-lived image,
  workers, debugger sessions, watches, traces, and cleanup state are not the
  same resource as a one-shot project execution.

Values excluded from the model or represented by a restricted semantic domain:

- Unknown command names.
- Invalid project targets.
- Invalid registry trust strings.
- Invalid sync stages.
- Invalid bridge RPC methods or parameter schemas.

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
clpm [options] deps tree [--package <member>] [--depth N]
clpm [options] deps why <system> [--package <member>]
clpm [options] deps audit [--json]
clpm [options] deps sbom --format <format> [--out <path>]

clpm [options] registry list|add|update|trust|init ...
clpm [options] registry key generate|list|import|verify ...
clpm [options] registry publish ...

clpm [options] run [-- <args...>]
clpm [options] run repl [system]
clpm [options] run exec -- <cmd...>
clpm [options] run test
clpm [options] run script <name> [-- <args...>]
clpm [options] run scripts

clpm [options] store clean [--dist] [--store]
clpm [options] store gc [--dry-run]

clpm [options] repl-bridge daemon [--detach] [--no-load] [--status] [--stop]
clpm [options] repl-bridge eval FORM [--package P] [--worker W] [--debug] ...
clpm [options] repl-bridge call METHOD [--params-json JSON] [--PARAM VALUE]...
```

Bare `clpm [options]` denotes `clpm [options] deps sync`.

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
| `repl` | Derived execution | `run repl` | Starts an interactive project Lisp. |
| `scripts` | Derived execution/listing | `run script`, `run scripts` | Scripts are named project executions. |
| `package` | Artifact constructor | `project package` | Builds the artifact configured by project metadata. |
| `clean` | Store/project cleanup | `store clean` | Removes generated project/store reachability. |
| `gc` | Store cleanup | `store gc` | Garbage-collects unreachable store entries. |
| `repl-bridge` | Primitive carrier | `repl-bridge` | Persistent image has independent lifecycle state. |

## Constructors

Terminal constructors:

```haskell
doctor       :: Invocation
skill        :: Invocation
help         :: Selector -> Invocation
bridgeDaemon :: DaemonAction -> Invocation
```

Inductive constructors:

```haskell
project      :: ProjectOperation -> Invocation
deps         :: DependencyOperation -> Invocation
registry     :: RegistryOperation -> Invocation
run          :: RunOperation -> Invocation
store        :: StoreOperation -> Invocation
replBridge   :: BridgeOperation -> Invocation
```

Derived constructors:

```haskell
defaultInvocation = deps (sync Active)
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
| `replBridge` | Operate on persistent project image state | Yes | No |
| old `resolve/fetch/build/install` | Pipeline prefixes | Yes | Yes, `deps sync --to ...` |
| old `keys/publish` | Registry operations | Yes | Yes, `registry key/publish` |
| old `exec/test/repl/scripts` | Execution modes | Yes | Yes, `run ...` |

## Denotation Laws

```haskell
Law: "default is active dependency sync"
  denote (parse []) ctx world = denote (deps (sync Active)) ctx world

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

Law: "run/repl"
forall system.
  denote (run (repl system)) ctx world =
    executeInteractiveLisp system (activationEnv ctx world) world

Law: "run/test"
  denote (run test) ctx world =
    executeConfiguredTests (activationEnv ctx world) world

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

Law: "help is schema projection"
forall selector ctx world.
  denote (help selector) ctx world =
    Succeeded world (HumanText (render selector commandSchema))
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

Law: "bridge cleanup leaves no kept operational state"
forall bridge.
  cleanup bridge =
    bridge { debugSessions = empty
           , watches = empty
           , traces = empty
           , throwawayWorkers = empty
           }
```

Conditions moved into types:

- `SyncStage = Lock | Source | Build | Active`.
- `RunOperation = EntryPoint Args | Repl MaybeSystem | Exec Command |
  Test | Script Name Args | Scripts`.
- `RegistryOperation` contains `KeyOperation` and `PublishOperation`; they
  are not top-level commands.

## Interface Morphism Checks

Claimed project-local interface: parser/rendered help schema.

```haskell
Law: "parse/help schema"
forall invocation selector.
  parse invocation = Right command
  => help selector commandSchema mentions command iff command is public
```

Rejected instances:

| Instance | Smallest counterexample | Left denotation | Right model method | Design response |
| --- | --- | --- | --- | --- |
| Flat top-level command monoid | `resolve` and `install` | Both operate on same realization pipeline at different prefixes | No single resource identity | Collapse to `deps sync --to ...`. |
| `keys` as top-level resource | `keys import` | Mutates trust key material | Not independent of registry trust | Move to `registry key`. |
| `scripts` as project metadata operation | `scripts run fmt` | Executes command in activation | Not a manifest constructor | Move execution to `run script`. |
| `repl-bridge` under `run` | `repl-bridge daemon --status` | Observes/cleans daemon lifecycle state | Not one-shot execution | Keep top-level carrier. |

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
| Orthogonality | 4 | Project, deps, registry, run, store, and bridge have separate carriers. |
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
- `cmd-repl-bridge`

Derived constructors:

- `deps sync --to lock/source/build/active`
- `registry key ...`
- `registry publish ...`
- `run repl/exec/test/script/scripts`

Observation implementation:

- Preserve existing JSON and human rendering for observations after the
  command path is changed.

Transported instances through `denote`/`reify`:

- Existing internal functions may remain if the public parser no longer
  exposes their old top-level names.
- No compatibility aliases are added.

## Property and Command Tests

Generators:

- Small command vectors for public and removed command paths.

Denotation properties:

- Bare `clpm` and `clpm deps sync` return the same kind of outcome in a
  project.
- `deps sync --to lock` writes/refreshes `clpm.lock` and does not require
  activation.
- `store gc --dry-run` does not mutate store reachability.

Observation properties:

- `help` only advertises public commands.
- `deps search/info/audit/sbom --json` remain stable.

Failed-counterexample regressions:

- `clpm add`, `clpm install`, `clpm keys`, `clpm publish`, `clpm test`, and
  `clpm gc` are unknown top-level commands after the refinement.

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

- Registry indexes, source fetch cache layout, ASDF output paths, and bridge
  transport details.

Performance risks:

- `deps sync --to source/build` now denotes a prefix from project intent. Avoid
  redundant solver or fetch work where existing lock hashes prove inputs are
  unchanged.

## Open Semantic Decisions

No open semantic decisions block this implementation. Naming, grouping, and
derived-command expansion are fixed by the target surface above.

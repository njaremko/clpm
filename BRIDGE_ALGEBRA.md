# `clpm repl` denotational algebra

Mode: semantic design specification.

Implementation allowed: yes for the current rewrite, because the denotation
gate passed and the user explicitly requested implementation. Code changes
must preserve this spec; if a desired change alters meaning, equality,
observations, or laws, update this algebra before implementation.

## Intent

The bridge denotes an interactive, project-scoped SBCL REPL service for an
LLM. It should be simple to drive, hard to misuse, and powerful enough to use
Common Lisp as Common Lisp: persistent packages and history, hot
redefinition, restarts, frame evaluation, inspector sessions, source
navigation, compilation diagnostics, tracing, watches, and daemon
introspection. Timing and profiling remain ordinary Lisp work performed
through `eval`, not separate bridge methods.

This algebra models the public meaning of the bridge:

1. Typed client actions over a live image.
2. Server frames observable by JSON-RPC clients.
3. Persistent resources that must be queryable: workers, debug sessions,
   inspectors, watches, redefinitions, traces, and method schemas.
4. A deliberately tiny CLI algebra that renders or composes the same typed
   actions instead of duplicating the RPC registry.

This algebra deliberately ignores:

1. Socket implementation, TCP-vs-Unix transport mechanics, thread libraries,
   mailbox representations, locks, counters, caches, and log rotation.
2. The internal denotation of arbitrary SBCL/Common Lisp programs. The bridge
   treats SBCL as an external semantic oracle with its own image transition
   semantics.
3. Human editor integration and replacement of SLIME/Slynk.
4. Sandboxing. A bridge action has the authority of the hosted image and host
   process.

Primary users are LLM agents and simple non-Lisp tools. Humans may use the
CLI, but the design center is a stateless tool caller driving a stateful
server.

Representative examples:

1. Evaluate `(defun f (x) (+ x 1))`, then evaluate `(f 41)` without reloading
   the system.
2. Enter a debugger on an error, inspect frame locals, then invoke a restart
   without re-running side effects.
3. Open an inspector on a value, traverse into a part, evaluate with `*`
   bound to the focus, then close the session.
4. Watch `src/*.lisp`, reload on save, then call `list-redefinitions`
   before handoff.
5. Ask the registry through `call methods` or `call help --method eval` to
   discover the exact request schema rather than relying on stale
   documentation.

Edge cases and failures:

1. Reader errors, eval errors, resource exhaustion, output caps, worker death,
   unavailable frame variables, missing packages/symbols/systems/files, bad
   restart arguments, unknown params, wrong JSON types, stale daemon files,
   closed connections, multiple active debug sessions, and ambiguous fresh
   debug actions.
2. Long-running evals, query-driven input, streaming output, independent
   workers, concurrent one-shot workers, and watch events occurring later than
   the request that created the watch.
3. Fresh integer ids are capabilities, not meaning. Specs and tests should
   reason up to consistent renaming of debug/inspect/watch ids.

## Carrier Types

Names below are semantic carriers, not implementation structs.

```haskell
data Bridge
data World            -- SBCL image + filesystem + clock + schedule oracle
data Image            -- opaque SBCL image state
data MethodRegistry
data Action
data RequestId
data ConnectionId
data Frame
data Event
data Terminal
data Error
data Payload

data WorkerName
data Worker
data WorkerStatus
  = WorkerIdle
  | WorkerBusy EvalId
  | WorkerInDebugger DebugId
  | WorkerDead

data PackageChoice
  = UseWorkerPackage
  | UsePackageOnce PackageName

data EvalOptions
data OutputTrace
data ReplHistory
data RedefinitionSet
data ConditionSnapshot
data RestartSnapshot
data FrameSnapshot

data DebugId
data DebugSession
data InspectorId
data Inspector
data InspectFocus
data WatchId
data Watch
data TraceSpec
```

Associated semantic structures:

```haskell
type Frames = Seq Frame
type Workers = FiniteMap WorkerName Worker
type DebugSessions = FiniteMap DebugId DebugSession
type Inspectors = FiniteMap InspectorId Inspector
type Watches = FiniteMap WatchId Watch

data BridgeState =
  BridgeState
    { image          :: Image
    , registry       :: MethodRegistry
    , workers        :: Workers
    , debuggers      :: DebugSessions
    , inspectors     :: Inspectors
    , watches        :: Watches
    , shutdown       :: ShutdownState
    }

data StepResult =
  StepResult
    { state' :: BridgeState
    , world' :: World
    , frames :: Frames
    }
```

Implementation-shaped types intentionally excluded from meaning:

- hash tables, mutexes, threads, socket objects, mailboxes, process ids,
  handler function pointers, JSON alists, counters except as fresh-name supply,
  and event-log bytes.

## Primitive Observations

The complete primitive observation is an action trace:

```haskell
step :: BridgeState -> World -> Action -> StepResult
```

Derived primitive-looking observations are projections through `step`:

```haskell
methods        :: BridgeState -> MethodRegistry
currentPackage :: WorkerName -> BridgeState -> PackageName
workersView    :: BridgeState -> WorkerSummarySet
debuggersView  :: BridgeState -> DebugSessionSummarySet
inspectorsView :: BridgeState -> InspectorSummarySet
watchesView    :: BridgeState -> WatchSummarySet
```

Public wire observations:

```haskell
renderFrame  :: Frame -> JsonObject
decodeRequest :: JsonObject -> DecodeErrorOr Action
renderCli    :: CliCommand -> Frames -> ExitText
```

Derived observations:

```haskell
ping              = projectDaemonSummary . step Ping
help m            = projectMethodSpec m . methods
diff w            = redefinitions . worker w
listDebugSessions = debuggersView
listWatches       = watchesView
listWorkers       = workersView
```

Operational non-semantics:

- precise OS pid, exact elapsed milliseconds, GC timing, log contents, socket
  path strings, and raw fresh integer values. These may be useful payloads but
  they do not define bridge equality.

## Candidate Denotations

Candidate A: bag of JSON handlers.

```haskell
denoteA :: Bridge -> Map MethodName (JsonParams -> JsonResponse)
```

Pros: matches the simplest v1 implementation and makes `methods` easy.

Cons: cannot explain streamed frames, query continuations, server-owned debug
sessions, workers, watches, connection close behavior, or restart
continuations. It makes method schemas documentation rather than constructors.

Rejected.

Candidate B: one linear REPL transcript.

```haskell
denoteB :: Bridge -> [ClientText] -> [ServerText]
```

Pros: captures interactive flavor and output order.

Cons: makes named workers, inspectors, watches, and concurrent streams look
like accidental interleavings. It cannot state that independent workers commute
up to schedule, or that debug sessions outlive the connection that observed
them.

Rejected.

Candidate C: raw SBCL image.

```haskell
denoteC :: Bridge -> Image
```

Pros: maximal SBCL power.

Cons: not hard to misuse; loses protocol closure, resource identities,
discovery, output caps, and public frame behavior. It also fails to explain
schema errors and CLI behavior.

Rejected.

Chosen denotation: a capability-indexed Mealy machine over an opaque SBCL
world.

```haskell
denote :: Bridge -> (BridgeState, World, Action) -> StepResult
denote bridge = step
```

This is the simplest precise model found so far:

1. `World` parameterizes SBCL, filesystem, clock, and schedule behavior
   without reimplementing Common Lisp semantics in the bridge algebra.
2. `BridgeState` contains exactly the persistent public resources an agent can
   later observe or drive.
3. `Action` is typed by the method registry; malformed JSON never enters the
   algebra.
4. `Frames` preserves ordered observations where order matters, while
   resource inventories use maps/sets where order does not matter.
5. Long-running and interactive behavior is represented by state transitions
   that create addressable capabilities, not by hidden connection state.

Representability restrictions:

- Only actions accepted by `decode` are bridge actions.
- Fresh ids are representable capabilities. Equality is alpha-equivalence over
  fresh `DebugId`, `InspectorId`, and `WatchId` names.
- A `DebugSession` is live only while the original dynamic continuation is
  parked in the debugger.
- A `FrameSnapshot` may contain unavailable variables; unavailable is a
  semantic result, not an implementation failure.

Partiality, errors, strictness, ordering, nondeterminism:

- Request errors are explicit `ProtocolError` terminals and do not mutate
  bridge state.
- Lisp errors are explicit eval/debug outcomes and may preserve a live
  continuation when debug mode is on.
- Frame order within one connection/request is semantic.
- Cross-worker/watch interleavings are schedule-parametric; compare two
  bridges under the same `World` schedule.
- Output is an ordered monoid capped by a prefix law.

## Equality

Bridge states are equal when all well-typed action streams produce the same
observable frames and final resource inventories under the same `World`, up to
consistent renaming of fresh capability ids.

```haskell
Law: "semantic equality"
forall b1 b2.
  (forall s w actions.
     run actions (denote b1) s w ==alpha run actions (denote b2) s w)
  => b1 = b2
```

Frame equality uses:

1. exact equality for method names, terminal success/error shape, condition
   snapshots, output prefixes, worker names, packages, and payload fields;
2. alpha-equivalence for fresh debug/inspect/watch ids;
3. schedule-parametric equality for concurrent event interleavings;
4. exclusion of operational diagnostics such as log byte counts.

No-leak rule:

```haskell
Law: "no public distinction after equal denotation"
forall x y.
  denote x = denote y
  => forall publicContext.
       observe publicContext x ==alpha observe publicContext y
```

Consequences:

- A socket connection is a delivery path, not ownership of a debug,
  query, inspector, or watch resource.
- Project root is part of the REPL carrier identity. Two different project
  roots denote different daemon images even when their forms, packages,
  worker names, or symbol names coincide.
- A reachable transport endpoint denotes a project daemon only when its
  liveness observation carries an opaque identity proof for that project.
  Token authentication without a project proof is transport reachability, not
  REPL ownership.
- A cache hit, thread reuse, method-handler arrangement, or fresh-id counter
  value may not alter semantic observations except through alpha-renamed
  capabilities.
- JSON and CLI renderers may hide implementation detail but may not invent
  semantic outcomes.

## Actions and Constructor Responsibility

Public constructors are typed actions. JSON params are not constructors until
`decode` accepts them.

```haskell
decode :: MethodRegistry -> JsonObject -> ProtocolErrorOr Action
step   :: BridgeState -> World -> Action -> StepResult
```

Action families:

| Family | Constructors | One semantic responsibility |
| --- | --- | --- |
| Discovery | `Ping`, `Methods`, `Help method` | observe daemon and registry facts |
| Lifecycle | `Shutdown`, CLI `Daemon mode` | start/observe/stop the service boundary |
| Worker | `CurrentPackage`, `SetPackage`, `ListWorkers`, `KillWorker`, `Reset`, `Interrupt` | manage worker state and current eval |
| Eval | `Eval`, `TimeEval`, `ProfileEval` | run one form in a worker with options |
| Condition/query | `QueryResponse`, `ListDebugSessions`, `DebugEvalInFrame`, `DebugInvokeRestart`, `DebugContinue`, `DebugAbort` | drive live continuations |
| Inspector | `Inspect`, `InspectInto`, `InspectPop`, `InspectEval`, `InspectMutate`, `InspectPage`, `InspectClose` | navigate or mutate a focused value |
| Watch | `Watch`, `ListWatches`, `Unwatch`, `Tick` | stream file-change induced loads |
| Source/image | `Apropos`, `Documentation`, `Arglist`, `CompleteSymbol`, `PackageInfo`, `ClassInfo`, `FunctionInfo`, `FindDefinition`, `Xref`, `Describe`, `DescribeSystem`, `Macroexpand`, `CompileFile`, `LoadFile`, `Disassemble`, `ImageInfo`, `LoadedSystems`, `ListPackages`, `Gc`, `Trace`, `Untrace`, `ListTraced`, `ListRedefinitions` | observe or mutate the SBCL image through standard Lisp capabilities |
| CLI core | `Daemon`, `Eval`, `Call` | provide the smallest command surface that can construct every public action |

Every public method in `src/repl.lisp` is covered by one row above.
The public CLI surface is intentionally not one subcommand per method. It is a
small constructor algebra:

```text
clpm repl daemon [--detach] [--no-load] [--status] [--stop]
clpm repl eval FORM [eval/debug flags]
clpm repl call METHOD [--PARAM VALUE | --params-json JSON]...
```

`daemon` is the lifecycle constructor. `eval` is a derived but privileged
constructor for the interactive REPL/debug loop because it must hold a
connection while debugger/query continuations are exchanged. `call` is the
generic typed constructor for every daemon method in the registry; it uses the
same method schema as the daemon, so convenience aliases are recipes rather
than public constructors.

Current RPC inventory from the method registry:

```text
ping, current-package, set-package, eval, interrupt, reset, list-workers,
kill-worker, describe, list-redefinitions, shutdown, watch, list-watches,
unwatch, methods, help, query-response, list-debug-sessions,
debug-invoke-restart, debug-eval-in-frame, debug-continue, debug-abort,
find-definition, xref, macroexpand, compile-file, apropos, documentation,
arglist, complete-symbol, package-info, class-info, function-info,
disassemble, describe-system, image-info, loaded-systems, list-packages, gc,
trace, untrace, list-traced, inspect, inspect-into,
inspect-pop, inspect-eval, inspect-mutate, inspect-page, inspect-close,
load-file
```

Legacy CLI wrappers that should disappear during the rewrite:

```text
serve, interrupt, ping, status, stop, methods, describe, diff,
image-info, list-packages, loaded-systems, describe-system, package-info,
current-package, set-package, apropos, complete-symbol, arglist, doc,
documentation, disassemble, function-info, class-info, find-definition,
find-definitions, xref, who-calls, who-references, who-sets, who-binds,
macroexpand, compile-file, load-file, gc, trace,
untrace, list-traced, workers/list-workers, kill-worker, reset, list-watches,
unwatch, watch, inspect, debug, list-debug-sessions, debug-eval-in-frame,
debug-invoke-restart, debug-continue, debug-abort
```

Each legacy wrapper must either become a `call METHOD ...` recipe, become part
of `daemon`, or disappear. Keeping a wrapper whose denotation is only
`Call method params` violates parsimony.

## Denotation Laws

### Decode and dispatch

```haskell
Law: "decode/closed"
forall registry json.
  decode registry json =
    ProtocolError e
    or TypedAction a where methodName a in domain registry
```

```haskell
Law: "decode/no-mutation"
forall s w bad.
  decode (registry s) bad = ProtocolError e
  => dispatch bad s w = (s, w, [ErrorFrame e])
```

```haskell
Law: "methods/domain"
forall s w.
  frames (step s w Methods) = [Result (methodSpecs (registry s))]
```

```haskell
Law: "help/lookup"
forall s w m.
  m in domain (registry s)
  => frames (step s w (Help m)) = [Result (methodSpec m)]
```

### Worker and package state

```haskell
Law: "worker/default-exists-on-observe"
forall s w.
  ListWorkers creates or observes a worker named "default"
```

```haskell
Law: "set-package/persistent"
forall s w worker p.
  packageExists p w
  => currentPackage worker (state' (step s w (SetPackage worker p))) = p
```

```haskell
Law: "eval/read-package"
forall s w worker form opts.
  package used to read form =
    case packageChoice opts of
      UseWorkerPackage -> currentPackage worker s
      UsePackageOnce p -> p
```

```haskell
Law: "package-override-scoped"
forall s w worker p form.
  currentPackage worker s = p0
  => currentPackage worker (state' (step s w (Eval worker (UsePackageOnce p) form))) = p0
```

```haskell
Law: "eval/package-persists-without-override"
forall s w worker form.
  packageChoice = UseWorkerPackage
  => currentPackage worker (state' (step s w (Eval worker UseWorkerPackage form)))
     = postEvalPackage reported by SBCL for that worker
```

```haskell
Law: "same-worker-serial"
forall a b worker.
  target a = worker and target b = worker
  => run [a,b] observes a before b
```

```haskell
Law: "project images are isolated"
forall projectA projectB actionsA actionsB.
  projectA /= projectB
  => run actionsA (repl projectA) cannot create, mutate, or observe
     bindings, workers, debugger sessions, inspectors, watches, traces, or
     history in repl projectB
```

```haskell
Law: "project-daemon-liveness-requires-proof"
forall project endpoint.
  tokenValid endpoint && missing (ping endpoint).projectId
  => endpoint does not denote repl project
```

```haskell
Law: "different-workers-commute-up-to-schedule"
forall a b workerA workerB.
  workerA /= workerB and disjointEffects a b
  => run [a,b] ==schedule run [b,a]
```

### Eval and REPL behavior

`SBCL.eval` is an external oracle:

```haskell
sbclEval :: Image -> WorkerContext -> EvalOptions -> Form -> LispOutcome
```

```haskell
Law: "eval/delegates-to-sbcl"
forall s w action.
  action = Eval worker packageChoice form opts
  => step s w action =
       bridgeWrap (sbclEval (image s) (workerContext s worker packageChoice) opts form)
```

```haskell
Law: "eval/history-success"
forall successful eval with values vs and form f.
  history' = shiftHistory history f vs
```

```haskell
Law: "eval/redefinition-log"
forall eval that reads a tracked top-level defining form d.
  d in redefinitions worker (state')
```

```haskell
Law: "output-prefix"
forall eval.
  terminalOutput eval = boundedPrefix (concat (streamEvents eval))
```

```haskell
Law: "resource-exhaustion-terminal"
forall eval limit.
  limitExceeded eval limit
  => terminal eval = Error resource-exhausted
     and no later success frame exists for that request id
```

### Conditions, query, and debugger

```haskell
Law: "eval/error-no-debug"
forall unhandled condition c.
  debug opts = false
  => terminal = Error eval-error (snapshot c)
     and no live DebugSession is created
```

```haskell
Law: "eval/error-debug"
forall unhandled condition c.
  debug opts = true
  => frames contain Event debugger-entered (snapshot c) debugId
     and debuggers state' contains debugId
```

```haskell
Law: "debug/session-not-connection-owned"
forall s w debugId connection.
  close connection may stop delivery to connection
  but does not remove debugId from debuggers s
```

```haskell
Law: "debug-eval-in-frame/non-consuming"
forall debugId frame form.
  DebugEvalInFrame debugId frame form
  returns frame values or frame-eval error
  and debugId remains live
```

```haskell
Law: "debug-restart/resumes-continuation"
forall debugId restart args.
  validRestart debugId restart args
  => DebugInvokeRestart debugId restart args
     resumes the original dynamic continuation at the signal point
     and removes debugId when the continuation leaves the debugger
```

```haskell
Law: "debug-bad-restart-args-preserve-session"
forall debugId restart badArgs.
  argsDoNotReadOrEvaluate badArgs
  => DebugInvokeRestart debugId restart badArgs returns Error
     and debugId remains live
```

```haskell
Law: "debug-abort/removes-session"
forall debugId.
  DebugAbort debugId removes debugId and lets the eval unwind as failure
```

```haskell
Law: "query-response/same-id"
forall evalId value.
  QueryResponse evalId value is accepted only when evalId has a live query
  waiting; otherwise it is ProtocolError
```

### Inspector

An inspector is a zipper over an object graph in the image.

```haskell
Law: "inspect/open"
forall form.
  Inspect form creates fresh inspector id i and focus = valueOf form
```

```haskell
Law: "inspect-into-pop"
forall i part.
  validPart i part
  => InspectPop (InspectInto i part) = i
```

```haskell
Law: "inspect-page/window"
forall i offset.
  InspectPage i offset = boundedWindow offset pageSize (parts (focus i))
```

```haskell
Law: "inspect-eval/focus-binding"
forall i form.
  InspectEval i form evaluates form with * bound to focus i
```

```haskell
Law: "inspect-close/removes"
forall i.
  InspectClose i removes i; later inspector actions on i are protocol errors
```

### Watches

```haskell
Law: "watch/open"
forall dir glob mode.
  directoryExists dir
  => Watch dir glob mode creates fresh watch id and emits watch-acknowledged
```

```haskell
Law: "watch/tick-reload"
forall watch file.
  fileMatches watch file and mtimeIncreased file
  => Tick emits file-reloaded or reload-failed
     and successful reload mutates image as LoadFile file
```

```haskell
Law: "watch/auto-revert"
forall watch file.
  autoRevert watch and successful reload file
  => Tick emits revert-applied for file
```

```haskell
Law: "unwatch/idempotent"
forall id.
  id not in watches => Unwatch id returns stopped=false and preserves state
```

### Source, image, and trace operations

Read-only image observations do not mutate image state:

```haskell
Law: "image-observation/read-only"
forall op in {Apropos, Documentation, Arglist, CompleteSymbol, PackageInfo,
              ClassInfo, FunctionInfo, FindDefinition, Xref, Describe,
              DescribeSystem, Disassemble, ImageInfo, LoadedSystems,
              ListPackages, ListTraced, ListRedefinitions}.
  image (state' (step s w op)) = image s
```

Image-mutating operations delegate to SBCL:

```haskell
Law: "compile-file/delegates"
  CompileFile path = bridgeWrap (SBCL.compileFile image path)
```

```haskell
Law: "load-file/delegates"
  LoadFile path = bridgeWrap (SBCL.load image path)
```

```haskell
Law: "macroexpand/delegates"
  Macroexpand form recursive = bridgeWrap (SBCL.macroexpand image form recursive)
```

```haskell
Law: "trace/idempotent"
forall symbols.
  Trace symbols <> Trace symbols = Trace symbols
```

```haskell
Law: "untrace/clears"
forall symbols.
  ListTraced after Untrace symbols excludes symbols
```

```haskell
Law: "gc/observational"
  Gc may change operational memory facts but must not change Lisp values,
  packages, workers, debug sessions, inspectors, watches, or method schemas
```

### Lifecycle

```haskell
Law: "shutdown/resolves-owned-resources"
forall s w.
  Shutdown interrupts or resolves live eval/debug/watch resources, emits
  success, and transitions to ShutdownRequested
```

```haskell
Law: "status/cleanup-stale"
  CLI Status and Stop may clean stale daemon files, but stale file cleanup is
  operational; it is not a bridge-state transition unless a live daemon exists
```

## Algebraic Structures

1. `Frames` is a monoid under concatenation, with empty trace as identity.
   It is not commutative.
2. `OutputTrace` is a monoid per channel. Terminal output is a bounded-prefix
   homomorphism from the full trace.
3. `MethodRegistry` is a finite map from method name to typed schema.
   `decode` is a partial algebra homomorphism from JSON objects into
   `Action + ProtocolError`.
4. `RedefinitionSet` is an idempotent semilattice keyed by definition identity.
5. `Workers`, `DebugSessions`, `Inspectors`, and `Watches` are finite maps.
   Insertions allocate fresh names; equality is alpha-equivalence over those
   fresh names.
6. `ReplHistory` is a fixed-width shift register, not a list monoid.
7. Worker-targeted actions are lens-like state transitions over
   `Workers[workerName]`.
8. CLI commands form a rendering morphism from action traces to
   `(exit-code, stdout, stderr)`.

## Interface and Protocol Morphism Checks

### JSON-RPC dispatch

```haskell
Law: "dispatch/decode-step"
forall json s w.
  decode (registry s) json = TypedAction a
  => dispatch json s w = step s w a
```

Failure pressure: if unknown params are ignored, `decode` is not closed and
`methods/help` are not the source of truth. Repair: method specs are
constructive schemas, not descriptive docs.

### Methods/help discovery

```haskell
Law: "help-methods-morphism"
forall m.
  Help m = lookup m Methods
```

Failure pressure: hand-written docs can drift from dispatch. Repair:
`methods` and `help` are projections of the same registry used by `decode`.

### CLI rendering

```haskell
Law: "cli/render-morphism"
forall command action s w.
  desugarCli command = action
  => runCli command s w = renderCli command (frames (step s w action))
```

Core CLI constructors:

```haskell
Daemon Start opts        = lifecycleStart opts
Daemon Status            = lifecycleStatus
Daemon Stop              = lifecycleStop

Call method params       =
  dispatch (encode method params)

Eval form opts           =
  Call "eval" (evalParams form opts)
  with an interactive continuation policy for debug/query events
```

Rejected derived wrappers:

```haskell
Doc symbol args          = Call "documentation" ...
Workers                  = Call "list-workers" ...
Diff worker              = Call "list-redefinitions" ...
WhoCalls symbol          = Call "xref" --direction calls ...
Inspect form opts        = Call "inspect" ...
Debug form selectors     = Eval form debug=true selectors
```

Failure pressure: one wrapper per RPC recreates the bag-of-handlers model at
the CLI layer. Repair: expose `Call` as the typed morphism from CLI syntax to
the method registry, and keep only `Eval` where the connection-continuation law
requires a specialized client loop.

### Worker product

```haskell
Law: "worker-lens-morphism"
forall worker action.
  target action = worker
  => step action over BridgeState =
       over workers[worker] (workerStep action)
```

Failure pressure: global package state would make named workers misleading.
Repair: package, history, and redefinition log live in `Worker`.

### Streaming/output

```haskell
Law: "terminal-output-is-stream-projection"
forall eval.
  terminal.output = boundedPrefix (foldMap eventChunk stdoutEvents)
```

Failure pressure: buffering unbounded output and truncating after eval makes
memory behavior non-semantic. Repair: bounded sink is the model.

### Debug same-id vs session-id

```haskell
Law: "debug-addressing-morphism"
forall action debugId.
  sameIdContinuation action debugId == freshSessionRequest action debugId
  where both name the same live DebugSession
```

Failure pressure: connection-owned continuation tables make fresh session
requests impossible. Repair: debug sessions are server-owned resources.

### Inspector zipper

```haskell
Law: "inspector-zipper-morphism"
  InspectInto and InspectPop preserve the usual zipper focus/path laws;
  InspectEval is evaluation in the denoted focus context.
```

Failure pressure: one-shot inspect that closes immediately cannot express
held traversal. Repair: single-shot CLI is derived from open/traverse/close;
RPC inspector sessions remain first-class.

### Rejected interfaces

| Interface | Smallest counterexample | Failure | Response |
| --- | --- | --- | --- |
| Commutative set of frames | `(format t "a")` then `(format t "b")` | `ab` and `ba` differ | use ordered `Seq Frame` |
| Stateless request handler | debug eval enters debugger, client reconnects | no place to store continuation | use `DebugSessions` map |
| Raw JSON byte equality | two equivalent runs allocate debug ids 1 and 2 | raw bytes differ but capability behavior is same | compare up to fresh-id alpha-equivalence |
| Public monad over actions | `Watch` may remain live and emit later frames | bind would hide scheduling and liveness | expose transition system, not monad API |
| Connection owns debug session | close discovery connection before restart | restart cannot resume | debug sessions are server-owned |

## Pressure Iterations

Iteration 1: bag of handlers.

- Candidate tried: `Map MethodName Handler`.
- Evidence/law pressure: debugger continuations, query responses, watches, and
  inspectors require live resources; `connection-not-owner` fails.
- Simplification made: replace handlers with typed `Action` over
  `BridgeState`.
- Remaining weakness: state machine could still be too implementation-shaped.
- Next pressure test: named workers and CLI wrappers.

Iteration 2: one transcript.

- Candidate tried: linear REPL transcript.
- Evidence/law pressure: independent named workers, watch streams, and
  inspector sessions are not one sequence of REPL input/output.
- Simplification made: model bridge as finite maps of addressable resources
  plus ordered per-action frames.
- Remaining weakness: arbitrary SBCL effects would make laws huge.
- Next pressure test: delegate Lisp semantics to an opaque `World`.

Iteration 3: raw SBCL image.

- Candidate tried: bridge denotes only the hosted image.
- Evidence/law pressure: protocol errors, method discovery, output caps,
  sessions, and CLI behavior are not image facts.
- Simplification made: `Image` is one field in `BridgeState`, and SBCL is an
  oracle used by eval/source operations.
- Remaining weakness: fresh ids and scheduling may leak.
- Next pressure test: equality.

Iteration 4: exact trace equality.

- Candidate tried: compare JSON frames byte-for-byte.
- Evidence/law pressure: capability ids are observable integers but should not
  define user reasoning; independent scheduling can interleave differently.
- Simplification made: frame equality is alpha-equivalence for fresh
  capabilities and schedule-parametric for independent streams.
- Remaining weakness: operational diagnostics such as pid/time remain noisy.
- Next pressure test: classify them as payload observations, not equality.

Iteration 5: complete observation set.

- Candidate tried: `step` plus queryable resource inventories.
- Evidence/law pressure: "no invisible state" requires every persistent
  resource to be discoverable, but logs/caches/locks should not become meaning.
- Simplification made: public resources are semantic; implementation
  diagnostics are operational non-semantics unless surfaced by a typed method.
- Remaining weakness: none that changes denotation for the current surface.
- Next pressure test: property laws over generated action sequences.

Iteration 6: broad CLI wrapper surface.

- Candidate tried: one CLI subcommand for most RPC methods.
- Evidence/law pressure: nearly every wrapper denotes `Call method params`,
  so the CLI has many constructors with no independent semantic law. This
  violates constructor parsimony and makes documentation drift more likely.
- Simplification made: collapse the CLI to `daemon`, `eval`, and `call`.
- Remaining weakness: `eval` is still special.
- Next pressure test: `eval` must justify itself by connection-held
  debug/query continuation laws; otherwise it should collapse into `call`.

## Property-Test Plan

Generate only well-typed actions through the method registry, plus malformed
JSON for decoder tests.

1. Decode closure: unknown method, unknown param, missing required param, and
   wrong type all produce `ProtocolError` and preserve state.
2. Method registry: every registered method appears in `methods`; `help m`
   equals lookup from `methods`.
3. Package laws: `set-package` persists; per-eval package override is scoped;
   reading after `(in-package ...)` uses the intended package.
4. Worker laws: actions for disjoint named workers preserve each other's
   package/history/redefinition observations.
5. Output law: streamed chunks concatenate to the terminal bounded prefix.
6. Debug laws: `debug` creates a discoverable session; frame eval does not
   consume it; bad restart args preserve it; restart/continue/abort resolve it.
7. Connection law: closing the discovery connection does not remove a kept
   debug session.
8. Inspector laws: open/into/pop/page/eval/close satisfy zipper behavior on
   lists, vectors, hash tables, and objects with bounded rendering.
9. Watch laws: watch acknowledgement creates a discoverable watch; unwatch is
   idempotent; file modification emits reload or failure; auto-revert emits
   revert-applied after successful load.
10. CLI morphism: `call METHOD` produces the same frames as directly
    dispatching the typed action for METHOD; all old wrapper names are absent
    or documented as recipes.
11. Lifecycle: shutdown resolves kept sessions and watches before worker
    teardown.

Existing evidence lives mostly in `test/repl-*-test.lisp`; future
properties should be phrased against these laws rather than private structs.

## Open Semantic Decisions

None of these block the algebra gate:

1. Exact SBCL semantics remain externalized as `World`. This is deliberate:
   the bridge algebra specifies orchestration and observation, not Common Lisp.
2. Exact elapsed time, pid, and GC counters are diagnostic payloads. They may
   vary without changing bridge equality.
3. The event log is diagnostic unless a future RPC exposes it as a typed
   observation. If that happens, add log laws.

## Quality Gate

Score scale: 0 absent, 1 informal, 2 common cases only, 3 precise and
law-backed, 4 precise/simple/reusable/morphism-checked.

| Criterion | Score | Evidence |
| --- | --- | --- |
| Denotational fit | 4 | explicit `step` model explains eval, debug, inspector, watches, discovery, CLI |
| Simplicity | 3 | SBCL effects are parameterized; resources are finite maps; frames are a sequence |
| Compositionality | 3 | every public operation maps to a typed action family and step law |
| Semantic equality and abstraction safety | 3 | equality is complete observation up to fresh-id alpha and schedule |
| Closure | 3 | `decode` is the constructor gate; malformed JSON is outside `Action` |
| Power | 4 | covers the full current RPC/CLI surface and live SBCL affordances |
| Parsimony | 3 | action families separate worker, eval, debug, inspect, watch, source, lifecycle |
| Orthogonality | 3 | workers/debuggers/inspectors/watches are independent sub-algebras |
| Law quality | 3 | laws cover observations, interaction, partiality, and resource lifetime |
| Interface morphisms | 3 | dispatch, methods/help, CLI, worker lens, streaming, debug addressing checked |
| Generality | 3 | passive payloads parameterized; image/world opaque |
| Implementation independence | 4 | no law depends on threads, sockets, hash tables, or JSON alist representation |

Gate result: pass for specification. Implementation remains out of scope until
a user explicitly asks for it.

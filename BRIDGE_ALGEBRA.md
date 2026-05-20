# `clpm repl-bridge` algebra

This note is the semantic target for the bridge. The bridge should not be
understood as a collection of JSON handlers. It should be understood as a
state machine over a live Lisp image, with JSON and the CLI as views of that
machine.

## Intent

The bridge exists so an agent can use a Common Lisp image with REPL-quality
feedback:

- evaluate forms in persistent package and history state;
- observe values, stdout, stderr, conditions, restarts, and source locations;
- recover through the condition system without re-running side effects;
- inspect object graphs;
- keep long-running processes observable and interruptible;
- ask the daemon what operations and live state exist.

Out of scope: human editor integration, replacing SLIME/Slynk, or making
every SBCL implementation detail portable.

## Carrier Types

```text
BridgeState =
  { image
  , workers       : WorkerName -> Worker
  , processes     : ProcessId -> Process
  , inspectors    : InspectorId -> Inspector
  , watches       : WatchId -> Watch
  , event_log     : EventLog
  }

Worker =
  { name
  , package       : Package
  , history       : ReplHistory
  , redefinitions : RedefinitionSet
  , current       : Maybe ProcessId
  }

Process =
    Running EvalProcess
  | WaitingForQuery QuerySession
  | InDebugger DebugSession
  | Completed TerminalOutcome

TerminalOutcome =
    EvalSuccess Values OutputSnapshot ReplStateDelta
  | EvalFailure ConditionSnapshot OutputSnapshot
  | Interrupted
  | ResourceExhausted ResourceLimit
  | WorkerDied

ServerFrame =
    Event ProcessId EventPayload
  | Result RequestId ResultPayload
  | Error RequestId ErrorPayload
```

The central observation is:

```text
step : BridgeState x ClientAction -> BridgeState x [ServerFrame]
```

Every RPC, CLI command, continuation, interrupt, query reply, watch tick, and
debugger action should be explainable as a `step`.

## Equality

Two bridge states are observationally equal when every sequence of client
actions produces the same sequence of public frames and leaves equivalent live
processes, workers, inspectors, watches, and event-log facts.

Transport identity is not semantic equality. A Unix socket connection is a
delivery path, not ownership of a debug/query process.

## Core Laws

Law: `eval/read-package`

```text
forall worker form.
  read package used by eval(worker, form) = worker.package
```

After `(in-package :p)`, the next unqualified symbol must be interned in
package `P`, not in the package from which the daemon was implemented.

Law: `eval/history`

```text
successful eval(form, values) shifts *, **, ***, +, ++, +++, /, //, ///
as a normal CL REPL would.
```

Law: `package-override-is-scoped`

```text
eval(form, package = P) observes P while reading and evaluating form,
but does not mutate worker.package.
```

Law: `condition-live-debugger`

```text
unhandled condition with debug=true
  => emits debugger-entered and creates a live DebugSession
```

The session remains addressable until a restart, continue, or abort action
resolves it.

Law: `restart-preserves-continuation`

```text
invokeRestart(session, restart, args)
  resumes the original dynamic continuation at the signaling point.
```

This is the condition-system law that makes the bridge Lisp-like rather than
remote `eval`.

Law: `frame-eval`

```text
debugEval(session, frame, form)
  = evaluate form in the lexical context of frame
```

On SBCL this means `sb-di:eval-in-frame`, not a reconstructed `LET` of guessed
locals. If SBCL reports a source variable as unavailable at the suspended
program counter, the bridge must surface that fact rather than inventing a
binding.

Law: `output-prefix`

```text
terminal.output(channel) = boundedPrefix(concat(streamEvents(channel)))
```

Streaming and terminal capture are two observations of the same output trace.
Large output must be bounded while it is written, not only after evaluation.

Law: `closed-request-algebra`

```text
decode(json) = Either ProtocolError TypedRequest
```

Handlers consume typed requests. Method schemas are constructors, not just
documentation.

Law: `connection-not-owner`

```text
closing connection C may stop frames being delivered to C,
but it does not destroy an unresolved DebugSession or QuerySession.
```

## Findings Driving This Refactor

1. Package state is semantically wrong. `%eval-one` reads the form before
   binding `*package*` to the worker package. Persistent package state affects
   evaluation but not reading, so `(in-package :p)` followed by `(defun foo ...)`
   interns `FOO` in the wrong package.

2. Debug sessions are connection-owned. Continuations are routed through the
   per-connection in-flight table, so a debug stop cannot be resumed from a
   later one-shot CLI command. That is unpleasant for agent tooling and
   violates `connection-not-owner`.

3. `debug-eval-in-frame` reconstructed a local environment instead of using
   SBCL's frame evaluator. This is observable: a live argument can be evaluated
   from the actual frame, while an optimized-away source local should produce a
   precise frame-eval error instead of a guessed value.

4. Output capture is not a true bounded stream. The normal capture path writes
   into unbounded string streams and truncates after evaluation. The law should
   bound memory during writes.

5. Request schemas are descriptive, not constructive. Unknown fields are
   ignored and required/typed fields are checked manually in handlers. This
   makes the public algebra larger and less precise than the implementation.

6. Terminal outcomes mix protocol errors, Lisp errors, compatibility fields,
   warnings, and eval payloads. Internal code should have typed outcomes;
   JSON compatibility should be a renderer.

## Implementation Direction

The bridge should be simplified in this order:

1. Fix `eval/read-package` and strengthen tests around package state.
2. Replace guessed frame eval with `sb-di:eval-in-frame` and require the live
   frame-argument case to pass.
3. Introduce server-owned process/debug-session identities so debug sessions
   can survive the connection that discovered them.
4. Replace output capture with bounded character streams shared by streaming
   and non-streaming eval.
5. Introduce request decoders from method specs and remove handler-local
   ad hoc parsing.
6. Separate internal terminal outcome types from their JSON rendering.

Each step should move code toward `step : BridgeState x ClientAction ->
BridgeState x [ServerFrame]` and delete compatibility shims that preserve the
old bag-of-handlers model.

## Current State

Implemented:

1. `eval/read-package`: forms are now read in the worker package, with
   per-request package overrides scoped to that eval.
2. `frame-eval`: stopped frames are captured as live SBCL frame objects and
   `debug-eval-in-frame` uses `sb-di:eval-in-frame`.
3. `connection-not-owner`: debugger stops have server-owned session ids. A
   later connection can list sessions, evaluate in frames, invoke restarts,
   continue, or abort.
4. `restart-preserves-continuation`: fresh session-addressed restart actions
   resume the original eval. Bad restart argument forms report an error without
   consuming the debug session.
5. `output-prefix`: streaming and terminal output share a bounded sink, so both
   observations expose the same prefix and large writes are bounded as they
   occur.
6. `closed-request-algebra`: registered method specs now form a decode gate.
   Unknown params, non-object params, and wrong JSON types fail as protocol
   errors before handlers run. Transport-wide `token` and dispatch-wide
   `explain` remain explicit implicit params.
7. Terminal responses now have an internal `terminal-response` representation;
   the outer `{id,result/error}` JSON frame is rendered at the wire boundary,
   and eval-specific markers such as `truncated` / `worker_restarted` live in
   the eval result rather than as extra terminal-frame fields. Eval payloads
   remain structured as `eval-result` until their method renderer chooses
   success or error.
8. Shutdown now resolves active debug sessions before stopping workers, so a
   kept debugger stop cannot wedge daemon teardown.

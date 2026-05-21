# `clpm repl` v2 — the design for "actually using SBCL"

The v1 bridge (see `BRIDGE.md`) shipped a stateless one-shot eval channel:
LLM sends a form, daemon evaluates it, daemon returns one line of JSON.
That is enough for the bare loop ("define a function, see if it returns
the right value") but it is *not* enough to use SBCL. The killer features
of Common Lisp — restart-driven recovery, the inspector, in-image source
introspection, the tracer, the compiler's structured diagnostics — are
either invisible or read-only from outside the image.

This document is the plan for closing that gap. The goal is **an LLM
should be able to drive SBCL with the same authority a SLIME user has**:
enter the debugger and pick a restart, eval in any stack frame, inspect
a tangled object, find a function's source, hot-reload one method,
profile a hot loop, see compiler notes with source locations, get the
arglist of an unfamiliar function, and complete a partial symbol. With
that, "AI-assisted Common Lisp development" can mean something better
than "Python with macros."

The v1 surface stays. Everything below is *additive* — old clients keep
working, new flags turn the new capabilities on. The protocol is the
same line-delimited JSON over the same Unix-socket/TCP transport. What
changes is that a single request can produce a *stream* of messages and
can be answered out-of-band by the client while in flight.

---

## Design principles

These are the constraints I want the v2 design measured against. They
exist to prevent feature creep and to bias decisions toward
"pleasant for an LLM to drive."

1. **Every value an LLM might want, structured.** No more parsing
   `princ-to-string` output to recover a backtrace frame's source
   location, or grepping `apropos` text to find symbols of a given
   kind. If SBCL has it, the bridge surfaces it as a typed JSON field.

2. **No invisible state.** Every persistent thing the daemon holds —
   current package, history bindings, traced functions, watched files,
   bound handlers, named workers — must be queryable via an RPC. The
   LLM should be able to ask "what's the state of the daemon?" and
   get a complete answer.

3. **Backward compatible.** A v1 client sending a v1 `eval` still
   gets a v1 response. New behavior is opt-in via flags / new methods.
   Tests under `test/repl-*` continue to pass.

4. **One transport, one wire format, many shapes.** Line-delimited
   JSON over the existing socket. No websockets, no binary
   framing, no second port. The shape change is that some
   methods now produce a *sequence* of frames on the same
   connection instead of a single response.

5. **Server-streamed and bidirectional during eval.** A long-running
   form can emit stdout chunks, trace lines, and progress notifications
   to the client as it runs; an eval that calls `(yes-or-no-p ...)`
   pauses and asks the client. The client can interrupt at any point.

6. **Defensive everywhere.** A buggy `print-object` method must not
   blank out the response. A 50 MB readback must not OOM the LLM. A
   circular structure must not hang the daemon. Every print and
   inspect path uses bounded, pretty-print-controlled rendering with
   a fallback.

7. **No magic that hides Lisp semantics.** The LLM should be able to
   tell whether a value it sees is the primary value or one of
   multiple values, whether output came from `*standard-output*` or
   `*error-output*`, whether a condition was *signaled and handled*
   (lurking, possibly meaningful) or *unhandled* (the eval crashed).

8. **Discoverable.** `clpm repl call methods` enumerates every RPC
   the daemon answers. `clpm repl call help --method <method>` returns its
   parameter schema and one-line description. An LLM that knows
   nothing about the bridge can introspect its way to competence.

---

## Protocol upgrade (foundation)

The single-line-request / single-line-response model of v1 is too
narrow for restart-driven debugging or streamed output. v2 keeps the
wire format (line-delimited JSON, one object per line) but allows
*either* side to send multiple messages per request, all correlated by
the request's `id`.

### Frame types

Every frame is a JSON object on its own line. Frames carry an `id`
matching the originating request. Server-to-client frames carry one
of four shapes:

```jsonc
// Non-terminal: an in-progress notification on this request.
{"id": 7, "event": "stdout", "data": "loading...\n"}

// Terminal success.
{"id": 7, "result": { ... }}

// Terminal failure (request-level, not eval-level).
{"id": 7, "error": {"code": "...", "message": "..."}}

// Server-initiated request to the client. Client must answer with a
// frame carrying the same `id` and the `query-response` method.
{"id": 7, "event": "query", "prompt": "Continue? ",
 "response_type": "boolean"}
```

Client-to-server frames keep the v1 shape *plus* one new use:
follow-up messages on an *in-progress* request reuse its `id`.

```jsonc
// v1-style new request.
{"id": 8, "method": "eval", "params": {"form": "..."}}

// Continuation of request 7 (e.g., the worker is paused in the
// debugger; the LLM picks a restart).
{"id": 7, "method": "debug-invoke-restart",
 "params": {"name": "USE-VALUE", "args": ["0"]}}

// Answering a `query` event from the server.
{"id": 7, "method": "query-response", "params": {"value": true}}
```

A connection stays open for the lifetime of a logical session. v1
clients that close after one round-trip continue to work — the daemon
just notices EOF and stops emitting events for that id.

### Request lifecycle

```
client                                 daemon
  ──── {id:1, method:eval, ...} ───►
                                    spawn eval on worker
  ◄──── {id:1, event:stdout, ...}
  ◄──── {id:1, event:stdout, ...}
                                    eval finishes
  ◄──── {id:1, result:{...}}
  (no more frames for id=1)
```

Or, with the debugger:

```
  ──── {id:1, method:eval, --debug, ...} ───►
                                    eval signals error
  ◄──── {id:1, event:debugger-entered, session:{...}}
  ──── {id:1, method:debug-eval-in-frame, params:{frame:2, form:"x"}} ───►
  ◄──── {id:1, event:frame-eval-result, value:"42"}
  ──── {id:1, method:debug-invoke-restart, params:{name:"USE-VALUE",args:["0"]}} ───►
  ◄──── {id:1, event:stdout, data:"continuing\n"}
  ◄──── {id:1, result:{values:["0"], ...}}
```

### Backward-compatibility rules

- A request that does not use any v2-only flag *must* receive exactly
  one response frame (a v1-shape `result` or `error`). The daemon
  must not emit `event` frames unless the client opted in.
- The opt-in is per-method:
  - `eval` opts in with `--stream`, `--debug`, or `--query-interactive`.
  - `compile-file`, `load-file` always stream diagnostics.
  - `trace`, `inspect`, `watch` are inherently session-shaped.

### Tickets in this section

- **`#101 P0 protocol` Multi-frame responses on a kept-open connection.**
  Today `%handle-connection` reads one request, writes one response,
  closes. Replace with a loop: read frame, dispatch, write zero-or-more
  events, write one terminal frame; if the client sends a new
  top-level method on the same connection, handle it. EOF on the
  client side terminates the loop. Acceptance: an integration test
  opens one TCP/Unix connection, runs three `ping`s back-to-back, and
  reads three responses without reopening.

- **`#102 P0 protocol` Correlated continuation messages.** Per-id
  routing table inside the connection handler so a follow-up
  `debug-invoke-restart` finds the worker mailbox that the originating
  `eval` blocks on. Acceptance: a unit test posts an eval, receives
  `debugger-entered`, sends `debug-invoke-restart`, receives the
  result — all on one connection — without race.

- **`#103 P0 protocol` Streamed stdout/stderr events.** When
  `eval --stream`, the captured stream is flushed as a `stdout` /
  `stderr` event every 64 KB *or* on `force-output` *or* at most every
  100 ms (whichever first). The terminal `result` still includes the
  full string. Acceptance: a form that prints 10 lines with a `sleep`
  between each yields ≥9 `stdout` events.

- **`#104 P0 protocol` Bidirectional `query` for interactive I/O.**
  Bind `*query-io*` and `*standard-input*` to a stream backed by the
  request's id; on read, emit `event: query`, block the worker, wait
  for `query-response`. Default behavior (no `--query-interactive`)
  remains v1: reads return EOF. Acceptance: with the flag,
  `(yes-or-no-p "?")` round-trips to the client and back.

- **`#105 P0 protocol` Interrupt during streaming.** `interrupt` is
  already async; verify that interrupting a long-running `--stream`
  eval cleanly emits an `event: interrupted` and terminates with an
  error frame, not a stuck connection.

- **`#106 P1 protocol` `methods` and `help` RPCs.** Daemon serves
  `{id:N, method:"methods"}` returning a list of every RPC with its
  parameter schema and a one-line description. `help` with a method
  name returns long-form documentation. Acceptance: the response is
  generated *from the same source* the dispatcher reads (one source of
  truth — drift between docs and code is a recurring failure mode).

---

## Condition system & debugger

The lever that makes CL distinct from every other LISP-shaped REPL is
the condition system. v1 surfaces conditions as JSON strings *after*
they unwind. v2 keeps the worker paused at the error so the LLM can
do what a SLIME user would: see the frame, choose a restart, evaluate
expressions in the frame to figure out what happened.

A "debug session" is a piece of worker state: the original condition,
the live restart objects, the captured frames, and an inbound action
mailbox. The eval call's stack stays unwound *only* if the LLM aborts;
if it picks a restart, the worker invokes it and the eval continues.

### Tickets

- **`#110 P0 debug` Rich condition serialization.** Replace the
  current `{type, message, restarts:[name...], backtrace:[string...]}`
  with:

  ```jsonc
  {
    "type": "SIMPLE-ERROR",
    "message": "Division by zero",
    "report": "the principled-to-string render",
    "slot_values": {"format-control": "...", "format-arguments": [...]},
    "restarts": [
      {"name": "USE-VALUE",  "report": "Use a value", "interactive": true,
       "args_arity": 1},
      {"name": "STORE-VALUE","report": "Store a value", "interactive": true,
       "args_arity": 1},
      {"name": "ABORT",      "report": "Abort", "interactive": false,
       "args_arity": 0}
    ],
    "backtrace": [
      {"i": 0, "name": "FOO", "args": ["3"], "source": "src/x.lisp:42",
       "vars": {"X": "3"}},
      {"i": 1, "name": "BAR", "args": [], "source": null, "vars": {}},
      ...
    ]
  }
  ```

  Frame `vars` is best-effort and may be empty when SBCL can't
  reconstruct the lexenv (heavy optimization, tail-call elimination).

  Acceptance: a known-shape condition (e.g., `simple-type-error`) round-trips
  its slots; `interactive` reflects whether the restart was created with
  `:interactive-function`; `args_arity` is computed from that function or
  defaults to 0/1 conservatively.

- **`#111 P0 debug` `eval --debug` enters an interactive debug session.**
  On error, instead of `handler-case`'ing into a terminal response, the
  worker enters `handler-bind` with a handler that:
  1. Captures the condition + restarts (live objects) into a debug session.
  2. Emits `event: debugger-entered`.
  3. Blocks on a debug-session mailbox.

  Subsequent client messages on the same id drive that mailbox. The
  eval call's continuation is preserved; if the LLM picks a restart,
  the worker invokes it and the form continues. If the LLM picks
  `abort` (or sends `debug-abort`), the handler returns normally and
  the outer `handler-case` produces the v1-shape `eval-error` response.

  Acceptance: `eval --debug '(/ 1 0)'` enters the debugger; a
  follow-up `debug-invoke-restart {"name":"ABORT"}` produces a
  terminal `error` frame; a follow-up `debug-invoke-restart` with
  `{"name":"USE-VALUE","args":["0"]}` produces a terminal `result`
  with `value: "0"`.

- **`#112 P0 debug` `debug-eval-in-frame frame form`.** Reads the
  captured locals for `frame` and evaluates `form` in a lexenv that
  shadows them. Uses `sb-debug:var` / `sb-di:frame-debug-vars`. Output
  capture is independent of the outer eval (its own bounded sink).
  Returns `event: frame-eval-result` with `values`, `output`,
  `error_output`. Errors in frame eval re-enter the debugger as a
  *nested* session if the user asked for `--debug` at the outer eval.

  Acceptance: in the debugger for `(let ((x 7)) (error "x=~A" x))`,
  `debug-eval-in-frame frame=<top> form="(* X 2)"` returns `value: "14"`.

- **`#113 P0 debug` `debug-invoke-restart name [args]`.** Looks up the
  restart by name in the captured list and invokes it. Args are
  *forms* (strings), read and evaluated in the worker's package before
  being passed. If the restart's `:interactive-function` would prompt,
  v2 short-circuits by using the provided args directly.

  Acceptance: works for `ABORT`, `CONTINUE`, `USE-VALUE`,
  `STORE-VALUE`, and any user-defined restart in `restart-case`.

- **`#114 P0 debug` `debug-continue` / `debug-abort`.** Sugar over
  `debug-invoke-restart` for the two most common cases (`CONTINUE` for
  `cerror`, `ABORT` to give up). Both return terminal frames.

- **`#115 P1 debug` `break` produces a debug session, not an error.**
  `(break "...")` calls the SBCL break system. In a v2 daemon under
  `--debug`, that surfaces as `event: debugger-entered` with
  `kind: "break"`. The lone restart is `CONTINUE`. The LLM can
  inspect, eval in frame, then `debug-continue`, and the eval
  proceeds. Acceptance: `(progn (defparameter *x* 1) (break "look")
  (incf *x*))` round-trips; after a `debug-continue` the resulting
  `*x*` is 2.

- **`#116 P1 debug` `*break-on-signals*` per eval.** `eval
  --break-on '(or warning style-warning)'` binds
  `*break-on-signals*` to that type-specifier for the duration of the
  form. Lets the LLM say "tell me where this style-warning is coming
  from" instead of grepping output.

- **`#117 P1 debug` Declarative `--handlers` for non-interactive
  recovery.** `eval --handlers '((division-by-zero (use-value 0))
  (undefined-function (use-value identity)))'` binds handlers before
  evaluating the form. The handler for each condition type invokes the
  named restart with the given args, *without* entering the debugger.
  Useful when the LLM already knows what to do and doesn't want a
  round trip per error.

  Acceptance: `eval --handlers '((division-by-zero (use-value 999)))'
  '(/ 1 0)'` returns `value: "999"`.

- **`#118 P2 debug` Signal recording.** A `handler-bind` on
  `condition` (everything, including non-error conditions) at the
  outermost layer records `{type, message, handled-by:<class>}` into
  `result.signaled_conditions`. Warnings and ignored signals become
  visible. Cheap; opt-in via `eval --record-signals` to avoid the
  per-signal overhead by default.

---

## Inspector

`inspect` in CL is the single best tool for navigating a tangled
data structure. A class with a circular slot graph, a hash table whose
values are CLOS instances, an alist of plists — `inspect` walks
all of it. v2 ports this experience to the bridge as an inspector
session: open on a value, navigate by slot index / hash key / array
index, see the new value, optionally mutate.

### Tickets

- **`#120 P1 inspect` `inspect FORM` opens an inspector session.**
  Evaluates `FORM` in the worker, then describes the value as:

  ```jsonc
  {
    "session": "ins-3",
    "value_repr": "#<MY-CLASS ...>",
    "type": "MY-CLASS",
    "parts": [
      {"i": 0, "label": "x", "repr": "1",   "kind": "slot"},
      {"i": 1, "label": "y", "repr": "...", "kind": "slot"},
      ...
    ],
    "actions": ["into", "pop", "mutate"]
  }
  ```

  `parts` is shaped by the type:
  - CLOS / struct: one entry per slot
  - cons cell: car / cdr
  - vector / list / array: indexed elements (paged at 100 per page)
  - hash table: key/value entries (paged)
  - symbol: name, package, value (if bound), function (if fbound), plist

  Acceptance: `inspect '(loop for i from 1 to 5 collect i)'` produces
  a session with parts `[1, 2, 3, 4, 5]`.

- **`#121 P1 inspect` `inspect-into i`.** Push a new layer onto the
  inspector stack focused on `parts[i]`. Server tracks the stack so
  `inspect-pop` works.

  Acceptance: `inspect-into 2` on a list returns the inspection of
  that element; `inspect-pop` restores the previous view.

- **`#122 P1 inspect` `inspect-eval FORM`.** Eval `FORM` in a lexenv
  where `*` is the currently focused value. Lets the LLM compute on
  the inspection target without leaving the session.

- **`#123 P2 inspect` `inspect-mutate i FORM`.** Set part `i` to the
  value of `FORM`. Opt-in per session via `inspect --mutable`. For
  CLOS that means `(setf (slot-value ...))`; for vectors `(setf
  aref)`; for hash tables `(setf gethash)`; for cons cells `setf car`
  / `setf cdr`.

- **`#124 P2 inspect` Pagination and bounded rendering.** Per-part
  `repr` uses `--print-level 4 --print-length 64` by default. Long
  sequences page with `inspect-page n`. Acceptance: a 10 000-element
  list's inspector opens in <100 ms and exposes pagination.

- **`#125 P3 inspect` Inspector close.** Explicit `inspect-close
  session` for cleanup; sessions also clean on connection drop.

---

## Source navigation & compile diagnostics

The compiler is half the value of using a Lisp REPL: every redefinition
gets type-checked, undefined-function-warned, style-noted. v1 swallows
all of it as stderr text. v2 surfaces compiler diagnostics as
structured frames with source locations, and lets the LLM ask the
image where any definition lives.

### Tickets

- **`#130 P1 source` `compile-file PATH` with structured diagnostics.**
  Wraps `asdf:compile-file*` (or `cl:compile-file`) with a
  `handler-bind` that captures every condition signaled during
  compilation. Each diagnostic streams as an event:

  ```jsonc
  {"id": 7, "event": "diagnostic",
   "severity": "style-warning",       // or "warning", "error", "note"
   "type": "SB-INT:SIMPLE-STYLE-WARNING",
   "message": "undefined function FOO",
   "source": {"file": "src/x.lisp", "line": 17, "column": 3,
              "form": "(foo 1)"}}
  ```

  Terminal `result` carries `success: bool` and `output_truename`.

  Acceptance: a file that triggers one undefined-function warning
  yields exactly one `diagnostic` event with the right file and line.

- **`#131 P1 source` `load-file PATH` with structured diagnostics.**
  Same shape, runs `cl:load` with the same handler-bind. Plus reports
  the package the load left active in `result.package`.

- **`#132 P1 source` `find-definition SYMBOL [kind]`.** Returns a list
  of source locations:

  ```jsonc
  [{"kind":"function","file":"src/x.lisp","line":42},
   {"kind":"method","specializers":["MY-CLASS","T"],
    "file":"src/x.lisp","line":58}]
  ```

  Uses `sb-introspect:find-definition-sources-by-name`. `kind`
  filters: `:function`, `:method`, `:macro`, `:class`, `:variable`,
  `:condition`, etc.

  Acceptance: `find-definition 'asdf:load-system` returns at least
  one entry pointing into the ASDF source.

- **`#133 P1 source` `xref CALLERS|CALLEES|REFERENCES SYMBOL`.** Uses
  `sb-introspect:who-calls` / `who-references` / `who-binds` etc.
  Streams entries with source locations.

- **`#134 P2 source` Per-eval source recording.** `eval --source-file
  FILE --source-line N '(defun foo ...)'`. The redefinition log
  records `(:source FILE :line N)` so the existing `diff` machinery
  can show a real diff instead of "differs from source." Cheap; closes
  a long-standing limitation in `clpm repl call list-redefinitions`.

- **`#135 P2 source` `revert SYMBOL`.** Reads the on-disk definition
  (via `find-definition`) and re-evaluates it, undoing an in-image
  edit. Useful after experiments. Errors loudly if the file location
  is unknown or stale.

- **`#136 P2 source` `macroexpand FORM [n]`.** `macroexpand-1` by
  default, full expansion if `n=nil`. Returns the expanded form as a
  prin1'd string. Bonus: `macroexpand-all` via the SBCL walker.

---

## Introspection & completion

The questions an LLM asks of a Lisp image: "what does this function
take?", "what's in this package?", "what methods are defined on this
class?", "is there a function called something like X?". All of these
are one SBCL call away; v2 just plumbs them through.

### Tickets

- **`#140 P1 intro` `apropos PATTERN [package]`.** Returns symbol
  entries with `name`, `package`, `kinds` (an array — same symbol can
  be `:function` *and* `:macro` *and* a `:variable`), and `external`
  bool.

- **`#141 P1 intro` `documentation SYMBOL TYPE`.** Wraps
  `cl:documentation`. `TYPE` is one of `:function`, `:variable`,
  `:type`, `:structure`, `:setf`, `:method-combination`.

- **`#142 P1 intro` `arglist SYMBOL`.** Wraps
  `sb-introspect:function-lambda-list`. Returns the lambda list as a
  prin1'd string *and* a parsed form for tools that need structure.
  Marks `&optional`, `&key`, `&rest`, default expressions.

- **`#143 P1 intro` `complete-symbol PREFIX [package]`.** Returns up
  to N candidates. Useful for the LLM to "remember" a half-recalled
  function name.

- **`#144 P1 intro` `package-info PACKAGE`.** Name, nicknames, use
  list, used-by list, exports (with kind annotations), internal
  symbol count. Per-package-local-nicknames (SBCL extension).

- **`#145 P1 intro` `class-info CLASS`.** Direct supers, direct subs,
  precedence list, direct slots (with type / initform / accessor),
  methods specialized on the class.

- **`#146 P1 intro` `function-info SYMBOL`.** Combined arglist,
  documentation, inline-p, ftype declarations, the
  `sb-introspect:function-type` if known.

- **`#147 P2 intro` `disassemble SYMBOL`.** Captures
  `(disassemble symbol)` output. Bounded by the existing 1 MB cap.

- **`#148 P2 intro` `describe-system NAME`.** ASDF-side: list a
  system's components, dependencies (resolved + declared), load
  status, source location. Bridges ASDF's reflection.

---

## Multiple values, history, printing

Lisp is a multi-value language and v1 silently drops all but the
primary value. The REPL "history" of `*` `**` `***` is how interactive
Lisp programmers think. Both should be first-class.

### Tickets

- **`#150 P0 values` Return all values.** `result.values` is an array
  of prin1'd strings, one per value SBCL returned. `result.value`
  remains the first one for back-compat.

  Acceptance: `eval '(floor 7 2)'` returns `values: ["3","1"]`.

- **`#151 P0 history` Persistent `*`, `**`, `***`, `+`, `++`,
  `+++`, `/`, `//`, `///`.** After each eval, the daemon updates these
  in the worker's package (`*package*` of the worker, which is
  typically `cl-user`). Standard REPL semantics: `*` is the most
  recent primary value, `/` is the most recent values list, `+` is
  the most recent form. Acceptance: `eval '(+ 1 2)'` followed by
  `eval '(* * 10)'` returns `30`.

- **`#152 P0 print` Per-eval print controls.** Flags map to bindings
  for the duration of the eval:
  `--print-length N` → `*print-length*`, `--print-level N` →
  `*print-level*`, `--print-circle` → `*print-circle*`, `--print-radix`,
  `--print-base`, `--print-pretty`. Defaults are sensible (length 200,
  level 8, circle T).

- **`#153 P0 print` Defensive value rendering.** Wrap every
  user-value print in `handler-case`; if `print-object` errors, fall
  back to `format nil "#<unprintable ~A: ~A>" (type-of v) (princ-to-string error)`.
  A buggy method must not blank the response.

- **`#154 P1 print` Pretty-printer settings for human output.** When
  `eval --pretty`, render the value with `*print-pretty* t`,
  `*print-right-margin* 80`, and gentle indentation. The structured
  fields (`output`, `values`) stay unaffected.

- **`#155 P2 print` Truncate-by-depth, not just by length.** Today the
  1 MB cap is a global byte limit. Add depth-aware rendering so an
  ugly value still produces a *useful* shape: at the head, with
  ellipses where it'd be deep.

---

## Tracing, timing, profiling

The fast loop "is this function doing what I think? how often is it
called? where is it slow?" needs three primitives: `trace`,
`time-eval`, `profile-eval`.

### Tickets

- **`#160 P1 trace` `trace SYMBOL [...]`.** Wraps `cl:trace`. Each
  invocation streams events on any in-flight session that asked for
  trace output via `--trace`:

  ```jsonc
  {"id": 7, "event": "trace",
   "kind": "enter", "depth": 2, "name": "FOO", "args": ["1","2"]}
  {"id": 7, "event": "trace",
   "kind": "leave", "depth": 2, "name": "FOO", "values": ["3"]}
  ```

  Daemon installs a custom trace function (via the underlying
  `sb-debug:*trace-encapsulate-default*` mechanism) that pushes events
  onto a per-session channel.

- **`#161 P1 trace` `untrace [SYMBOL ...]`.** Mirror.

- **`#162 P1 trace` `tracedp` / `list-traced`.** Discoverability.

- **`#163 P1 time` `time-eval FORM`.** Returns `result.timing` with
  `real_ms`, `cpu_ms`, `gc_real_ms`, `cons_bytes`, `eval_count`,
  `processor_cycles` (when available). Same shape `(time ...)` shows.

- **`#164 P2 profile` `profile-eval FORM [--top N] [--mode :cpu|:alloc]`.**
  Wraps `sb-sprof:with-profiling`. Returns the top-N entries with name
  + source location + self-time fraction.

- **`#165 P3 trace` Conditional trace and break.** `trace FOO :when
  '(> arg 100)' :break-on-result '(eq result :error)'`.

---

## Workers and concurrent contexts

A single worker is fine for the bare loop, but mixing experiments
(loading a new library; testing a hot function; running a slow test
suite) all on one worker means they collide. v2 lets the LLM spin up
named workers with independent `*package*` / history / handlers.

### Tickets

- **`#170 P2 workers` Named workers.** `eval --worker foo` runs in
  worker named `foo`. If it doesn't exist, it's spawned. Each worker
  has its own package state, history bindings, redefinition log
  *bucket*. The default (no `--worker`) keeps using the v1 singleton.

- **`#171 P2 workers` `list-workers`.** Returns each worker's name,
  state (idle/busy/in-debugger), current package, last eval id, age.

- **`#172 P2 workers` `kill-worker NAME`.** Like `reset` but scoped.
  Idempotent.

- **`#173 P3 workers` `eval --concurrent`.** Single-use disposable
  worker that's destroyed after the eval, with isolation from shared
  state. Useful for "is this form safe to run?" experiments without
  contaminating the main worker.

---

## File watching and hot reload

Editing in-image and editing on disk diverge constantly. v2 adds a
watcher: file save → re-eval the changed top-level forms, surface
diagnostics. Optional, opt-in.

### Tickets

- **`#180 P3 watch` `watch DIR [--glob '*.lisp']`.** Daemon polls
  `directory + glob` (1s interval; native FSEvents/inotify is out of
  scope), and on a mtime change, re-evaluates the file via
  `compile-file` semantics. Streams `diagnostic` and a terminal
  `reloaded` event.

- **`#181 P3 watch` `unwatch ID` / `list-watches`.** Mirror.

- **`#182 P3 watch` Auto-revert support.** When a watched file changes,
  any in-image definition recorded as coming from that file is
  automatically reverted to the on-disk version, with a notification.
  Makes "edit on disk, see effect immediately" Just Work.

---

## Image and ASDF management

Things an LLM wants to know about the running daemon: what's loaded,
what's bound, where am I, can I dump state.

### Tickets

- **`#190 P2 image` `image-info`.** Pid, Lisp impl + version,
  uptime, `*features*`, default pathname, working directory, total
  bytes consed, GC count, loaded image-saved-from path if any.

- **`#191 P2 image` `loaded-systems`.** ASDF's loaded systems with
  versions and load timestamps.

- **`#192 P2 image` `list-packages`.** Names + nicknames + sizes
  (internal + external symbol counts). Lets the LLM map "the
  vendored library defined a package; what is it called?".

- **`#193 P3 image` `save-image PATH`.** Wraps
  `sb-ext:save-lisp-and-die` *but* spawn it in a child so the running
  daemon survives. Or: explicit "this will exit the daemon" warning,
  return last event before death.

- **`#194 P3 image` `gc [--full]`.** Trigger GC, return before/after
  bytes-consed.

---

## Discoverability, ergonomics, and docs

The bridge should be usable from cold context: an LLM that has never
seen this skill should be able to discover its capabilities.

### Tickets

- **`#200 P1 docs` `methods` RPC.** Generated from a single source
  table of `(method-name params-schema docstring)`. Returns one
  entry per method, with parameter names, types, defaults, and a
  one-line summary.

- **`#201 P1 docs` `help METHOD` RPC.** Same source, returns the
  long-form docstring + a tiny worked example for each method.

- **`#202 P1 docs` `clpm repl call methods` CLI.** Prints the
  `methods` table as a human-readable list. Useful from a shell.

- **`#203 P1 docs` Skill update.** Rewrite
  `.claude/skills/clpm-repl.md` to cover v2: the debugger
  workflow, the inspector workflow, compile diagnostics, source
  navigation. Keep the doc under 250 lines; structure as recipes the
  LLM can copy-paste.

- **`#204 P2 docs` README v2 section.** Replace the
  "AI-assisted development" subsection with a fuller "operator
  manual" pointing at the skill and listing the new capabilities.

- **`#205 P2 ergo` `--explain` global flag.** Before executing, the
  daemon emits a `plan` event describing what it's about to do. Helps
  debug protocol misuse.

- **`#206 P3 ergo` Result rendering helpers.** `--pretty --tree`
  renders nested values as an indented tree instead of one s-exp.
  Heavy lifting; isolate behind one flag so it doesn't bloat the
  default path.

---

## Hygiene, safety, observability

Per principle #6 ("defensive everywhere"), v2 includes a number of
small safety nets so the daemon stays useful even when an eval is
abusive.

### Tickets

- **`#210 P1 safety` Heartbeat / liveness.** A long-running eval
  emits `event: heartbeat` every 30 s with `{frame_count, gc_count,
  bytes_consed}`. Lets the LLM know "the daemon is alive, just slow."
  Cheap; only on `eval --stream`.

- **`#211 P1 safety` Per-eval `*break-on-signals*` `:none`
  override.** Even if the daemon's global default is to break on
  warnings, individual evals can opt out with `--break-on nil`.

- **`#212 P2 safety` Crash recovery.** If the worker thread dies
  unexpectedly (genuine SBCL crash, not a user signal), the daemon
  logs `event: worker-died`, the supervisor respawns a fresh worker,
  and the next eval gets `code: worker-restarted` *plus* a normal
  result.

- **`#213 P2 obs` Per-method counters in `ping`.** Today's `ping`
  returns total `eval_count`. Extend to a histogram: `{method:
  count, ...}` and a recent-error count.

- **`#214 P2 obs` Slowlog.** Evals taking > 1 s automatically log
  `event: slow-eval` to `.clpm/repl.log` with the elapsed time
  and the first 200 chars of the form. Helps a future operator (the
  LLM or the human) find pathological evals retroactively.

- **`#215 P3 safety` Per-session resource caps.** Optional
  `--max-cons-bytes`, `--max-real-ms` on `eval` that abort the eval
  with `code: resource-exhausted` when crossed.

---

## Implementation order

This is too much to land in one push. Suggested order:

1. **Protocol foundation** (#101–#106). Without this, nothing else
   composes. Land alongside a v1 → v2 compatibility test that proves
   old behavior is untouched.
2. **Multiple values + history + defensive printing** (#150–#153). The
   smallest set that materially improves *every* eval response.
3. **Debugger** (#110–#117). The headline feature. Build atop the
   protocol foundation.
4. **Compile / source / introspection** (#130–#136, #140–#146).
   Independent and high-value; can land in parallel with the
   debugger work.
5. **Inspector** (#120–#125). Smaller than the debugger but related.
6. **Trace / time / profile** (#160–#164).
7. **Workers, image, watch** (#170–#193). Lower priority; bigger
   architectural surface.
8. **Docs and discoverability** (#200–#204) — refresh continuously,
   *not* at the end. Every method ticket includes a docs delta.

### Estimated effort

| Section | Tickets | Rough total |
|---|---|---|
| Protocol foundation | 6 | 3–4 days |
| Debugger | 9 | 4–5 days |
| Inspector | 6 | 2–3 days |
| Source / compile | 7 | 3–4 days |
| Introspection | 9 | 2 days (mostly thin wrappers) |
| Values / history / printing | 6 | 1–2 days |
| Tracing / timing / profiling | 6 | 2–3 days |
| Workers | 4 | 2 days |
| Watch / hot reload | 3 | 1–2 days |
| Image / ASDF | 5 | 1–2 days |
| Docs / ergo | 7 | 2 days |
| Hygiene / safety | 6 | 2 days |
| **Total** | **74** | **~5 weeks** |

These are eyeball estimates from someone who's just written v1; treat
as ranges. The protocol foundation has the most risk; once it's in,
the rest is mostly mechanical surface area over `sb-introspect`,
`sb-debug`, `sb-profile`, `sb-sprof`, and `asdf`.

---

## Out of scope (deliberately)

These showed up while writing the doc and were *not* added:

- **A built-in editor.** Editing happens in the user's IDE; the
  bridge integrates via `watch` + `find-definition`.
- **A SLIME-compatible protocol.** SWANK is a different shape and
  carrying its protocol semantics is a lot of complexity for no
  benefit when the *user* is an LLM, not Emacs.
- **Sandboxing.** Per principle #2 in the original BRIDGE.md, the
  bridge is "logged into the workstation." If you need a sandbox,
  use Docker.
- **Persistent state across daemon restarts.** History bindings, the
  redefinition log, debug sessions — all die with the daemon. A
  saved image (#193) is the escape hatch.
- **A second transport (HTTP, websockets, gRPC).** Adds operational
  surface for no protocol expressivity gain.
- **Multi-project daemons.** One daemon per project, full stop.
  Sharing breaks the "loaded systems are pinned to this project's
  lockfile" guarantee.
- **Authentication beyond the existing socket-mode-0600 / TCP-token.**
  If the threat model is "another user on the box," sockets stop
  working too. The bridge is single-user.

---

## What "excellent" looks like, concretely

A walkthrough an LLM should be able to drive end-to-end after v2 ships,
with no other tools:

```sh
# 1. Start a daemon for this project, with its lockfile-resolved
#    systems loaded.
clpm repl daemon --detach

# 2. Find where a function is defined; read its source.
clpm repl call find-definition --symbol my-app:slow-fn
# → src/my-app/core.lisp:142

# 3. Trace it, then run a workload.
clpm repl call trace --symbol my-app:slow-fn
clpm repl eval '(my-app:run-suite)'
# (stream of `trace` events; final timing in result.timing)

# 4. A test errors; enter the debugger.
clpm repl eval --debug '(my-app:run-test-7)'
# → event: debugger-entered { condition: ... frames: [...] }

# 5. Inspect the bound variable in frame 3.
clpm repl call debug-eval-in-frame --frame 3 --form 'STATE'

# 6. Patch it, then continue.
clpm repl call debug-eval-in-frame --frame 3 --form '(setf state :ok)'
clpm repl call debug-continue
# → result: success

# 7. Fix the function and re-evaluate from source.
clpm repl call load-file --path src/my-app/core.lisp
# (any compile diagnostics stream; final result.success: true)

# 8. Did the redefinition land where the file says?
clpm repl call list-redefinitions
# → list-redefinitions, all up-to-date with source

# 9. Profile a hot path.
clpm repl call profile-eval --top 10 --form '(my-app:bench-1)'

# 10. Done.
clpm repl daemon --stop
```

If at any step the LLM hits a wall — an error it can't introspect, a
restart it can't pick, a value it can't render — v2 has failed its
goal. That's the bar.

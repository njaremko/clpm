---
name: clpm-repl-bridge
description: Drive a persistent project-scoped Lisp daemon over a Unix-socket JSON-RPC for fast LLM-assisted Common Lisp development.
---

# clpm-repl-bridge (v2)

The bridge is a daemonized SBCL image, scoped to a CLPM project, that answers
JSON-RPC requests over `.clpm/repl-bridge.sock`. State (loaded systems, defined
functions, current package) persists between calls, so redefining one function
is dramatically faster than reloading systems from scratch.

This skill exists because, without it, an LLM editing Lisp code is condemned to
either spawn a fresh SBCL per check (slow, cold cache) or paste forms into an
interactive REPL (fragile, hard to script). The bridge gives the LLM a stable,
scriptable handle on a long-lived image — *plus* live restarts, an inspector,
file watching, compile diagnostics, source navigation, and a method table that
makes the protocol self-documenting.

## Operating rules

- **One-shot evaluation:** Use `clpm repl-bridge eval` for individual forms.
  State persists — `defparameter`s stick, `defun`s stay redefined, the current
  package carries over.

- **Prefer in-image redefinition.** When iterating on a function, edit the
  source file, then re-send the single `defun` (or `(load "path/to/file.lisp")`).
  Don't re-run `asdf:load-system` for tiny changes — milliseconds vs. seconds.

- **A hung form is not fatal.** Run `clpm repl-bridge interrupt` from another
  shell. It unwinds the worker's current eval; the daemon stays up.

- **Wedge-recovery is one call.** If the worker is corrupted, send `reset`. It
  terminates the worker thread, spawns a fresh one, and clears the redefinition
  log. The daemon, loaded systems, and persistent current-package survive.

- **Don't restart the daemon casually.** Loaded systems live there. Restart
  only on dependency-graph changes (then: `clpm install` + fresh `serve`).

- **Method table is the source of truth.** `clpm repl-bridge methods` lists
  every RPC with one-line summaries; `clpm repl-bridge methods NAME` returns
  the full doc and parameter schema. If a recipe below looks stale, ask the
  daemon.

## Cheat sheet

The CLI is the LLM-friendly entry point — every interactive feature
(debugger, inspector, watcher, ...) has a single-shot wrapper so you
never need to script raw JSON-RPC over a socket. Each subcommand
auto-starts the daemon on first contact.

```sh
# lifecycle
clpm repl-bridge serve --detach        # explicit daemon launch
clpm repl-bridge status                # is it up?
clpm repl-bridge stop                  # clean shutdown
clpm repl-bridge ping                  # liveness + per-method counters

# evaluation (auto-starts daemon)
clpm repl-bridge eval '(+ 1 2)'
clpm repl-bridge eval '(read-from-string "FOO")' --package my-app
clpm repl-bridge time-eval '(some-fn)'             # wall + cpu + cons
clpm repl-bridge profile-eval '(big-fn)' --top 10  # sb-sprof flat report
clpm repl-bridge gc [--full]                       # trigger a GC
clpm repl-bridge interrupt                         # unwind current eval

# introspection
clpm repl-bridge apropos PATTERN [--package P]
clpm repl-bridge arglist SYMBOL
clpm repl-bridge doc SYMBOL [--type function|variable|type]
clpm repl-bridge describe SYMBOL
clpm repl-bridge find-definition SYMBOL [--kind function|class|...]
clpm repl-bridge xref SYMBOL --direction calls|references|sets|binds
clpm repl-bridge who-calls SYMBOL                  # alias for --direction calls
clpm repl-bridge complete-symbol PREFIX [--limit N]
clpm repl-bridge disassemble SYMBOL
clpm repl-bridge function-info SYMBOL
clpm repl-bridge class-info CLASS-NAME
clpm repl-bridge package-info PKG-NAME
clpm repl-bridge describe-system SYS-NAME
clpm repl-bridge macroexpand FORM [--full]
clpm repl-bridge list-packages
clpm repl-bridge loaded-systems
clpm repl-bridge image-info                        # pid, lisp, mem, ...
clpm repl-bridge current-package [--worker W]
clpm repl-bridge set-package NAME [--worker W]

# load / compile / watch
clpm repl-bridge compile-file PATH                 # streams diagnostics
clpm repl-bridge load-file PATH
clpm repl-bridge watch DIR [--glob G] [--auto-revert]   # streams events
clpm repl-bridge list-watches
clpm repl-bridge unwatch ID

# workers and tracing
clpm repl-bridge workers                           # alias: list-workers
clpm repl-bridge kill-worker NAME
clpm repl-bridge reset [--worker W]
clpm repl-bridge trace SYMBOL...
clpm repl-bridge untrace [SYMBOL...]
clpm repl-bridge list-traced

# single-shot interactive features (no held session)
clpm repl-bridge inspect FORM [--path 0,1,2]       # walks the path, closes
clpm repl-bridge debug FORM                        # prints frames+restarts, exits rc=3
clpm repl-bridge debug FORM --restart NAME [--arg V...]
clpm repl-bridge debug FORM --frame N --frame-eval '(local-form)'

# discovery
clpm repl-bridge methods                           # one-line summary per RPC
clpm repl-bridge methods eval                      # full doc + params
clpm repl-bridge diff                              # top-level redefinitions seen
clpm repl-bridge help                              # this command listing

# global flag: append `--json` for raw JSON-on-a-line responses
clpm repl-bridge image-info --json
```

## Response shape

Every response is one line of JSON, terminated by `\n`.

```jsonc
// success
{"id":1,"result":{
  "value":"3",                 // prin1 of primary value
  "values":["3"],              // every value (multiple-value-list)
  "output":"", "error_output":"",
  "package":"COMMON-LISP-USER",
  "elapsed_ms":2,
  "conditions":[],
  "history":{"*":"3","**":"…"},
  "truncated":false
}}

// error
{"id":1,"error":{
  "code":"eval-error",         // or reader-error, interrupted, protocol-error,
                               //    output-truncated, worker-restarted
  "message":"…",
  "details":{
    "output":"…","error_output":"…",
    "conditions":[{
      "type":"SIMPLE-ERROR", "message":"…",
      "restarts":[{"name":"CONTINUE","report":"…","arity":0},...],
      "backtrace":[{"function":"…","args":[…],"vars":[…],"source":{…}}, ...],
      "slot_values":{…}
    }]
  }
}}
```

Read `result.value` for the return value, `result.output` for stdout. On
error, the actionable text is `error.details.conditions[0].message`; the
backtrace is debugger-grade (live restarts, frame locals, source positions).

## Recipe: interactive debugger

The CLI hides the continuation dance behind `clpm repl-bridge debug`:

```sh
# default: print frames+restarts, abort cleanly, exit rc=3
clpm repl-bridge debug '(error "boom")'

# pick a restart immediately
clpm repl-bridge debug '(restart-case (/ 1 0) (use-value (v) v))' \
    --restart USE-VALUE --arg 0
# → "=> 0", rc=0

# evaluate a form in a stack frame, then abort
clpm repl-bridge debug '(let ((x 7)) (error "x=~A" x))' \
    --frame 0 --frame-eval '(* x 2)'

# CONTINUE on a cerror
clpm repl-bridge debug '(cerror "skip" "oops")' --restart CONTINUE
```

`--break-on K` binds `*break-on-signals*` for the eval. `--package P`
chooses the reader package. The session is always closed when the
command exits — no held debugger state between invocations.

Raw JSON-RPC form (only needed when scripting outside the CLI):

```jsonc
// 1) issue the eval; client must stream events on a held connection
{"id":7,"method":"eval","params":{"form":"(/ 1 0)","debug":true}}

// 2) daemon emits:
{"id":7,"event":"debugger-entered","condition":{…,"restarts":[…]}}

// 3) inspect a frame's locals, then invoke a restart
{"id":7,"method":"debug-eval-in-frame","params":{"index":0,"form":"(list :a :b)"}}
{"id":7,"method":"debug-invoke-restart","params":{"name":"ABORT"}}
```

`break-on: "warning"` makes the daemon enter the debugger on signaled warnings;
`handlers: [{type:"…",restart:"…"}]` is the non-interactive variant that just
invokes a named restart on a matching condition.

## Recipe: inspector

CLI form (single-shot, session opened/closed automatically):

```sh
clpm repl-bridge inspect '(list :a :b :c)'             # show parts at depth 1
clpm repl-bridge inspect '(list 100 200 300)' --path 1 # walk parts[1], depth 2
clpm repl-bridge inspect '(make-hash-table)' --keep    # leave session open
```

Raw JSON-RPC for held sessions (chained traversal, mutation, eval-in-focus):

```jsonc
{"id":10,"method":"inspect","params":{"form":"(make-hash-table)"}}
// → {result:{session:1,parts:[…],total:0,…}}
{"id":11,"method":"inspect-eval","params":{"session":1,"form":"(hash-table-count *)"}}
```

Continuation methods: `inspect-into`, `inspect-pop`, `inspect-eval`
(binds `*` to the current focus), `inspect-mutate`, `inspect-page`
(100 per page), and `inspect-close`.

## Recipe: source navigation and compile diagnostics

```jsonc
// find every definition of FOO
{"id":1,"method":"find-definitions","params":{"symbol":"foo"}}

// who calls bar?
{"id":2,"method":"who-calls","params":{"symbol":"bar"}}

// compile a file and surface warnings/errors with source positions
{"id":3,"method":"compile-file","params":{"path":"src/my-app.lisp"}}
// → {result:{diagnostics:[{severity:"warning",file:"…",line:42,message:"…"}]}}
```

## Recipe: introspection

`apropos PATTERN`, `documentation`, `arglist`, `complete-symbol PREFIX`,
`package-info`, `class-info`, `function-info`, `disassemble`,
`describe-system` (ASDF).

## Recipe: named / concurrent workers

Default behaviour is a single worker named `"default"` — same package, same
history, same redefinition log everyone has used since v1. For isolated
experiments, pass `worker: NAME` (named workers have independent state) or
`concurrent: true` (a fresh worker that's destroyed after the eval).

```jsonc
{"id":1,"method":"eval","params":{"form":"(in-package :keyword)","worker":"scratch"}}
{"id":2,"method":"list-workers"}        // see every worker
{"id":3,"method":"kill-worker","params":{"name":"scratch"}}
```

`reset` / `interrupt` / `current-package` / `set-package` / `list-redefinitions`
all take an optional `worker` to scope to a non-default slot.

## Recipe: file watching and hot reload

```jsonc
{"id":1,"method":"watch","params":{"dir":"src","glob":"*.lisp","auto_revert":true}}
// → event:watch-acknowledged {id:7}
// later, when src/foo.lisp changes on disk:
// → event:file-reloaded {file:"…/foo.lisp",diagnostics:[…]}
// → event:revert-applied {file:"…/foo.lisp"}        (auto_revert mode)

{"id":2,"method":"unwatch","params":{"id":7}}
```

`list-watches` enumerates active watchers.

## Recipe: time / trace / profile

```jsonc
{"id":1,"method":"time-eval","params":{"form":"(some-fn)"}}
// → {result:{values:[…],timing:{real_ms:12,bytes_consed:8192,…}}}

{"id":2,"method":"trace","params":{"symbols":["my-fn"]}}
{"id":3,"method":"untrace","params":{"symbols":["my-fn"]}}
{"id":4,"method":"profile-eval","params":{"form":"(big-fn)","top":10}}
```

## Recipe: image management

`image-info`, `loaded-systems`, `list-packages`, and `gc [--full]` answer the
"what's loaded?" / "what packages exist?" / "how much memory?" questions
without `eval`-ing your way around.

## Recipe: explain a request before running it

Pass `explain: true` in any params object and the daemon emits a `plan` event
echoing what it parsed before invoking the handler. Use this when a protocol
call isn't doing what you expected — the plan tells you whether the parsing or
the handler is the problem.

## Caps and conventions

- **Request size:** 64 KB max per line of JSON. Forms above that must be
  loaded from a file.
- **Output size:** 1 MB combined stdout+stderr per `eval`. Excess is dropped;
  the response carries `truncated: true`.
- **Backtrace:** capped at 16 frames; bridge frames are stripped.
- **Concurrency:** evals on the same worker are serialized through a mailbox;
  named workers are independent.
- **Continuations:** `query-response` (read-from-stdin replies) and `debug-*`
  ride on the *same id* as the eval that triggered them.

## When NOT to use the bridge

- **Adding/removing a dependency** — restart the daemon after `clpm install`.
- **Changing the lockfile or registries** — `*central-registry*` was wired up
  at `serve` time.
- **Sandboxed evaluation** — this is a normal SBCL image. The socket is mode
  `0600` and project-local, but treat the bridge as "logged into the host".

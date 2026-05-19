---
name: clpm-repl-bridge
description: Drive a persistent project-scoped Lisp daemon over a Unix-socket JSON-RPC for fast LLM-assisted Common Lisp development.
---

# clpm-repl-bridge

The bridge is a daemonized SBCL image, scoped to a CLPM project, that answers
JSON-RPC requests over `.clpm/repl-bridge.sock`. State (loaded systems, defined
functions, current package) persists between calls, so redefining one function
is dramatically faster than reloading systems from scratch.

This skill exists because, without it, an LLM editing Lisp code is condemned to
either spawn a fresh SBCL per check (slow, cold cache) or paste forms into an
interactive REPL (fragile, hard to script). The bridge gives the LLM a stable,
scriptable handle on a long-lived image.

## Operating rules

- **One-shot evaluation:** Always use `clpm repl-bridge eval` for individual
  forms. The daemon persists state across calls — `defparameter`s stick,
  `defun`s stay redefined, the current package carries over.

- **Prefer in-image redefinition.** When iterating on a function, edit the
  source file, then `clpm repl-bridge eval '(load "path/to/file.lisp")'` or
  re-send the single `defun`. Do not run `asdf:load-system` again for tiny
  changes — that's seconds vs. milliseconds.

- **Surface drift before declaring work done.** Run
  `clpm repl-bridge diff` after a session of in-image edits. It lists every
  `defun`/`defmethod`/`defclass`/... you mutated in-image and compares it
  against what's on disk. Anything marked `differs` is code that exists only
  in the running image — save it to a file or it's gone on next restart.

- **A hung form is not fatal.** If `eval` doesn't return within your patience
  window, run `clpm repl-bridge interrupt` from another shell (or in another
  tool call). It unwinds the worker's current eval; the daemon stays up.

- **Wedge-recovery is one call.** If you've corrupted the readtable, redefined
  `cl:car`, or otherwise broken the worker, send `reset` via `eval` — it
  terminates the worker thread, spawns a fresh one, and clears the
  redefinition log. The daemon survives; loaded systems survive; the
  persistent current-package survives.

- **Don't restart the daemon casually.** It holds your loaded systems. Stop
  it only when you're finished, or when you've changed something the image
  can't pick up (e.g. an `:depends-on` in `clpm.project` — that needs
  `clpm install` then a fresh `serve`).

## Response shape

Every response is **one line of JSON**, terminated by `\n`. Parsing rules:

```jsonc
// success
{"id":1,"result":{
  "value":"3",                 // prin1 of the primary value, as a string
  "output":"",                 // stdout captured during eval
  "error_output":"",           // stderr captured during eval
  "package":"COMMON-LISP-USER",
  "elapsed_ms":2,
  "conditions":[],             // signaled but unhandled conditions
  "truncated":false            // true if output hit the 1 MB cap
}}

// reader or eval error
{"id":1,"error":{
  "code":"reader-error",       // or "eval-error", "interrupted",
                               //    "output-truncated", "protocol-error"
  "message":"end of file on #<...>",
  "details":{                  // only on eval-error / reader-error
    "output":"...",            // anything printed before the error
    "error_output":"...",
    "conditions":[{            // first frame has the actual condition
      "type":"SIMPLE-ERROR",
      "message":"...",
      "restarts":["ABORT"],
      "backtrace":["...","..."]   // up to 16 frames, bridge frames stripped
    }]
  }
}}
```

Read `result.value` for the return value, `result.output` for stdout. On
error, the actionable text is `error.details.conditions[0].message`; the
backtrace is for debugging, not LLM consumption.

## Cheat sheet

```sh
# start the daemon for the current project (loads .clpm/asdf-config.lisp)
clpm repl-bridge serve --detach

# evaluate one form (auto-starts the daemon if none is running)
clpm repl-bridge eval '(+ 1 2)'

# evaluate inside a specific package, just for this call
clpm repl-bridge eval '(symbol-package (read-from-string "FOO"))' --package my-app

# describe a symbol (its docstring, type, etc.)
clpm repl-bridge describe car

# show what's been redefined in-image since serve / last reset
clpm repl-bridge diff

# break out of a hung eval; daemon stays up
clpm repl-bridge interrupt

# is the daemon up?
clpm repl-bridge status

# clean shutdown
clpm repl-bridge stop
```

## Workflow: tight edit/test loop

```sh
# session starts
clpm repl-bridge serve --detach
clpm repl-bridge eval '(asdf:load-system "my-app")'

# iterate on one function
$EDITOR src/my-app.lisp
clpm repl-bridge eval '(load "src/my-app.lisp")'      # or just resend the defun
clpm repl-bridge eval '(my-app:run-test-1)'

# before handing off
clpm repl-bridge diff       # any 'differs'? save them
clpm repl-bridge stop
```

## Caps and timeouts

- **Request size:** 64 KB max for a single line of JSON. Forms above that
  must be loaded from a file (`(load "tmp.lisp")`).
- **Output size:** 1 MB combined stdout+stderr per `eval`. Excess is
  silently dropped; the response carries `truncated: true` and
  `code: "output-truncated"`.
- **Backtrace:** first 16 frames, with `CLPM.REPL-BRIDGE::` and
  `SB-IMPL::` frames stripped.
- **One eval at a time.** Requests are serialized through a single worker
  thread; other methods (`ping`, `interrupt`, `status`) run concurrently
  with whatever the worker is doing.

## When NOT to use the bridge

- **Adding/removing a dependency.** That edits `clpm.project`; the daemon's
  loaded systems won't reflect it. Use `clpm add`/`clpm remove`, then
  `clpm install`, then restart the daemon.
- **Changing CLPM-resolved registries or lockfile.** Same reason —
  the daemon's `*central-registry*` was wired up at `serve` time.
- **Sandboxed evaluation.** This is a normal SBCL image. Anything an
  attacker could do at the REPL, they can do via the bridge. The socket is
  mode `0600` and project-local, but treat the bridge as "logged into the
  workstation."

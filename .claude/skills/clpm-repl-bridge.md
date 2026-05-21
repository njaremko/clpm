---
name: clpm-repl-bridge
description: Prefer and operate CLPM's persistent project-scoped Lisp daemon for fast Common Lisp debugging, inspection, and cleanup.
---

# clpm-repl-bridge

Use `clpm repl` whenever you need to understand or change a live Common
Lisp system. On a terminal it starts a foreground project Lisp for a human; in
non-interactive tool use it ensures a persistent project-scoped SBCL daemon
with loaded systems, package state, workers, debugger sessions, inspector
sessions, watches, traces, and a self-describing RPC registry.
Use `--interactive` or `--non-interactive` when stdin/stdout detection is not
the behavior you want.

Prefer it over fresh `sbcl` or one-off scripts for bug investigation, local
redefinition, source navigation, frame inspection, timing, tracing, and
recovery through restarts. Fresh processes are still right for clean
end-to-end tests, dependency graph changes, packaging, and CI gates.

## Public CLI

The CLI has one default plus three explicit semantic commands:

```sh
clpm repl [--interactive|--non-interactive]
clpm repl daemon [--detach] [--no-load] [--status] [--stop]
clpm repl eval FORM [--package P] [--worker W] [--debug] ...
clpm repl call METHOD [--params-json JSON] [--PARAM VALUE]...
```

- bare `clpm repl` starts a foreground project Lisp when stdin/stdout are
  terminals; otherwise it ensures a detached daemon and returns.
- `--interactive` forces the foreground project Lisp; `--non-interactive`
  forces the detached daemon ensure.
- `daemon` owns lifecycle. Use `--detach` to start in the background,
  `--status` to check and clean stale pid/socket files, and `--stop` for
  normal shutdown.
- `eval` is the ergonomic one-form path. It auto-starts the daemon unless
  `--no-autostart` is supplied, prints human output by default, and owns the
  interactive debugger continuation loop.
- `call` is the generic RPC constructor. It sends exactly one daemon method.
  Values after `--PARAM` are parsed as JSON when possible, otherwise sent as
  strings. Use `--params-json` for arrays, objects, explicit `null`, or tricky
  quoting.

Discover the real daemon schema instead of trusting memory:

```sh
clpm repl call methods
clpm help repl eval
clpm repl call help --method gc
```

## First Moves

```sh
clpm repl
clpm repl daemon --status
clpm repl eval '(+ 1 2)'
clpm repl call ping
```

After changing `clpm.project`, `clpm.lock`, registry configuration, or
dependency sources, run `clpm deps sync` and restart the daemon so ASDF registry
state and loaded systems match the new dependency graph.

## Debug-First Workflow

Use `eval --debug` before trying to "reason around" a Common Lisp condition.
First observe the stop, frame numbers, restarts, and source locations:

```sh
clpm repl eval '(error "boom")' --debug
```

Then rerun with a selected restart or frame eval:

```sh
clpm repl eval '(restart-case (/ 1 0) (use-value (v) v))' \
  --debug --restart USE-VALUE --arg 0

clpm repl eval '(let ((x 7)) (error "x=~A" x))' \
  --debug --frame 0 --frame-eval '(* x 2)'
```

Use declarative recovery when you already know the condition and restart:

```sh
clpm repl eval '(restart-case (/ 1 0) (use-value (v) v))' \
  --handler division-by-zero=use-value:999
```

Keep a debugger stop only when you need multiple follow-up calls against the
same live stack:

```sh
clpm repl eval '(restart-case (error "need value") (use-value (v) v))' \
  --debug --keep
clpm repl call list-debug-sessions
clpm repl call debug-eval-in-frame --session 1 --frame 4 --form 'x'
clpm repl call debug-invoke-restart --session 1 --name USE-VALUE \
  --args '["42"]'
clpm repl call debug-abort --session 1
```

If more than one debug session is active, always pass `--session N`.

## Everyday Calls

```sh
# image and package state
clpm repl call current-package
clpm repl call set-package --name CL-USER
clpm repl call image-info
clpm repl call loaded-systems
clpm repl call list-packages
clpm repl call gc --full true

# workers and recovery
clpm repl call list-workers
clpm repl call interrupt --worker default
clpm repl call reset --worker default
clpm repl call kill-worker --name scratch

# source and introspection
clpm repl call compile-file --path src/foo.lisp
clpm repl call load-file --path src/foo.lisp
clpm repl call find-definition --symbol my-function
clpm repl call xref --symbol my-function --direction callers
clpm repl call macroexpand --form '(my-macro x)' --recursive true
clpm repl call documentation --symbol my-function --type function
clpm repl call arglist --symbol my-function

# inspector sessions
clpm repl call inspect --form '(list :a :b :c)'
clpm repl call inspect-into --session ins-1 --i 0
clpm repl call inspect-eval --session ins-1 --form '(length *)'
clpm repl call inspect-close --session ins-1

# watch, trace, profile
clpm repl call watch --dir /absolute/path/to/src --glob '*.lisp' --auto-revert true
clpm repl call list-watches
clpm repl call unwatch --id 1
clpm repl call trace --symbols '["my-fn"]'
clpm repl call untrace --symbols '["my-fn"]'
clpm repl call list-traced
clpm repl call time-eval --form '(some-fn)'
clpm repl call profile-eval --form '(big-fn)' --top 10
```

`call` emits raw JSON responses and streams raw event frames, which is the right
shape for agents and other tools.

## Cleanup Checklist

Before handing off, make the daemon state boring:

```sh
clpm repl call list-debug-sessions
clpm repl call debug-abort --session 1
clpm repl call list-watches
clpm repl call unwatch --id 1
clpm repl call list-traced
clpm repl call untrace
clpm repl call list-workers
clpm repl call kill-worker --name scratch
clpm repl call list-redefinitions
clpm repl daemon --status
```

Non-empty `list-redefinitions` means the image contains definitions that may
still need to be written to source. Use `daemon --stop` for normal shutdown and
let `daemon --status` or `daemon --stop` clean stale files; do not delete
`.clpm/repl.sock`, `.clpm/repl.pid`, or `.clpm/repl.log` by hand unless the CLI
cannot recover and the user agrees.

## Limits

- Request lines are capped at 64 KB; load larger forms from files.
- Eval output is capped at 1 MB combined stdout/stderr.
- Backtraces are capped and daemon frames are elided in `eval` human output.
- The daemon has the authority of the host process. Do not treat it as a
  sandbox.

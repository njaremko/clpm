---
name: clpm-repl-bridge
description: Prefer and operate CLPM's persistent project-scoped Lisp daemon for fast Common Lisp debugging, inspection, and cleanup.
---

# clpm-repl-bridge

Use `clpm repl-bridge` whenever you need to understand or change a live Common
Lisp system. It gives agents a persistent project-scoped SBCL image with loaded
systems, package state, workers, debugger sessions, inspector sessions, watches,
traces, and a self-describing RPC registry.

Prefer it over fresh `sbcl`, `clpm repl`, or one-off scripts for bug
investigation, local redefinition, source navigation, frame inspection, timing,
tracing, and recovery through restarts. Fresh processes are still right for
clean end-to-end tests, dependency graph changes, packaging, and CI gates.

## Public CLI

The CLI has three semantic commands:

```sh
clpm repl-bridge daemon [--detach] [--no-load] [--status] [--stop]
clpm repl-bridge eval FORM [--package P] [--worker W] [--debug] ...
clpm repl-bridge call METHOD [--params-json JSON] [--PARAM VALUE]...
```

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
clpm repl-bridge call methods
clpm repl-bridge call help --method eval
clpm repl-bridge call help --method debug-eval-in-frame
```

## First Moves

```sh
clpm repl-bridge daemon --detach
clpm repl-bridge daemon --status
clpm repl-bridge eval '(+ 1 2)'
clpm repl-bridge call ping
```

After changing `clpm.project`, `clpm.lock`, registry configuration, or
dependency sources, run `clpm install` and restart the daemon so ASDF registry
state and loaded systems match the new dependency graph.

## Debug-First Workflow

Use `eval --debug` before trying to "reason around" a Common Lisp condition.
First observe the stop, frame numbers, restarts, and source locations:

```sh
clpm repl-bridge eval '(error "boom")' --debug
```

Then rerun with a selected restart or frame eval:

```sh
clpm repl-bridge eval '(restart-case (/ 1 0) (use-value (v) v))' \
  --debug --restart USE-VALUE --arg 0

clpm repl-bridge eval '(let ((x 7)) (error "x=~A" x))' \
  --debug --frame 0 --frame-eval '(* x 2)'
```

Use declarative recovery when you already know the condition and restart:

```sh
clpm repl-bridge eval '(restart-case (/ 1 0) (use-value (v) v))' \
  --handler division-by-zero=use-value:999
```

Keep a debugger stop only when you need multiple follow-up calls against the
same live stack:

```sh
clpm repl-bridge eval '(restart-case (error "need value") (use-value (v) v))' \
  --debug --keep
clpm repl-bridge call list-debug-sessions
clpm repl-bridge call debug-eval-in-frame --session 1 --frame 4 --form 'x'
clpm repl-bridge call debug-invoke-restart --session 1 --name USE-VALUE \
  --args '["42"]'
clpm repl-bridge call debug-abort --session 1
```

If more than one debug session is active, always pass `--session N`.

## Everyday Calls

```sh
# image and package state
clpm repl-bridge call current-package
clpm repl-bridge call set-package --name CL-USER
clpm repl-bridge call image-info
clpm repl-bridge call loaded-systems
clpm repl-bridge call list-packages
clpm repl-bridge call gc --full true

# workers and recovery
clpm repl-bridge call list-workers
clpm repl-bridge call interrupt --worker default
clpm repl-bridge call reset --worker default
clpm repl-bridge call kill-worker --name scratch

# source and introspection
clpm repl-bridge call compile-file --path src/foo.lisp
clpm repl-bridge call load-file --path src/foo.lisp
clpm repl-bridge call find-definition --symbol my-function
clpm repl-bridge call xref --symbol my-function --direction calls
clpm repl-bridge call macroexpand --form '(my-macro x)' --full true
clpm repl-bridge call documentation --symbol my-function --type function
clpm repl-bridge call arglist --symbol my-function

# inspector sessions
clpm repl-bridge call inspect --form '(list :a :b :c)'
clpm repl-bridge call inspect-into --session ins-1 --i 0
clpm repl-bridge call inspect-eval --session ins-1 --form '(length *)'
clpm repl-bridge call inspect-close --session ins-1

# watch, trace, profile
clpm repl-bridge call watch --dir src --glob '*.lisp' --auto-revert true
clpm repl-bridge call list-watches
clpm repl-bridge call unwatch --id 1
clpm repl-bridge call trace --symbols '["my-fn"]'
clpm repl-bridge call untrace --symbols '["my-fn"]'
clpm repl-bridge call list-traced
clpm repl-bridge call time-eval --form '(some-fn)'
clpm repl-bridge call profile-eval --form '(big-fn)' --top 10
```

`call` emits raw JSON responses and streams raw event frames, which is the right
shape for agents and other tools.

## Cleanup Checklist

Before handing off, make the daemon state boring:

```sh
clpm repl-bridge call list-debug-sessions
clpm repl-bridge call debug-abort --session 1
clpm repl-bridge call list-watches
clpm repl-bridge call unwatch --id 1
clpm repl-bridge call list-traced
clpm repl-bridge call untrace
clpm repl-bridge call list-workers
clpm repl-bridge call kill-worker --name scratch
clpm repl-bridge call list-redefinitions
clpm repl-bridge daemon --status
```

Non-empty `list-redefinitions` means the image contains definitions that may
still need to be written to source. Use `daemon --stop` for normal shutdown and
let `daemon --status` or `daemon --stop` clean stale files; do not delete
`.clpm/repl-bridge.*` by hand unless the CLI cannot recover and the user agrees.

## Limits

- Request lines are capped at 64 KB; load larger forms from files.
- Eval output is capped at 1 MB combined stdout/stderr.
- Backtraces are capped and daemon frames are elided in `eval` human output.
- The daemon has the authority of the host process. Do not treat it as a
  sandbox.

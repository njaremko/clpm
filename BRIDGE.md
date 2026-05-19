# CLPM REPL Bridge — Design & Tracker

A persistent, project-scoped Lisp image that an LLM (or any subprocess-driving client) can drive over a Unix domain socket using line-delimited JSON-RPC. Built into clpm so users get LLM-friendly Lisp dev without installing anything else — no MCP server, no Slynk, no editor protocol.

This document is both the design and the implementation tracker. Tickets cite files and line numbers so they can be picked up cold.

Legend: `[ ]` open · `[~]` in progress · `[x]` done · Priorities: `P0` = critical path, `P1` = required for v1, `P2` = real productivity wins, `P3` = polish.

## Progress log

- `2026-05-19` Design drafted. 28 tickets enumerated below. No code yet.
- `2026-05-19` **#001 landed.** `clpm.io.json:read-json` and `read-json-from-string` parse exactly the encoding `write-json` accepts (`(:object ((k . v)…))`, `(:array (…))`, primitives). Recursive descent, surrogate-pair aware, integer/float split based on whether `.` or `e/E` appears. Rejects leading-zero numbers, trailing commas, unquoted keys, lone surrogates, and trailing non-whitespace per the spec. 10K-entry array parses in ~5 ms — well under the 200 ms ceiling. Test corpus in `test/io-json-roundtrip-test.lisp`.

## Lessons / decisions

(populated as work lands.)

---

## Motivation

The image-based workflow is Common Lisp's superpower for AI-assisted development. Hot-redefine one `defun`, re-run the failing test, repeat — no system reload, no FASL recompile, no second-spent waiting on ASDF. The current clpm test loop pays a 1-2 s ASDF load cost *per test file* because every test runs `sbcl --script` from scratch. With a persistent image that cost is amortized once.

The goal of this feature is to let an LLM drive a persistent Lisp image with the same affordances `M-x slime` gives a human: structured eval results, captured output, condition reporting, interrupt, package awareness.

## Non-goals

- **Not a SLIME/Slynk replacement for humans.** Editor integration is out of scope. If you want SLIME, you should use SLIME.
- **Not a debugger UI.** Restarts are *reported* but not *invokable* in v1 — LLMs interacting with restart menus is its own design problem.
- **Not multi-tenant.** One daemon per project, one client at a time. No request pipelining, no parallel evals. Add later if it matters.
- **Not network-exposed.** Unix domain socket only on POSIX. Loopback TCP with a token file on Windows (deferred). Never bind a public interface.
- **Not an MCP server.** No MCP framing, no `claude mcp add`, no JSON-RPC `Content-Length` headers. The wire protocol is plain line-delimited JSON over a local socket — usable by any process that can `connect()` and `read()`.

## Architecture

```
                                       ┌──────────────────────────────┐
                                       │   clpm repl-bridge daemon    │
                                       │   (long-running SBCL)        │
client                                 │                              │
─────────────────────────────────────  │  ┌────────────────────────┐  │
 $ clpm repl-bridge eval '(+ 1 2)' ──► │  │ accept-loop (main)     │  │
                                       │  │   reads one LDJSON     │  │
                                  ◄──  │  │   line, dispatches     │  │
                                       │  │                        │  │
                                       │  └─────────┬──────────────┘  │
                                       │            │                 │
                                       │            ▼                 │
                                       │  ┌────────────────────────┐  │
                                       │  │ eval worker (thread)   │  │
                                       │  │   evaluates one form   │  │
                                       │  │   at a time;           │  │
                                       │  │   interruptible        │  │
                                       │  └────────────────────────┘  │
                                       │                              │
                                       │  state: *package*, redef     │
                                       │  log, loaded systems, GC     │
                                       │  roots                       │
                                       └──────────────────────────────┘
                                                  ▲
                                                  │ Unix domain socket
                                                  │ .clpm/repl-bridge.sock
                                                  │ mode 0600
```

### Why this shape

- **One daemon per project, project-scoped socket.** Two clpm projects in different directories are mutually invisible. Daemons live alongside `.clpm/` (already excluded from the source tree by clpm convention).
- **Each `clpm repl-bridge eval` is a one-shot client.** The daemon persists state; the CLI does not. From the LLM's point of view, every form is a discrete tool call that returns a result — no stdin/stdout management, no `Monitor`-tool polling, no sentinel parsing. This is the single most important design decision: it matches how LLM tool-use actually works (stateless RPC, stateful server).
- **The CLI auto-starts the daemon if absent.** First `eval` after `clpm install` boots the daemon, loads the lockfile's systems, then runs the form. Subsequent evals attach to the running daemon in microseconds.
- **Filesystem permissions = auth.** Mode 0600 socket. No tokens, no TLS. The threat model is "untrusted local user," which Unix permissions already handle.
- **Worker thread + interrupt.** Eval runs on a dedicated thread. `sb-thread:interrupt-thread` signals a `user-interrupt` condition that unwinds cleanly. Client-side ctrl-C closes the socket; the daemon notices the broken pipe and interrupts the worker automatically.

## Wire protocol

Line-delimited JSON. Each request is one JSON object terminated by `\n`. Each response is one JSON object terminated by `\n`. No framing headers, no length prefixes, no batching.

### Request

```json
{"id": 17, "method": "eval", "params": {"form": "(+ 1 2)", "package": "cl-user"}}
```

Fields:
- `id` (integer or string, required): client-chosen correlation id, echoed in the response.
- `method` (string, required): one of the methods listed below.
- `params` (object, optional): method-specific arguments.

### Response (success)

```json
{"id": 17, "result": {"value": "3", "output": "", "error_output": "", "package": "COMMON-LISP-USER", "elapsed_ms": 1, "conditions": []}}
```

### Response (error)

```json
{"id": 17, "error": {"code": "reader-error", "message": "Unbalanced parenthesis", "details": "..."}}
```

Error codes (string, stable):
- `reader-error` — form couldn't be read (unbalanced parens, no such package, unknown character)
- `eval-error` — form evaluated but signaled a condition that escaped to the top level
- `interrupted` — eval was interrupted before completing
- `protocol-error` — request was malformed JSON, missing fields, or named an unknown method
- `worker-died` — the eval worker died catastrophically; daemon respawned it; client should retry
- `output-truncated` — eval completed but output exceeded the 1 MB cap; partial output returned

### Methods

| Method | Params | Result | Notes |
|--------|--------|--------|-------|
| `ping` | none | `{pid, uptime_ms, lisp, version}` | Liveness check. |
| `eval` | `{form: string, package?: string, timeout_ms?: int}` | `{value, output, error_output, package, elapsed_ms, conditions, redefined}` | Core. |
| `interrupt` | none | `{}` | Signals the worker if it's evaluating; no-op if idle. |
| `current-package` | none | `{package}` | Reports the daemon's persistent current package. |
| `set-package` | `{name: string}` | `{package}` | Sets the daemon's persistent current package. Equivalent to evaluating `(in-package ...)` but doesn't go through the worker. |
| `describe` | `{symbol: string, package?: string}` | `{output}` | Captures `(describe ...)` output as text. |
| `list-redefinitions` | none | `{entries: [{kind, name, package, form}]}` | All in-image redefinitions tracked this session. |
| `reset` | none | `{}` | Kills and respawns the worker thread. Loses captured state; keeps the daemon process. |
| `shutdown` | none | `{}` | Daemon exits cleanly. |

### Output and elapsed time

`output` is everything the form wrote to `*standard-output*`, captured as a UTF-8 string. `error_output` is the same for `*error-output*`. `elapsed_ms` is wall-clock from request receipt to response generation (excludes socket time). `value` is `(prin1-to-string result)` for single-value returns; multiple-value forms get the primary value only in v1.

### Conditions

`conditions` is an array of `{type, message, restarts: [string]}` for any conditions signaled and handled during eval. Empty array on success. For error responses (`eval-error`), the array contains the unhandled condition that escaped — same shape, but reported via `error` rather than `result`.

### Size limits

- Request body: 64 KB. Larger requests rejected with `protocol-error`.
- Response output: 1 MB combined (`output` + `error_output`). Excess is truncated and `code: output-truncated` is set on the response. Value and conditions are still reported.

## Subcommand surface

```
clpm repl-bridge serve   [--socket PATH] [--no-load] [-p MEMBER] [--preload SYS]...
clpm repl-bridge eval    FORM [--package PKG] [-p MEMBER] [--no-autostart]
clpm repl-bridge interrupt
clpm repl-bridge status
clpm repl-bridge stop
clpm repl-bridge describe SYMBOL [--package PKG]
clpm repl-bridge diff
clpm repl-bridge ping
```

`serve` is the daemon. Normally run via `Bash(run_in_background=true)` by the LLM, or from a shell by a human. Reads requests from the socket, never from its own stdin. Logs to `.clpm/repl-bridge.log`.

`eval` is a one-shot client. Connects to the socket, sends one `eval` request, prints the response as JSON to stdout (or as a rendered summary with `--pretty`), exits with status 0 on success, non-zero on error. Auto-starts the daemon if `--no-autostart` isn't passed.

The other client commands (`interrupt`, `describe`, `diff`, `ping`) all share the same one-shot pattern.

`status` reports daemon state by reading `.clpm/repl-bridge.pid`. `stop` sends `shutdown`, waits up to 5 s, then `SIGTERM`s the PID. Both are pure CLI conveniences; no daemon round-trip required if the pidfile is absent.

## Lifecycle

1. **First `clpm repl-bridge eval` in a project.** No socket; client forks a daemon (`clpm repl-bridge serve --daemonize`), waits for the socket to appear (poll with 100 ms ticks, 5 s ceiling), sends the request.
2. **Daemon startup.** Creates `.clpm/`, writes `repl-bridge.pid`, binds the socket at mode 0600. Loads the project's lockfile-resolved systems via the same path `cmd-repl` uses today. Loads the bridge protocol module. Enters the accept loop.
3. **Subsequent evals.** Socket exists; client connects and sends. Microseconds of overhead.
4. **Worker death.** If the eval worker thread dies (uncaught condition outside the eval handler, OOM, etc.), the daemon respawns it, logs the death, and replies `worker-died`. Client is told to retry.
5. **Daemon death.** Socket goes stale. Next `eval` finds the connect failing, removes the stale socket, auto-respawns. Pidfile is repaired on startup.
6. **Project teardown.** `clpm clean` could optionally call `stop`. Default: leave the daemon running until explicitly stopped or the user reboots.

## File layout

```
src/io/json.lisp                 # +read-json (#001)
src/repl_bridge/protocol.lisp    # Wire encoding/decoding, error helpers
src/repl_bridge/server.lisp      # Daemon: socket accept, worker thread
src/repl_bridge/client.lisp      # One-shot client used by `eval`, etc.
src/repl_bridge/redef.lisp       # Redefinition tracking
src/commands.lisp                # cmd-repl-bridge dispatching on subcommands
test/repl-bridge-*-test.lisp     # Tests, one file per concern
```

A new `clpm.repl-bridge` package owns the server/client code. `clpm.commands` only sees the public surface (`start-server`, `with-client`, `send-request`).

---

## Tickets

### #001 — `[x]` `P0` `io` `protocol` Implement `clpm.io.json:read-json`

The wire protocol is line-delimited JSON. The daemon needs a reader; the writer already exists at `src/io/json.lisp:51`. No third-party JSON library is acceptable (CLPM bootstraps without Quicklisp).

**Scope**

- Read `null`, `true`, `false`, strings (with `\"`, `\\`, `\/`, `\b`, `\f`, `\n`, `\r`, `\t`, `\uXXXX`), integers, floats, arrays, objects.
- Return values in the same encoding the writer accepts:
  - object → `(:object ((key . val) ...))`
  - array  → `(:array (val ...))`
  - others → string / integer / float / `t` / `nil` / `:false`
- Read from a string (`read-json-from-string`) or from a stream (`read-json`); both consume exactly one value and leave trailing whitespace / following content alone.
- Reject malformed input with `clpm-parse-error`, including a 1-based column number when feasible.

**Acceptance criteria**

- A new test `test/io-json-roundtrip-test.lisp` constructs a corpus of values (nested objects, arrays, all escape sequences, unicode BMP characters, integer/float boundary cases including negative, scientific notation), writes them via `write-json-to-string`, reads them back via `read-json-from-string`, and asserts equality of the canonical re-write.
- Reader rejects: trailing garbage, unterminated string, invalid escape, lone `}`, `[1,]`, `{,}`, `01` (leading-zero number per JSON spec), bare identifiers (`undefined`).
- Reader handles a 1 MB input without quadratic blowup (test asserts elapsed under 200 ms).

### #002 — `[x]` `P0` `repl-bridge` `daemon` Socket server skeleton

A new module `src/repl_bridge/server.lisp` exposes `start-server` accepting a socket path. The function:

- Binds an SBCL `sb-bsd-sockets:local-socket` at `:type :stream`, calls `bind`, then `chmod 0600` on the resulting path via `sb-posix:chmod`.
- `listen`s with a small backlog (8 is plenty).
- Enters an accept loop: for each connection, read one request line, dispatch on `method`, write one response line, close.
- Catches `clpm-parse-error` and any malformed JSON, replies with `{"error": {"code": "protocol-error", ...}}`.
- Single-threaded request handling; no concurrent clients. If a second client connects mid-request the second is queued by `listen`'s backlog.

**Acceptance criteria**

- `(start-server "/tmp/cl-bridge-test.sock")` listens and answers a `ping` request with a well-formed response.
- Socket is mode 0600 after bind.
- Server cleans up the socket file on graceful shutdown (`unwind-protect`).
- New test `test/repl-bridge-server-test.lisp` exercises round-trip ping using `sb-bsd-sockets` as the client; no clpm CLI involved.

### #003 — `[x]` `P0` `repl-bridge` `eval` Worker thread with eval and output capture

The server delegates eval to a dedicated `sb-thread:thread`. The worker:

- Owns the daemon's persistent `*package*`.
- For each `eval` request:
  1. Binds `*standard-output*`, `*error-output*`, `*trace-output*`, `*debug-io*`, `*query-io*`, `*terminal-io*` to fresh string streams (or `make-broadcast-stream` for `*terminal-io*` so both directions are silenced).
  2. Binds `*standard-input*` to an empty `make-string-input-stream`, so user code calling `read` gets EOF rather than hanging.
  3. Calls `read-from-string` on the form. Reader errors become `reader-error` responses (no eval attempted).
  4. Wraps `eval` in `handler-case` catching `error`. Uncaught conditions become `eval-error` responses with type/message; the worker survives.
  5. Captures `prin1-to-string` of the primary return value.
  6. Reports elapsed wall-clock time.
- Survives any condition that doesn't kill the thread; signals worker-death to the supervisor on unrecoverable failure.

**Acceptance criteria**

- `eval '(+ 1 2)'` returns `"3"`.
- `eval '(format t "hi")'` returns `value: "NIL"`, `output: "hi"`.
- `eval '(error "boom")'` returns `error: {code: "eval-error", message: "boom"}`, daemon stays alive.
- `eval '(read)'` does not hang; reads EOF and either errors cleanly or returns NIL depending on the form.
- New test `test/repl-bridge-eval-test.lisp` covers all four cases plus output-after-error (output flushed before condition handling).

### #004 — `[x]` `P0` `repl-bridge` `client` One-shot client

A small client (`src/repl_bridge/client.lisp`) that connects to a socket path, sends one request, reads one response, returns the parsed object. Handles:

- Connect failure → returns `(:no-daemon ...)` so callers can decide to auto-start.
- Broken pipe mid-read → returns `(:io-error ...)` for the same reason.
- Slow daemon → optional `:timeout` keyword (default unset, meaning "wait forever"); on timeout, close the socket so the daemon notices and interrupts the eval.

**Acceptance criteria**

- `with-client (c socket-path) (send-request c '("eval" (("form" . "(+ 1 2)"))))` returns the parsed result object.
- Connect to a non-existent socket returns `(:no-daemon ...)` without raising.
- Test `test/repl-bridge-client-test.lisp` covers connect, send, receive, timeout-closes-socket.

### #005 — `[x]` `P0` `repl-bridge` `cli` `cmd-repl-bridge` dispatch

A new `cmd-repl-bridge` in `src/commands.lisp`, dispatching on the first arg:

```
clpm repl-bridge serve [--socket PATH] [--no-load] [-p MEMBER] [--preload SYS]...
clpm repl-bridge eval FORM [--package PKG] [-p MEMBER] [--no-autostart]
clpm repl-bridge interrupt
clpm repl-bridge status
clpm repl-bridge stop
clpm repl-bridge ping
```

Help text (`print-command-help :repl-bridge`) with per-subcommand drilling per #017 of the main tracker.

**Acceptance criteria**

- `clpm repl-bridge` with no args prints the usage line and exits 1.
- `clpm repl-bridge help <subcommand>` prints the focused page.
- Unknown subcommand prints "Unknown subcommand: X" and exits 1.
- New test `test/repl-bridge-help-test.lisp` matches the existing per-subcommand help test pattern.

### #006 — `[x]` `P0` `repl-bridge` `cli` `serve` end-to-end

`clpm repl-bridge serve` wires #002 + #003 + project loading (reuse the existing project-discovery and lockfile-resolution path from `cmd-repl`). Forks via `sb-daemon:daemonize` (or a homegrown fork-and-detach if `sb-daemon` is unavailable on macOS — check). Writes `.clpm/repl-bridge.pid`, binds `.clpm/repl-bridge.sock`, redirects stdout/stderr to `.clpm/repl-bridge.log`.

**Acceptance criteria**

- `clpm repl-bridge serve` in a project with a lockfile starts a daemon that has the lockfile-resolved systems loaded (verifiable: a subsequent `eval '(asdf:registered-system "alexandria")'` returns non-nil).
- Two consecutive `serve` invocations in the same project: the second exits non-zero with "daemon already running (pid N, socket .clpm/repl-bridge.sock)".
- Pidfile and socket cleaned up on normal shutdown.

### #007 — `[x]` `P0` `repl-bridge` `cli` `eval` end-to-end with auto-start

`clpm repl-bridge eval FORM` connects to `.clpm/repl-bridge.sock`. If absent or connect fails, spawn `serve --daemonize`, wait up to 5 s for the socket to appear, retry. Then send the request, print the JSON result line, exit with status 0 on success, non-zero on `error`-shaped responses.

Pretty mode: `--pretty` reformats the response as a human-readable summary (`value=...; output:\n...; error: ...`). Default mode is raw JSON, ideal for LLM consumption and `jq` piping.

**Acceptance criteria**

- `clpm repl-bridge eval '(+ 1 2)'` in a fresh project prints a JSON response with `value: "3"`. Daemon was auto-started.
- Second `eval` in the same project reuses the daemon (no new pidfile, same pid).
- `--no-autostart` errors with "no daemon running" if the socket is absent.
- Exit code is 0 for success, 1 for `error`-shaped response, 2 for transport failure (no daemon when autostart is disabled).
- Test `test/repl-bridge-eval-cli-test.lisp` exercises all four cases.

### #008 — `[x]` `P1` `repl-bridge` `interrupt` Interrupt mid-eval

Two paths:

- **Client-side broken pipe.** Client closes its socket (e.g. user ctrl-C'd `clpm repl-bridge eval`). Daemon's accept-loop notices EOF on `read-line` or write failure on `write-line`. If a worker is mid-eval, the daemon calls `sb-thread:interrupt-thread worker (lambda () (signal 'clpm.repl-bridge:user-interrupt))`. The worker's eval handler catches the interrupt, unwinds, replies `interrupted` (but the client is gone — that's fine, the daemon discards the response).
- **Explicit `interrupt` method.** Client sends `interrupt` over a *separate* connection (the original eval connection is busy reading the response). Daemon signals the worker as above, replies `{}` immediately to the interrupt request, then writes `interrupted` to the original eval connection.

`clpm repl-bridge interrupt` uses the explicit method.

**Acceptance criteria**

- `clpm repl-bridge eval '(loop)'` from one terminal; `clpm repl-bridge interrupt` from another within 1 s; both commands return within 2 s. The eval terminal sees a JSON response with `code: "interrupted"`.
- Ctrl-C on a running `clpm repl-bridge eval '(sleep 100)'` interrupts the daemon's worker within 500 ms (verified by sending a subsequent `ping` that succeeds).
- Test `test/repl-bridge-interrupt-test.lisp` covers both paths.

### #009 — `[x]` `P1` `repl-bridge` `eval` Condition marshalling

Capture details on conditions:

- `type` is the class name as a fully-qualified string: `"COMMON-LISP:SIMPLE-ERROR"`.
- `message` is `(princ-to-string condition)`.
- `restarts` is a list of restart names available at the point of signaling: `[ "ABORT", "CONTINUE", "USE-VALUE" ]`. Not invokable in v1 — informational only.
- For `eval-error`, include the first 16 frames of the backtrace as strings (via `sb-debug:list-backtrace` or equivalent). Skip frames from inside the bridge itself.

**Acceptance criteria**

- `eval '(error "boom")'` returns an `eval-error` whose `conditions[0].type` is `"SIMPLE-ERROR"`, `message` contains `"boom"`, and `restarts` includes `"ABORT"`.
- A condition signaled but *handled* within the form (e.g. `(handler-case (error "x") (error () 1))` returning `1`) appears in `conditions: []` — handled conditions are not reported.
- Backtrace omits bridge internals; first frame is in user-supplied code.

### #010 — `[x]` `P1` `repl-bridge` `package` Package persistence and per-call override

The daemon maintains `clpm.repl-bridge::*current-package*` as the persistent current package. Initial value: `(find-package "CL-USER")` (after `--no-load`) or the first package the project loads.

- `eval` with no `package` param uses the persistent current package.
- `eval` with a `package` param binds `*package*` for that call only; does not mutate the persistent state.
- A form that calls `(in-package :foo)` *does* mutate the persistent state (because the side effect on `*package*` persists across the eval scope).
- `current-package` and `set-package` methods report and mutate the persistent state without going through the worker.

Per-call package precedence: `params.package` > daemon's `*current-package*`.

**Acceptance criteria**

- Call 1: `eval '(defpackage :test-pkg)' 'cl-user'` → ok. Call 2: `eval '(in-package :test-pkg)' 'cl-user'` → ok, sets persistent. Call 3: `eval '(package-name *package*)'` → `"TEST-PKG"`.
- `eval '(package-name *package*)' --package 'cl'` → `"COMMON-LISP"`. The persistent package is unchanged.
- Test `test/repl-bridge-package-test.lisp`.

### #011 — `[x]` `P1` `repl-bridge` `lifecycle` Pidfile + socket-path management

Both daemon and client agree on:

- Pidfile: `.clpm/repl-bridge.pid`. Contains the daemon PID as a decimal string on a single line.
- Socket: `.clpm/repl-bridge.sock`. Created mode 0600.
- Log: `.clpm/repl-bridge.log`. Append-only stdout/stderr capture.
- Lock: `.clpm/repl-bridge.lock` (via `clpm.platform:with-file-lock` from #013 of the main tracker) — held during pidfile-write to prevent two `serve` invocations racing.

Stale-pid detection: on `serve` startup, if pidfile exists, read the PID and `kill(pid, 0)` (via `sb-posix:kill` with signal 0); if it fails with ESRCH, the previous daemon is gone — remove stale pidfile and socket, proceed. Otherwise, exit with "daemon already running."

**Acceptance criteria**

- `kill -9` the daemon, then `clpm repl-bridge eval ...` from a fresh shell: client detects no listener, removes stale files, auto-starts a new daemon. No human intervention required.
- Two concurrent `serve` invocations: one wins (acquires the lock and the socket), the other exits with a clear message naming the live PID.
- `clpm repl-bridge status` reports pid, socket path, log path, uptime, and the project root the daemon is rooted at.

### #012 — `[x]` `P1` `repl-bridge` `lifecycle` `status` and `stop`

`status`:

- No pidfile → "not running".
- Pidfile + live process + responsive socket → "running (pid N, uptime X)".
- Pidfile + live process + unresponsive socket → "running but unresponsive (pid N) — try `clpm repl-bridge stop`".
- Pidfile + dead process → "stale pidfile (cleaned)" and removes it.

`stop`:

- If running, send `shutdown` method; daemon exits cleanly within ~1 s.
- If unresponsive after 5 s, `kill TERM` the pid; remove pidfile and socket.
- If not running, no-op with exit 0.

**Acceptance criteria**

- `status` reports each of the four states correctly. Test asserts the exit code (0 for any well-determined state).
- `stop` is idempotent — running it twice in a row succeeds both times.

### #013 — `[x]` `P1` `repl-bridge` `tests` End-to-end smoke test

Black-box test of the whole feature using the actual CLI: spawn a daemon via `clpm repl-bridge serve`, run a series of `clpm repl-bridge eval` commands, assert observed JSON output. Tear down with `stop`.

Coverage:

- Cold start (no daemon) auto-spawns and runs an eval.
- Three consecutive evals share state (`(defvar *x* 1)` then `*x*` returns 1).
- Eval of a form that errors returns the right `error` shape.
- `stop` cleans up files.

**Acceptance criteria**

- `test/repl-bridge-e2e-test.lisp` runs in under 10 s and asserts each case.
- No leaked daemons after the test exits (pgrep-style check at teardown).

### #014 — `[x]` `P1` `repl-bridge` `help` Per-subcommand help

Per the pattern established in main-tracker #017: `clpm help repl-bridge` shows a top-level page; `clpm help repl-bridge eval`, `clpm help repl-bridge serve`, etc. show focused pages. `print-command-help` gains a `:repl-bridge` branch with sub-subcommand drilling.

**Acceptance criteria**

- Five focused pages: `serve`, `eval`, `interrupt`, `status`, `stop`. Each cites its flags and a short example.
- `test/help-output-test.lisp` extended to assert each page contains its `Usage: clpm repl-bridge <sub>` line and that the umbrella usage doesn't leak.

### #015 — `[x]` `P1` `repl-bridge` `safety` Output size cap

Worker captures `*standard-output*` to a string-output stream. When the captured length exceeds 1 MB (sum of stdout + stderr), close the form's output streams to a sink (further writes silently discarded) and set a flag. The response carries `code: "output-truncated"` but still includes the truncated head.

**Acceptance criteria**

- `eval '(loop (format t "x"))'` (interrupted at some bound) returns a response with truncated output, ~1 MB in size, `code: "output-truncated"`, and the daemon survives.
- A non-truncating eval (output < 1 MB) does not carry the truncated code.

### #016 — `[x]` `P1` `repl-bridge` `safety` Request size cap

Daemon refuses requests larger than 64 KB. The first `read-line` is bounded; if the line exceeds the cap, the daemon reads-and-discards the rest, replies `protocol-error`, and closes the connection.

**Acceptance criteria**

- 64 KB request: accepted.
- 64 KB + 1 byte request: rejected with `protocol-error`; daemon remains responsive.

### #017 — `[x]` `P2` `repl-bridge` `redef` Track redefined top-level forms

A pre-eval pass walks the form (one level deep — no recursion into macroexpansion) looking for top-level definers. For each, record:

```
{kind, name, package, form, recorded-at}
```

Definers tracked: `defun`, `defmethod`, `defmacro`, `defgeneric`, `defclass`, `defstruct`, `defvar`, `defparameter`, `defconstant`, `define-condition`, `defpackage`. Storage: a `defvar` hash keyed on `(kind name package)`, value is the latest record.

The persistent log survives across evals but resets on `reset` or daemon restart.

**Acceptance criteria**

- After `eval '(defun foo () 1)'`, `list-redefinitions` returns one entry with `kind: "defun"`, `name: "FOO"`, `package: "<current>"`.
- Redefining the same function returns *one* entry (the most recent form), not two.
- `defmethod` on the same `defgeneric` is tracked per method (qualifiers + specializers in the key).

### #018 — `[x]` `P2` `repl-bridge` `redef` `diff` subcommand

`clpm repl-bridge diff` walks the redefinition log; for each entry, locates the symbol's source file via `sb-introspect:find-definition-source` (or the equivalent), reads the file, finds the corresponding top-level form, and diffs the recorded form vs. the file form. Output is a list of `{kind, name, package, status}` where status is one of `up-to-date`, `differs`, `not-in-source`.

This is the *state drift* mitigation. Run before declaring work done.

**Acceptance criteria**

- Redefine `foo` in-image; without saving to disk, `diff` reports `foo` as `differs` (or `not-in-source` if the function originally came from a Quicklisp registry where editing isn't expected).
- Save the matching form to the source file; `diff` now reports `up-to-date`.
- Doesn't false-positive on forms whose recorded printed form differs only in whitespace.

### #019 — `[x]` `P2` `repl-bridge` `describe` `describe` and `set-package` methods

`describe` captures `(describe symbol)` output as a string. `set-package` mutates the daemon's persistent current package without going through the worker (so it can't be interrupted and can't error from user code).

**Acceptance criteria**

- `describe '(("symbol" . "car"))'` returns text containing `"COMMON-LISP:CAR"` and `"function"`.
- `set-package '(("name" . "cl-user"))'` returns `{package: "COMMON-LISP-USER"}` and the next `eval` reports the new package.

### #020 — `[x]` `P2` `repl-bridge` `lifecycle` `reset` method

`reset` kills the worker thread (via `sb-thread:terminate-thread`) and spawns a fresh one. Useful when the worker's state is wedged (corrupt `*readtable*`, accidentally redefined `cl:car`, etc.) but the daemon is still serviceable.

Resets:
- Worker `*package*` (reverts to the daemon's persistent current package)
- Captured streams
- Redefinition log (with a warning in the response, so the LLM knows)

Does not affect:
- Loaded systems (those are in the daemon's image, not the worker's stack)
- The daemon's persistent current package setting

**Acceptance criteria**

- `eval '(let ((*readtable* (copy-readtable))) (set-syntax-from-char #\\( #\\Space) :ok)'` corrupts the readtable. Next `eval '(+ 1 2)'` errors with reader confusion. After `reset`, `eval '(+ 1 2)'` returns 3.

### #021 — `[x]` `P2` `docs` `skill` Claude Code skill markdown

Ship `.claude/skills/clpm-repl-bridge.md` documenting how an LLM should drive the bridge:

- "Always run `clpm repl-bridge eval` for one-shot forms; the daemon persists state between calls."
- "Prefer redefining a single `defun` over `asdf:load-system` reloads — much faster."
- "After non-trivial in-image redefinitions, run `clpm repl-bridge diff` to see what's drifted from the source files."
- "If a form hangs, run `clpm repl-bridge interrupt` from another shell or via a second tool call."
- "The eval response is one line of JSON: parse `result.value` (string), `result.output` (string), `error.code` if present."

The skill is invoked via `/skill clpm-repl-bridge` in Claude Code, or auto-loaded from the project's `.claude/skills/` directory.

**Acceptance criteria**

- File exists with at least the bullets above.
- Skill text is under 200 lines; LLM-pasteable and skimmable.
- README "AI-assisted development" subsection points at it.

### #022 — `[x]` `P2` `docs` `readme` "AI-assisted development" section

Add a README subsection (after "Trust & provenance" probably) explaining the bridge: what it is, why it exists, a 5-line copy-paste example showing `clpm repl-bridge serve` + an eval, and a pointer to the skill.

**Acceptance criteria**

- README has a clear section header.
- Example is copy-pasteable and works from `clpm new myproj --bin && cd myproj`.

### #023 — `[x]` `P3` `repl-bridge` `health` `ping` and backtrace

`ping` returns daemon health: pid, uptime, lisp impl/version, count of evals serviced. Cheap; doesn't touch the worker.

Backtrace in `eval-error` responses is the first 16 stack frames, skipping bridge-internal frames. Use `sb-debug:list-backtrace` (returns a list of frames) and `princ-to-string` each frame.

**Acceptance criteria**

- `ping` round-trip is under 5 ms wall-clock on localhost.
- An `eval-error` includes `error.backtrace` as an array of up to 16 strings, none beginning with `CLPM.REPL-BRIDGE::` or `SB-IMPL::`.

### #024 — `[ ]` `P3` `repl-bridge` `cross-platform` Windows TCP fallback

Windows lacks Unix domain sockets (until very recent builds). Fall back to a loopback TCP socket bound to `127.0.0.1` on a random ephemeral port; write `.clpm/repl-bridge.port` containing the port and a 32-hex-char shared token. Every request must include `"token": "..."` in its params; daemon rejects requests without the matching token.

Deferred unless a Windows user files an issue.

**Acceptance criteria**

- `cmd-repl-bridge` detects the OS and uses the right transport.
- Windows tests skip with a clear message if not running on Windows.

### #025 — `[ ]` `P3` `repl-bridge` `config` Manifest-level autostart and preload

Allow projects to express:

```lisp
:repl-bridge (:autostart t :preload ("clpm" "alexandria"))
```

`autostart: t` means `clpm install` ends with a `clpm repl-bridge serve --daemonize` if no daemon is running. `preload` is a list of additional systems to `asdf:load-system` after the lockfile-resolved ones.

**Acceptance criteria**

- Manifest parser (`src/project.lisp`) accepts the new `:repl-bridge` field, round-trips it.
- `clpm install` honors `autostart: t`; daemon comes up with the listed systems loaded.

### #026 — `[ ]` `P3` `repl-bridge` `preload` Lockfile-driven preload

`clpm repl-bridge serve` loads the project's lockfile-resolved systems on startup. Already implicit via #006 if `cmd-repl`'s loading is reused — promote it to an explicit, tested guarantee. The systems loaded are precisely those `clpm.solver:resolution-to-load-order` would return.

**Acceptance criteria**

- After `serve`, `eval '(mapcar #\'asdf:component-name (asdf:already-loaded-systems))'` returns the lockfile's resolved systems plus their transitive deps.
- `--no-load` disables this; daemon starts with no project systems loaded.

### #027 — `[x]` `P3` `repl-bridge` `obs` Structured event log

Daemon writes one JSON line per event to `.clpm/repl-bridge.log`: `{ts, event, id?, method?, elapsed_ms?, error?}`. Events: `accept`, `request`, `response`, `interrupt`, `worker-died`, `shutdown`.

Lets the user (or LLM via `Bash(rg <error> .clpm/repl-bridge.log)`) see what's been happening without enabling verbose mode at the protocol layer.

**Acceptance criteria**

- One-line-per-event format; `jq` can consume the log.
- Log rotates after 10 MB (rename to `.1`, start fresh).

### #028 — `[ ]` `P3` `repl-bridge` `cross-impl` CCL/ECL support

The bridge core is SBCL-specific in two places: thread interrupt (`sb-thread:interrupt-thread`) and socket binding (`sb-bsd-sockets`). Generalize:

- Thread interrupt: `bordeaux-threads` is not an option (no QL dep), so use `clpm.lisp:lisp-run-argv`-style impl dispatch with hand-rolled functions per impl. CCL has `ccl:process-interrupt`. ECL has `mp:process-interrupt`.
- Sockets: similar dispatch. CCL has `ccl:make-socket`. ECL has `sb-bsd-sockets` (lib provided).

Defer unless a CCL/ECL user wants it.

**Acceptance criteria**

- The bridge starts and serves an eval on CCL and ECL.
- Each impl's interrupt path is tested.

---

## Suggested implementation order

Land in dependency order; each ticket is roughly self-contained but the daemon won't function end-to-end until #001-#007 are all in:

1. **#001** (JSON reader) — unblocks everything.
2. **#002 + #003 + #004** (server skeleton, eval worker, client). These three can land as a single PR or as three small ones; together they make a working daemon talking to a working client.
3. **#005 + #006 + #007** (CLI wiring). End of week one: `clpm repl-bridge eval '(+ 1 2)'` works on a fresh project.
4. **#008-#012** (interrupt, conditions, package, pidfile, status/stop). End of week two: feature is robust enough to use daily.
5. **#013-#016** (e2e tests, help, output cap, request cap). Hardening pass.
6. **#017-#020** (redefinition tracking, describe, reset). The productivity multiplier.
7. **#021-#022** (skill + README). User-facing documentation.
8. **#023-#028** (ping, backtrace, Windows, manifest config, preload formalization, event log, cross-impl). Polish, in any order.

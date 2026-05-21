# CLPM - Common Lisp Package Manager

A technically excellent package manager for Common Lisp with proper dependency resolution, cryptographic integrity verification, and reproducible builds.

## Features

- **Deterministic dependency resolution** via backtracking with reason-chain conflict explanations
- **Content-addressed store** for sources and build artifacts
- **Cryptographic integrity** - all artifacts verified by SHA-256
- **Registry authentication** via Ed25519 signatures
- **Implementation-keyed build cache** - build outputs keyed by Lisp impl/version, platform, and compile settings
- **Per-project lockfiles** for reproducible builds
- **No Quicklisp dependency** - bootstraps from SBCL alone

## Installation

### From Source

```bash
sbcl --script clpm-bootstrap.lisp install-local .
```

### From Release

```bash
curl -fsSL https://github.com/clpm/clpm/releases/download/v0.1.0/clpm-bootstrap.lisp | sbcl --script
```

## Quick Start

```bash
# Check your environment (will report issues if not configured)
clpm doctor

# Quicklisp (online): configure + pin distinfo on first update
clpm registry add --quicklisp --trust tofu
clpm registry update quicklisp

# Create a new project
clpm project new myproject --bin
cd myproject

# Add deps from Quicklisp (nil constraint = "any"; lockfile pins a version)
clpm deps add alexandria
clpm deps add --test fiveam
clpm deps sync

# Run commands
clpm run test
clpm run

# Produce a distributable executable in dist/
clpm project package
./dist/myproject

# Optional: configure a signed git registry (example values)
clpm registry add --name main --url https://github.com/clpm/clpm-registry.git --trust ed25519:...
```

## Trust & provenance

Quicklisp dists are not signed. CLPM supports explicit trust modes for
Quicklisp by pinning the `distinfo.txt` hash. Configure TOFU when adding the
registry, then let the first update record the observed digest:

```bash
clpm registry add --quicklisp --trust tofu
clpm registry update quicklisp
clpm registry trust list
```

If Quicklisp changes and your pin no longer matches, refresh it explicitly:

```bash
clpm registry trust refresh quicklisp
```

Registry trust values are intentionally closed: signed git registries use
`ed25519:<key-id>`, and Quicklisp registries use `tofu` or
`sha256:<64-hex-digest>`. The CLI does not provide a persistent "no trust"
mode; use scoped `--insecure` on verifier-bearing commands only for explicit
debugging.

To inspect what CLPM actually used, run:

```bash
clpm deps audit
clpm deps audit --json
```

To generate a deterministic SBOM from your lockfile:

```bash
clpm deps sbom --format cyclonedx-json --out sbom.json
```

## AI-assisted development: the REPL operator manual

CLPM ships `clpm repl`: a persistent project-scoped SBCL daemon that
answers JSON-RPC over a Unix socket. It exists so an LLM (or any non-Lisp
tool) can drive a long-lived image — redefining one `defun` instead of
reloading systems, capturing stdout/stderr per call, surfacing in-image drift
from disk.

```bash
clpm deps sync                                       # one-time setup
clpm repl daemon --detach                           # background daemon
clpm repl daemon --status --json                    # machine-readable status
clpm repl eval '(asdf:load-system "my-app")'
clpm repl eval '(my-app:hello)'
clpm repl call methods                              # list callable daemon RPCs
clpm repl daemon --stop
```

State persists across `eval` calls. Hung evals are unwound with
`clpm repl call interrupt` (daemon stays up); a wedged worker is
recovered with `clpm repl call reset`.
Use `clpm repl eval FORM` for evaluation; `clpm repl call` is not an
alternate spelling of the daemon's eval RPC. `call` requires an existing
daemon; it never starts one implicitly. Transport fields such as
`project_root` and `token` are owned by CLPM and cannot be supplied through
`call` parameters.

### Capabilities

- **Interactive debugger.** `eval --debug` pauses on the first
  unhandled condition, emits a live restart chain, and accepts
  restart and frame-selection flags. Kept sessions are driven with
  `call debug-invoke-restart`, `call debug-eval-in-frame`,
  `call debug-continue`, and `call debug-abort`.
- **Inspector sessions.** `call inspect --form FORM` returns paginated parts of any
  value; `inspect-into` / `inspect-pop` walk the structure; `inspect-eval`
  binds `*` to the focus; `inspect-mutate` replaces an element.
- **Compile diagnostics.** `call compile-file --path PATH` returns warnings/errors with
  file + line positions, suitable for surfacing in an editor.
- **Source navigation.** `call find-definition`, `call xref`, `call apropos`,
  `call documentation`, `call arglist`, `call complete-symbol`,
  `call disassemble`, and `call describe-system`.
- **Named workers.** `eval --worker NAME` runs on an isolated worker with
  its own `*package*`, history, and redefinition log. `call list-workers`
  and `call kill-worker --name NAME` manage them.
- **Trace.** `call trace`, `call untrace`, and `call list-traced`.
  Use `repl eval` for explicit timing or profiling forms inside the image.
- **File watching.** `call watch --dir DIR --glob '*.lisp' --auto-revert true` polls and
  re-LOADs matching files on mtime change, streaming `file-reloaded` /
  `reload-failed` / `revert-applied` events.
- **Image introspection.** `image-info`, `loaded-systems`, `list-packages`,
  `gc`, all through `call METHOD`.
- **Self-documenting.** Public callable methods are published in the registry
  the dispatcher exposes. `call methods` lists them; `call help --method
  METHOD` returns the long doc + parameter schema.

Run `clpm skill` for agent recipes, and `clpm help repl` for the
three-command REPL surface.

## Project File Format

The `clpm.project` file is a data-only S-expression:

```lisp
(:project
  :name "my-app"
  :version "0.1.0"
  :systems ("my-app")
  :depends
    ((:dep :system "alexandria" :constraint (:semver "^1.4.0"))
     (:dep :system "hunchentoot" :constraint (:exact "1.3.0")))
  :test-depends
    ((:dep :system "fiveam" :constraint (:semver "^1.4.0")))
  :registries
    ((:git :url "https://example.org/clpm-registry.git"
           :name "main"
           :trust "ed25519:abcd..."))
  :sbcl (:min "2.4.0" :max nil)
  :build (:jobs 8 :compile (:debug nil :speed 3 :safety 1)))
```

### Constraint Types

- `(:semver "^1.2.3")` - Caret range: `>=1.2.3 <2.0.0`
- `(:semver "~1.2.3")` - Tilde range: `>=1.2.3 <1.3.0`
- `(:semver ">=1.0 <2.0")` - Comparison range
- `(:exact "1.2.3")` - Exact version
- `(:git :url "..." :ref "...")` - Git source override
- `(:path "../local-lib")` - Local path override

### Quicklisp caveats

Quicklisp does not publish a version constraint model, so CLPM has to
synthesize one. The result is correct but mildly counter-intuitive:

- **Versions are derived from the Quicklisp dist date.** A release in the
  `quicklisp-2024-10-12` dist gets the synthetic version `20241012`. There
  is no per-release version; every system in a given dist shares the same
  version. This is the lever Quicklisp gives us — bumping the dist
  updates every QL-sourced system at once.
- **Transitive dependencies parsed from `systems.txt` get a `nil`
  constraint** (i.e. "any version"). Quicklisp publishes the dependency
  graph but not version constraints between systems. In practice this
  means QL-sourced dependencies will resolve to whatever the registry has
  available — there is no way for `bordeaux-threads` to express "I need
  alexandria >= 1.4" from QL data alone.
- **Mixing Quicklisp and Git registries is supported.** Constraints from
  your `clpm.project` apply uniformly. If you write `alexandria@^1.4.0`
  in your manifest, the solver will honor that even when alexandria
  comes from Quicklisp; but a transitive `(alexandria nil)` from another
  QL system will accept any version. Pin the version explicitly in your
  manifest if you care.

If you need a different version of a Quicklisp-sourced library than the
dist provides, override it with a `(:git ...)` or `(:path ...)` source in
your manifest.

### How resolution works

CLPM uses a deterministic depth-first backtracking solver (not PubGrub,
despite some legacy comments). The rules a user needs to know:

1. **System pick order is alphabetical** by system id.
2. **Within a system, candidates are ordered highest-version-first**,
   with the lockfile's previously-chosen version (if any) lifted to the
   front so re-runs are stable.
3. **`clpm deps update <sys>`** lifts the lockfile preference for the named
   systems only; everything else is held at its current pin unless a
   transitive constraint forces it to move. Unlocked systems are
   selected first so any constraints they propagate land before still-
   locked systems are bound.
4. **Conflicts produce a reason chain** ("system X requires Y at A, but
   Z requires Y at B") rather than a derivation graph. The chain is the
   actual sequence of decisions that led to the conflict, so it's
   reproducible even on re-runs.
5. **Same inputs always produce the same lockfile.** The alphabetical
   tie-break plus deterministic candidate ordering means there is no
   timestamp / iteration-order / hash-randomization dependence anywhere.

## Commands

Bare `clpm` prints the command schema. Commands that change project state are
explicit resource operations.

| Command | Description |
|---------|-------------|
| `clpm help <command> [subcommand ...]` | Show command-specific help |
| `clpm doctor` | Check environment and configuration |
| `clpm project new <name> --workspace [--dir <path>]` | Create a workspace scaffold |
| `clpm project new <name> --bin\|--lib [--dir <path>]` | Create a project scaffold |
| `clpm project new <name> --bin\|--lib --member-of <workspace-dir>` | Create and register a workspace member |
| `clpm project init [name]` | Initialize new project |
| `clpm project workspace <init\|add\|remove\|list> ...` | Workspace management |
| `clpm project package` | Build a distributable executable |
| `clpm deps add [--dev\|--test] [--any\|--caret] ... <system>...` | Add one or more dependencies |
| `clpm deps remove [--dev\|--test] <system>` | Remove a dependency |
| `clpm deps sync [--to lock\|source\|build\|active]` | Resolve, fetch, build, and activate by stage |
| `clpm deps update [sys...]` | Update dependencies |
| `clpm deps search <query> [--limit N] [--json]` | Search configured registries |
| `clpm deps info <system> [--json] [--all]` | Show system details |
| `clpm [-p <member>] deps tree [--depth N]` | Show dependency tree |
| `clpm [-p <member>] deps why <system>` | Explain why a system is included |
| `clpm deps audit [--json]` | Provenance report |
| `clpm deps sbom --format <cyclonedx-json\|cyclonedx-xml\|spdx-json> [--out <path>]` | SBOM export |
| `clpm registry <add\|list\|update\|trust\|init\|key\|publish> ...` | Manage registries, keys, trust, and publishing |
| `clpm run [-- <args...>]` | Run the project entrypoint |
| `clpm run exec -- <cmd...>` | Run a command in the project env |
| `clpm run test` | Run project tests |
| `clpm run script <name> [-- <args...>]` | Run a project script |
| `clpm run scripts` | List project scripts |
| `clpm repl <daemon\|eval\|call> ...` | Persistent project REPL/debug protocol |
| `clpm store clean [--dist] [--store]` | Remove project-local outputs |
| `clpm store gc [--dry-run]` | Garbage collect store |

### Top-Level Options

- `-v, --verbose` - Verbose output
- `-h, --help` - Show root help
- `--version` - Show the CLPM version

### Command-Scoped Options

Scoped options must appear before the command token. Command-local options
remain after the command and are documented in `clpm help <command>`.

- `-p, --package <member>` - Workspace member target before project-scoped commands from a workspace root
- `-j, --jobs N` - Parallel dependency realization for `deps sync` beyond the lock stage
- `--lisp <impl>` - Lisp implementation (`sbcl|ccl|ecl`) for build/package/run commands
- `--offline` - Use cached artifacts for `deps sync` realization and `deps sbom`
- `--insecure` - Skip signature verification on verifier-bearing registry/dependency commands
- `--fetch-retries N` - Retry budget for CLPM-managed fetches (default: 3, env: `CLPM_FETCH_RETRIES`)
- `--fetch-timeout SECS` - Per-request CLPM fetch timeout (default: 60, env: `CLPM_FETCH_TIMEOUT`)
- `--with-optional <sys>` - Opt in to an optional dependency for `deps sync/update` (repeatable)
- `--with-all-optional` - Opt in to every optional dependency for `deps sync/update`

## Registry Format

CLPM uses Git-based registries with signed snapshots:

```
registry/
  snapshot.sxp      ; Current snapshot
  snapshot.sig      ; Ed25519 signature
  keys/
    <key-id>.pub    ; Public keys
  packages/
    <name>/
      <version>/
        release.sxp ; Release metadata
        release.sig ; Signature
```

### Creating a New Registry

```bash
# Generate an Ed25519 keypair (writes <id>.key and <id>.pub)
clpm registry key generate --out ./keys --id mykey

# Initialize a registry directory with an empty signed snapshot
clpm registry init --dir ./my-registry --key-id mykey --keys-dir ./keys

# Optional: create a git repository (CLPM does not run VCS commands)
jj git init ./my-registry
```

### Publishing a project (end-to-end example)

This is a fully local example you can run on one machine using a `file://` tarball URL.

```bash
# 1) Create a local registry
mkdir -p /tmp/clpm-demo
clpm registry key generate --out /tmp/clpm-demo/keys --id demo
clpm registry init --dir /tmp/clpm-demo/registry --key-id demo --keys-dir /tmp/clpm-demo/keys
jj git init /tmp/clpm-demo/registry

# 2) Create a project to publish
clpm project new demo-lib --lib --dir /tmp/clpm-demo

# 3) Publish it (writes release metadata + signatures into the registry)
clpm registry publish \
  --registry /tmp/clpm-demo/registry \
  --key-id demo \
  --keys-dir /tmp/clpm-demo/keys \
  --project /tmp/clpm-demo/demo-lib \
  --tarball-out /tmp/clpm-demo/tarballs/ \
  --tarball-url file:///tmp/clpm-demo/tarballs/demo-lib-0.1.0.tar.gz
```

## Examples

- `example/quicklisp-app/` — online Quicklisp workflow (network required)
- `example/workspace/` — local workspace workflow (no network required)

### Key and Signature Encodings

- `registry/keys/<key-id>.pub`: ASCII hex encoding of the 32-byte Ed25519 public key (64 hex chars) with an optional trailing newline.
- `registry/snapshot.sig` and `registry/packages/.../release.sig`: detached signature over the corresponding `.sxp` file, encoded as either:
  - ASCII base64 of the 64-byte signature (padding/newlines allowed), or
  - ASCII hex of the 64-byte signature (128 hex chars).

### Snapshot Format

```lisp
(:snapshot
  :format 1
  :generated-at "2024-01-15T10:30:00Z"
  :releases ("alexandria@1.4.0" "bordeaux-threads@0.8.8" ...)
  :provides (("alexandria" . "alexandria@1.4.0")
             ("bordeaux-threads" . "bordeaux-threads@0.8.8")
             ...))
```

### Release Format

```lisp
(:release
  :format 1
  :name "alexandria"
  :version "1.4.0"
  :source (:tarball :url "https://..." :sha256 "...")
  :artifact-sha256 "..."
  :systems ("alexandria")
  :system-deps (("alexandria" (("trivial-features" nil))))
  :license "Public Domain"
  :description "General utilities library")
```

## Directory Layout

```
~/.local/share/clpm/     ; Data directory
  registries/            ; Cloned registries
~/.cache/clpm/           ; Cache directory
  store/
    sources/sha256/      ; Extracted source trees
    artifacts/sha256/    ; Downloaded archives
    builds/<build-id>/   ; Compiled fasls
  logs/                  ; Build logs
~/.config/clpm/          ; Config directory
  keys/                  ; Trusted public keys
```

## Security Model

CLPM provides:

- **Integrity**: Artifacts verified by SHA-256 hash
- **Registry authenticity**: Snapshots verified by Ed25519 signature
- **Deterministic resolution**: Same inputs = same dependency graph

CLPM does **not** guarantee that building packages is safe. Loading CL code executes arbitrary code. Consider:

- Running builds in OS sandbox (bubblewrap, sandbox-exec)
- Disabling network during builds
- Running under low-privilege user

## Building from Source

Requirements:
- SBCL 2.0.0+
- ASDF 3.3+

```bash
# Load and build
sbcl --eval '(require :asdf)'
     --eval '(push #P"./" asdf:*central-registry*)'
     --eval '(asdf:load-system :clpm)'
     --eval '(clpm:build-executable "clpm")'
```

## Testing

```bash
sh test/all-tests.sh
```

## License

MIT License

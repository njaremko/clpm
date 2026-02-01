# CLPM - Common Lisp Package Manager

A technically excellent package manager for SBCL with proper dependency resolution, cryptographic integrity verification, and reproducible builds.

## Features

- **Deterministic dependency resolution** with PubGrub-style conflict explanations
- **Content-addressed store** for sources and build artifacts
- **Cryptographic integrity** - all artifacts verified by SHA-256
- **Registry authentication** via Ed25519 signatures
- **SBCL-keyed build cache** - build outputs properly keyed by SBCL version, platform, and compile settings
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

# Configure a registry (example values)
clpm registry add --name main --url https://github.com/clpm/clpm-registry.git --trust ed25519:...

# Create a new project
clpm new myproject --bin
cd myproject

# Add dependencies (updates clpm.project and clpm.lock)
clpm add alexandria@^1.4.0

# Install + activate, then run commands
clpm install
clpm test
clpm run

# Produce a distributable executable in dist/
clpm package
```

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

## Commands

| Command | Description |
|---------|-------------|
| `clpm help <cmd>` | Show command-specific help |
| `clpm doctor` | Check environment and configuration |
| `clpm new <name> --bin\|--lib [--dir <path>]` | Create a new project scaffold |
| `clpm init [name]` | Initialize new project |
| `clpm add <dep>[@<constraint>]` | Add a dependency |
| `clpm remove <dep>` | Remove a dependency |
| `clpm registry <add\|list\|update> ...` | Manage global registries |
| `clpm resolve` | Resolve dependencies and write lockfile |
| `clpm fetch` | Download dependencies |
| `clpm build` | Build dependencies |
| `clpm install` | Resolve, fetch, and build |
| `clpm update [sys...]` | Update dependencies |
| `clpm repl` | Start SBCL with project loaded |
| `clpm run [-- <args...>]` | Run the project entrypoint |
| `clpm exec -- <cmd...>` | Run a command in the project env |
| `clpm test` | Run project tests |
| `clpm package` | Build a distributable executable |
| `clpm clean [--dist]` | Remove project-local outputs |
| `clpm gc` | Garbage collect store |

### Global Options

- `-v, --verbose` - Verbose output
- `-j, --jobs N` - Parallel build jobs
- `--offline` - Fail if artifacts not cached
- `--insecure` - Skip signature verification

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

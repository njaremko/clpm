# Repository Guidelines

## Project Structure & Module Organization

- `clpm.asd` defines the ASDF system and load order for `src/`.
- `src/` holds the implementation:
  - `src/main.lisp` is the CLI entry point and argument parsing.
  - `src/commands.lisp` implements command handlers (`clpm install`, `clpm repl`, etc.).
  - `src/io/`, `src/crypto/`, `src/solver/`, `src/registry/`, `src/build/` are focused subsystems.
- `test/` contains lightweight SBCL script tests (no separate test system yet).
- `example/` contains an example `clpm.project`.
- `clpm-bootstrap.lisp` is the bootstrap installer (SBCL-only; no Quicklisp).

## Build, Test, and Development Commands

- Requirements: SBCL 2.0.0+ and ASDF 3.3+.
- `sbcl --script clpm-bootstrap.lisp install-local .` — build and install `clpm` into `~/.local/bin/`.
- `clpm --version` — verify the installed binary is on your `PATH`.
- `sbcl --script test/basic-test.lisp` — run smoke tests; exits non-zero on failure.
- For a standalone build command, see `README.md` (“Building from Source”).

## Coding Style & Naming Conventions

- Indentation: 2 spaces; keep `;;;` section headers and docstrings consistent with existing files.
- Naming: functions/locals use kebab-case (`parse-args`), specials use earmuffs (`*verbose*`).
- Packages: add or adjust exports in `src/packages.lisp`; keep public APIs namespaced (`clpm.<area>`).
- Prefer small, composable functions and keep side effects at command/IO boundaries.

## Testing Guidelines

- Keep tests fast and deterministic; prefer unit tests under `test/`.
- Name test files `test/<topic>-test.lisp` and ensure failures terminate with `(sb-ext:exit :code 1)`.

## Commit & Pull Request Guidelines

- This workspace does not include VCS metadata (`.git`/`.jj`), so default to Conventional Commits:
  - Examples: `feat: add lockfile writer`, `fix: handle missing tar`, `test: cover semver edge cases`.
- When version control is initialized, use `jj` (not `git`) and keep changesets small and well-described.
- PRs: include a clear summary, how you tested (commands + output), and note any security/reproducibility impact.

## Security & Configuration Tips

- Avoid weakening integrity guarantees by default; `--insecure` is for debugging only.
- `CLPM_HOME` overrides the default data directory; keep new paths aligned with XDG-style conventions.

## Agent Skill Requirement

- When working on this repo, load and follow: `/Users/njaremko/.codex/skills/common-lisp-excellence/SKILL.md`
- Prefer the S-expression tools in `/Users/njaremko/.codex/skills/common-lisp-excellence/scripts/` for structured edits of data-only `.sxp` / `clpm.project` / `clpm.lock`-style files.

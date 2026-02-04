# Releasing CLPM

This repo is designed to bootstrap from SBCL + ASDF (no Quicklisp required to build CLPM itself).

## Prereqs

- SBCL 2.0.0+
- ASDF 3.3+

## Checklist

1) Update version

- [ ] Update `*version*` in `src/main.lisp`
- [ ] Run `clpm --version` after building to confirm it reports the new version

2) Run tests

- [ ] `sh test/all-tests.sh`
- [ ] Optional live Quicklisp smoke:
  - [ ] `CLPM_LIVE_QUICKLISP=1 sbcl --script test/manual/quicklisp-live-workflow.lisp`

3) Build and install locally

- [ ] `sbcl --script clpm-bootstrap.lisp install-local .`
- [ ] Ensure the installed `clpm` is on `PATH` (default: `~/.local/bin/`)
- [ ] Sanity-check:
  - [ ] `clpm doctor`
  - [ ] `clpm --help`

4) Release artifacts

- [ ] Build release binary using the “Building from Source” instructions in `README.md`
- [ ] Upload binaries + bootstrap script to the release location

5) Record the release in VCS (jj)

- [ ] `jj status` is clean
- [ ] `jj commit -m "release: vX.Y.Z"`


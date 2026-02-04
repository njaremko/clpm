# Quicklisp-backed example project

This example is meant to exercise CLPM against the real Quicklisp dist
(network required).

It intentionally keeps the `.asd` minimal and relies on CLPM to preload
dependencies from `clpm.project` when running `clpm run`, `clpm test`, and
`clpm package`.

## Try it

From the repo root:

```bash
cd example/quicklisp-app

# If you don't already have Quicklisp configured in CLPM:
clpm registry add --quicklisp --name quicklisp --url https://beta.quicklisp.org/dist/quicklisp.txt
clpm registry trust set quicklisp tofu
clpm registry update quicklisp

# Install deps from Quicklisp and activate the project
clpm install

# Run tests
clpm test

# Run the entrypoint
clpm run

# Build a distributable executable in dist/
clpm package
./dist/quicklisp-app
```

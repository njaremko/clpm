# CLPM Example Workspace

This is a small, fully local workspace intended to exercise CLPM end-to-end
without requiring any network access.

It contains:

- `lib-b/` - a tiny library
- `lib-a/` - a library that depends on `lib-b` via `(:path "../lib-b")`
- `app/` - an executable that depends on `lib-a` via `(:path "../lib-a")`

## Try it

From the repo root:

```bash
cd example/workspace/app

# Install dependencies and activate the project
clpm install

# Run tests
clpm test

# Run the entrypoint
clpm run -- hello world

# Build a distributable executable in dist/
clpm package
./dist/app hello world

# Clean project-local outputs
clpm clean --dist
```


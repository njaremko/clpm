# SexprEdit Issue Tracker

This tracker turns the Lisp structural-editing proposal into implementable work for CLPM's persistent REPL image. It is intentionally written as a living issue tracker rather than a speculative design note: every ticket names the user outcome, semantic contract, implementation boundary, and evidence required before the ticket can be closed.

## Problem Statement

LLM edits to Common Lisp source are currently expressed as text patches, but the meaningful unit of a Lisp program is not a line. It is a reader-controlled stream of forms whose behavior depends on packages, lexical and dynamic bindings, macros, CLOS, ASDF loading, and the live image. A useful replacement for a standard edit tool must let the agent ask for semantic source transformations while CLPM performs the exact, source-preserving file surgery and validation.

The unmet objective is: an agent should be able to inspect, transform, validate, and explain Common Lisp source through stable form identities and REPL-backed semantic observations, without guessing about parentheses, packages, macro bodies, or image freshness.

## Semantic Core

SexprEdit denotes a project as a set of source documents plus an optional live-image observation context. A source document denotes an ordered stream of concrete forms and trivia under a reader context. A form address denotes a stable source extent together with its parent relation, local child role, reader context, and printed surface text. An edit denotes a partial function from one project source state to another, with structured diagnostics when the function is undefined.

Two source states are observationally equal for a ticket when every public observation required by that ticket agrees: top-level form order, form surface text, reader success or failure, package context, source extents, structural diff, validation outcome, and any explicitly requested REPL/image facts. Formatting and comments are not incidental representation for this tool; they are observations unless a ticket explicitly says otherwise.

The first law is address soundness:

```lisp
show-form(index-file(file), path) = source-substring(file, extent(path))
```

The second law is edit locality:

```lisp
forall untouched-path.
  show-form(apply-edit(project, edit), untouched-path)
  = show-form(project, untouched-path)
```

The third law is validation transparency:

```lisp
validate-edit(apply-edit(project, edit), steps)
= ordered diagnostics produced by reading, compiling, loading, or testing that edited state
```

No implementation may satisfy these laws by reformatting whole files, hiding reader errors, silently changing packages, or confusing source definitions with image-only definitions.

## Status Legend

- `open`: Not implemented.
- `in-progress`: There is active code or tests, but acceptance evidence is incomplete.
- `done`: The implementation exists, tests cover the ticket's acceptance criteria, and CLPM validation has passed.

## Milestone 1: Structural Source Core

### SE-001: Reader-Preserving Source Index

Status: in-progress

Build a source indexer that reads a Lisp file as a concrete source document, recording each top-level form's ordinal, byte/character extent, line/column start, package context, first operator, definition name when applicable, and original surface text. The indexer must bind `*read-eval*` to `nil`; the tool is not allowed to execute read-time code while inspecting source.

The index is the foundation for every later ticket. It must distinguish "no form here", "reader error before a complete form", and "complete form with trailing trivia" because those are different editing outcomes. Reader conditionals may be represented conservatively in the first version, but active branches and inactive branches must not be silently collapsed.

Acceptance evidence:

- A test indexes files containing `defpackage`, `in-package`, `defun`, comments, blank lines, quoted forms, pathnames, characters, and reader conditionals.
- The returned form text exactly matches the original substring for every indexed top-level form.
- Reader errors produce structured diagnostics with file, line, column, and phase.

### SE-002: Stable Address Model

Status: in-progress

Define the address language used by every inspection and edit operation. The first version must support file plus top-level ordinal, file plus top-level definition selector, and child paths through ordinary list structure. Addresses must be serializable as JSON because they will travel through `clpm repl call`.

The model should prefer semantic selectors over fragile ordinals when a stable name exists, while still exposing ordinals for anonymous top-level forms. Ambiguous selectors must return candidates instead of picking one arbitrarily.

Acceptance evidence:

- A test resolves `{"file":"x.lisp","top_level":2}`.
- A test resolves `{"file":"x.lisp","defun":"example"}`.
- A test for duplicate definitions returns an ambiguity object with candidate paths and snippets.
- The address resolver never reads outside the requested project root.

### SE-003: Focused Form Lens

Status: in-progress

Expose `show-form` and `list-top-level-forms` as CLPM REPL methods. `list-top-level-forms` should summarize the file without dumping it. `show-form` should return exactly the selected surface form plus structural metadata: kind, name, extent, package, children count, and path.

The form lens is the replacement for opening a whole file in the agent context. It should give enough information to make a small edit while keeping unrelated source out of view.

Acceptance evidence:

- `clpm repl call sexpr-list-top-level-forms --file src/foo.lisp` returns ordered summaries.
- `clpm repl call sexpr-show-form --path '{...}'` returns one form and metadata.
- Tests prove comments before and after neighboring forms are not included in the form body unless they are inside the selected extent.

### SE-004: Structural Edit Primitives

Status: in-progress

Implement the minimal edit algebra: replace a form, insert before a form, insert after a form, delete a form, wrap a form using `%` as the old-form placeholder, and splice a form with multiple forms. Each operation must act on form extents, not line ranges.

The edit operation is undefined when the path is ambiguous, the replacement is not readable as exactly the required number of forms, the target no longer exists, or the edit would produce an unreadable file. Undefined edits must return diagnostics; they must not partially modify the file.

Acceptance evidence:

- Tests cover every primitive against source with comments and blank lines.
- Invalid replacement text leaves the file byte-for-byte unchanged.
- `%` substitution in wrap is parsed as syntax and replaces exactly one placeholder.

### SE-005: Transactional Edit Session

Status: in-progress

Add `sexpr-plan-edit`, `sexpr-apply-edit`, `sexpr-rollback-edit`, and `sexpr-commit-edit` semantics. A transaction records the pre-edit file text, requested operations, changed extents, and validation results. Rollback must restore the original text exactly.

The first implementation may use single-shot transactions rather than a long-lived session, but the protocol must already expose transaction identity and edit summary so later multi-step sessions fit without a breaking change.

Acceptance evidence:

- A failing validation rolls back the file.
- A successful transaction reports files touched, forms changed, new reader diagnostics, and changed top-level summaries.
- Concurrent edits detect stale preconditions rather than overwriting a newer file.

### SE-006: Form-Aware Structural Diff

Status: in-progress

Return a structural diff for every transaction. The initial diff must identify changed top-level forms, inserted forms, deleted forms, and changed calls where the operator is the same but arguments differ. It should include a compact human diff as secondary output, but the machine-readable structural diff is the primary artifact.

Acceptance evidence:

- Replacing `(foo x)` with `(foo x y)` reports a changed call with added argument `y`.
- Inserting a new `defun` reports an inserted top-level definition.
- Deleting a body form reports the enclosing definition and deleted form path.

## Milestone 2: Semantic Inspection

### SE-007: Macroexpand At Path

Status: in-progress

Add `sexpr-macroexpand-at` and `sexpr-macroexpand-1-at` methods. The tool should read the selected source form in the correct package, expand it in the daemon image, and return the expansion, whether expansion occurred, source path, package, and any condition diagnostics.

This ticket deliberately depends on the live image. If the defining macro is not loaded, the response should say so through normal condition data rather than pretending macroexpansion is a static source operation.

Acceptance evidence:

- A test macro expands correctly after its file is loaded into the daemon.
- A missing macro returns a structured condition.
- `recursive: true` and one-step expansion produce observably different results for a nested macro case.

### SE-008: Bindings And Scope Explanation

Status: in-progress

Implement a conservative `sexpr-bindings-at` / `sexpr-explain-scope` query for ordinary lexical forms: lambda lists, `let`, `let*`, `flet`, `labels`, `macrolet`, and `symbol-macrolet`. The result should distinguish lexical variables, dynamic declarations when visible, local functions, local macros, and symbol macros.

The first version may be syntactic and conservative. When a macro body cannot be understood without an editing contract, the tool must report uncertainty instead of inventing bindings.

Acceptance evidence:

- Tests cover nested lexical variables and shadowing.
- Tests distinguish a symbol macro from an ordinary lexical variable.
- Moving a form out of a binding context can request a warning based on lost visible bindings.

### SE-009: Symbol Facts And Scope-Aware Rename

Status: in-progress

Expose `sexpr-symbol-info` and `sexpr-rename-symbol`. Symbol facts combine package lookup, definitions, xref data, export status, and known namespace kind. Rename must support at least function namespace references within one package and must detect shadowing hazards before editing.

Common Lisp has multiple namespaces. The protocol must make the namespace explicit; a rename of a function named `FOO` must not silently rename a class, slot, lexical variable, or package with the same printed name.

Acceptance evidence:

- Symbol info returns definitions and references for a loaded function.
- Rename updates a function definition and its direct call sites inside the selected package.
- Rename refuses to proceed when a local binding with the same printed name would make the transformation ambiguous.

### SE-010: Source And Expansion Dual View

Status: in-progress

Relate source paths to macroexpanded forms. `sexpr-expansion-of` should return an expansion tree with best-effort origin metadata. `sexpr-source-origin` should map an expansion node back to the closest source path when that mapping is known.

This is a hard feature, so the first version can use coarse origin attribution: all generated expansion nodes may point to the macro call, while body forms preserve their specific child origins when the macro contract identifies a body.

Acceptance evidence:

- A binding macro expansion identifies the macro call as generated context.
- Body subforms retain source-origin links when the contract marks a body position.
- Unknown origins are represented explicitly.

### SE-011: Package Diagnostics And Defpackage Updates

Status: in-progress

Add package-aware inspections and edits: exported status, missing imports, accidental internal-symbol references, package conflicts, and `update-defpackage` for export/import/shadowing-import operations. A public API addition should be able to update the owning `defpackage` form without manual text editing.

Acceptance evidence:

- Adding an export preserves existing `defpackage` formatting as much as possible.
- Duplicate exports are detected and not inserted twice.
- A package conflict diagnostic includes both packages and symbols involved.

### SE-012: CLOS And Generic Function Awareness

Status: in-progress

Expose class and generic function facts: class layout, direct superclasses, slots, readers/writers/accessors/initargs, generic function lambda list, methods, qualifiers, and specializers. Provide high-level `add-slot`, `rename-slot`, and `add-method` edit operations.

Acceptance evidence:

- `sexpr-class-info` returns slot metadata for a loaded class.
- `sexpr-generic-info` returns methods and specializers for a loaded generic function.
- `add-method` inserts a correctly shaped `defmethod` in the requested file and validates it.

### SE-013: Definition And Call Graph

Status: in-progress

Build project-level definition and call graph queries that combine source indexing with available SBCL introspection. The result must be honest about certainty: certain source calls, possible calls through generic functions, and dynamic calls through `funcall`, `apply`, or `symbol-function` must be different result categories.

Acceptance evidence:

- A direct call appears in `certain_calls`.
- A generic function call is identified as dispatch-sensitive.
- A `funcall` through a variable is reported as dynamic rather than guessed.

## Milestone 3: Refactoring Algebra

### SE-014: Extract Function

Status: open

Implement `sexpr-extract-function` for a selected body form or contiguous body range. The tool must compute free lexical variables from the syntactic scope model, create a lambda list, insert a new `defun`, and replace the original form with a call. If free variable analysis is uncertain, the operation must stop with a diagnostic.

Acceptance evidence:

- A selected expression with two free variables becomes a new function with two parameters.
- The original function calls the extracted function exactly once.
- The transformed file reads and compiles.

### SE-015: Introduce Let And Bind Repeated Expression

Status: in-progress

Implement `sexpr-introduce-let` and `sexpr-bind-repeated-expression`. These edits should create lexical names without changing evaluation count or declaration scope. Repeated-expression binding must be disabled when the expression may have side effects unless the caller explicitly accepts the risk.

Acceptance evidence:

- Introducing a let around one expression preserves body order.
- Repeated pure expressions are replaced by one binding and multiple symbol uses.
- An effectful expression is rejected by default.

### SE-016: Lambda List And Call-Site Refactors

Status: open

Support `change-lambda-list`, `add-keyword-arg`, and `convert-to-keyword-argument` for functions. The tool must update compatible call sites and return broken call-site diagnostics for the rest.

Acceptance evidence:

- Adding an optional argument leaves old call sites valid.
- Converting a positional argument to a keyword updates direct calls.
- Calls through `apply` are reported as dynamic caveats.

### SE-017: Structural Movement Operations

Status: open

Expose familiar structural editing operations: slurp, barf, raise, splice, transpose, kill form, copy form, and move form. These operations must work on the CST and preserve surrounding trivia.

Acceptance evidence:

- Splicing a `progn` in body position inserts its children at the same body level.
- Raising a form replaces its parent without corrupting parentheses.
- Transposing sibling forms updates only their extents.

### SE-018: Rewrite Rules And Structural Search

Status: in-progress

Provide structural search patterns and named rewrite rules. Patterns should support single-form variables, many-form variables, symbol constraints, call constraints, binding constraints, and not-within constraints. Rewrite dry runs must show every planned change before applying.

Acceptance evidence:

- Searching for `(setf (cell-value ?cell) ?value)` returns path-bound matches.
- `??body` matches a variable-length body.
- A rewrite rule dry run reports affected forms without modifying files.

### SE-019: Semantic-Preserving Rewrite Classification

Status: in-progress

Classify common rewrites as safe, unsafe, or unknown under explicit reasons. The classifier should understand simple body-position `progn` splicing, single-use let inlining, duplicate-evaluation risk, declaration scope, and special binding hazards.

Acceptance evidence:

- Body-position `progn` splicing is marked safe.
- Inlining an expression used twice is marked unsafe when it may duplicate effects.
- Rewrites crossing a `declare (special ...)` boundary report a declaration-scope hazard.

## Milestone 4: Validation And Repair

### SE-020: Read, Compile, Load, Test Validation Pipeline

Status: in-progress

Implement `sexpr-validate-edit` with ordered steps: read, compile affected file, load affected file or system, run focused tests, run system tests, and macroexpand-check. Each step returns structured diagnostics tied back to source paths when possible.

Acceptance evidence:

- A malformed edit fails in the read phase.
- An undefined function after an edit fails in compile/load with symbol and source metadata when available.
- A passing validation reports every executed step.

### SE-021: Condition And Restart-Aware Repair Suggestions

Status: open

Convert Common Lisp conditions into edit-oriented repair candidates. For undefined variables, undefined functions, package errors, and argument count mismatches, return likely semantic fixes with enough context for an agent to choose safely.

Acceptance evidence:

- An unbound variable diagnostic lists visible bindings and candidate parameter/binding repairs.
- An undefined function diagnostic offers define, import, or rename candidates when evidence exists.
- Available restarts are included without auto-invoking them.

### SE-022: Lisp-Aware Linting

Status: in-progress

Add a lint layer for Lisp-specific hazards: undefined functions, undefined variables, unused lexical variables, accidental specials, missing or stale exports, duplicate methods, unreachable reader branches, suspicious quoted package symbols, `eq` on numbers or strings, mutation of quoted constants, invalid generalized places, misplaced declarations, macrolet capture risk, package locks, and `eval` where macroexpansion would suffice.

Acceptance evidence:

- Each lint has a stable machine-readable kind.
- Lints include source path and suggested next inspection or edit.
- False-positive-prone lints can report uncertainty rather than hard failure.

### SE-023: Source/Image Synchronization

Status: in-progress

Expose source-vs-image synchronization checks: source newer than image, image-only definitions, source-only definitions, and stale ASDF components. The tool should integrate with existing CLPM `load-system`, `compile-file`, and redefinition logs.

Acceptance evidence:

- Editing a loaded function makes it appear as source-newer-than-image until reloaded.
- An eval-defined function appears as image-only.
- `compare-image-to-source` returns a concise summary suitable for pre-final checks.

### SE-024: ASDF System Graph And Affected Files

Status: in-progress

Return ASDF system graphs with components, dependencies, source directories, and affected files for a source path. Edits to package files should know which later files must be recompiled or reloaded.

Acceptance evidence:

- `sexpr-system-graph` returns component order for `clpm`.
- `sexpr-affected-files` for an early serial component includes later serial components.
- Missing systems return structured diagnostics.

## Milestone 5: Project-Specific Semantics

### SE-025: Macro Editing Contracts

Status: open

Let projects define editing contracts for macros: binding macro, definition macro, iteration macro, body position, introduced bindings, name position, lambda-list position, and safety notes. Contracts must be ordinary data, not executable source transformations.

Acceptance evidence:

- A contract for a binding macro makes `bindings-at` aware of introduced names.
- A contract for a definition macro makes the top-level index classify it as a definition.
- Malformed contracts produce validation diagnostics.

### SE-026: Macro Shape Inference

Status: in-progress

Infer likely macro contracts from `defmacro` lambda lists and names when no explicit contract exists. The result must include confidence and uncertainty, never a false claim of certainty.

Acceptance evidence:

- `with-foo` style macros infer a body and likely introduced binding.
- `define-thing` style macros infer a definition name position.
- Low-confidence inference is exposed as uncertain.

### SE-027: Effect Summaries

Status: in-progress

Provide approximate effect summaries for forms: reads, writes, allocation, unknown calls, possible signals, and mutation of generalized places. Effects are conservative observations used to guard rewrites, not a proof system.

Acceptance evidence:

- `(setf (slot-value x 'y) z)` reports a write.
- Calls to unknown functions mark `calls_unknown`.
- Pure constants and variable references produce minimal effects.

### SE-028: Test Generation From Forms And REPL Examples

Status: open

Generate test scaffolding from selected definitions and captured REPL examples. The tool should inspect existing test style where possible and create a focused test with clear setup, action, and assertion sections.

Acceptance evidence:

- A captured successful REPL form can be written as a deterministic smoke test.
- Generated tests use the project's existing lightweight SBCL script style unless a test framework is detected.
- The generated test is validated by running it.

### SE-029: Form Provenance

Status: in-progress

Record per-transaction provenance: created by agent, timestamp, reason, operations, validation steps, and changed form identities. Provenance should live in the edit session metadata, not as noisy source comments by default.

Acceptance evidence:

- Every committed SexprEdit transaction returns provenance.
- Provenance survives through structural diff and validation output.
- No provenance comment is inserted into source unless explicitly requested.

## Milestone 6: Planning And Synthesis

### SE-030: Dry-Run Edit Plans

Status: in-progress

Support named edit plans that can be explained before application. A plan should report files touched, symbols changed, package updates, possible captures, validation steps, and rollback strategy.

Acceptance evidence:

- A rename plan explains all affected files before editing.
- A package export plan shows the exact `defpackage` target.
- Applying a plan produces the same operation list the explanation described.

### SE-031: Constraint-Based Edit Candidates

Status: open

Implement a candidate generator for constrained edit goals. The first version should handle narrow, typed goals such as adding an optional argument, making a function public, or wrapping a body with a known macro. It must return alternatives with explicit tradeoffs instead of directly mutating files.

Acceptance evidence:

- A goal to add an optional argument returns optional and keyword alternatives when both are valid.
- Constraints such as preserve existing call sites affect candidate ranking.
- Candidate application goes through the normal edit transaction path.

### SE-032: Ambiguity Management

Status: open

Every ambiguous selector or edit request must return a structured ambiguity response with candidates, snippets, and refinement keys. The agent should never have to infer ambiguity from a surprising edit.

Acceptance evidence:

- Duplicate function names in different packages return package refinement options.
- Multiple call sites return caller and argument-shape refinements.
- An ambiguous edit leaves files unchanged.

## Milestone 7: Agent Tooling Integration

### SE-033: CLPM REPL RPC Surface

Status: in-progress

Expose the implemented SexprEdit operations through CLPM's self-describing REPL method registry. Method docs must be good enough that `clpm repl call methods` and `clpm repl call help --method ...` teach an agent how to call each operation.

Acceptance evidence:

- Every SexprEdit method appears in `methods`.
- Every method has parameter schema, summary, and long doc.
- Reserved transport parameters remain rejected.

### SE-034: Agent Workflow Documentation

Status: open

Update CLPM's agent skill text and help output to explain the structural-edit workflow: inspect relevant forms, inspect bindings/macroexpansion before movement, apply structural transaction, validate, then check image/source synchronization.

Acceptance evidence:

- `clpm skill` mentions SexprEdit once the MVP methods exist.
- The workflow tells agents when text patches are still acceptable.
- The docs warn that source editing does not automatically update the live image unless validation loads it.

### SE-035: CLI Convenience Commands

Status: open

Add thin CLI conveniences for common operations without bypassing the REPL RPC protocol. Examples: `clpm repl sexpr list`, `clpm repl sexpr show`, `clpm repl sexpr replace`, or a similar command shape that fits CLPM's existing CLI style.

Acceptance evidence:

- CLI commands call the same RPC methods as agents.
- JSON output is available for machine clients.
- Human output is concise and avoids whole-file dumps.

## Completion Rule

This tracker is complete only when every ticket is `done`, the CLPM test suite passes, `clpm skill` describes the resulting workflow, and a final audit proves that structural edits can replace standard text editing for Common Lisp source in the supported scope. A ticket may be split if implementation reveals a simpler semantic boundary, but no split may drop a requirement; the replacement tickets must preserve the original acceptance evidence.

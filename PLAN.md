# Lean Native Parser Plan

## Objectives
- Deliver a Lean-native argument parser that consumes argv left-to-right and produces a single user-specified value via `Applicative`/`Alternative` composition.
- Prefer abstractions with straightforward proofs of progress, determinism, and error soundness, even if that means departing from previous implementations.
- Embrace “last value wins” semantics for options/flags and document any intentional differences from the legacy parser.
- Maintain an explicit activity log, including failed experiments and reverted approaches, so future agents can learn from them.

## Current Snapshot
- Normalised argv-to-token pass (`ParsedToken`) now splits argv into option tokens and positional strings after the first positional/`--`, simplifying downstream consumers.
- Interpreter now consumes a cursor backed by two arrays (`options`, `positionals`); the legacy `TokenStream` helpers were deleted after porting positional/flag/value primitives onto the cursor.
- Regression tests cover the cursor pipeline plus short-option permutations under the new “first positional locks positional mode” semantics, but structural proofs are presently missing.

## Architectural Direction (Cursor + Field Updaters)
- **TokenCursor Core**: keep the classified argv pass yielding `TokenCursor := { options : Array ParsedOption, positionals : Array String }`, measuring progress via array sizes so helper lemmas stay arithmetic and array-native.
- **Partial-State Interpreter**: invert parsing so each primitive contributes a field updater `Partial α → Except Error (Partial α)` (or curried on tokens). Folding these updaters over the option/positional buckets hydrates a `Partial α`; overwriting fields naturally gives “last value wins”.
- **Applicative Composition**: expose user-facing grammars by composing updaters applicatively. Each primitive exports usage metadata plus its updater; `many`, `some`, `<*>`, and `<|>` lift those updaters without reimplementing cursor plumbing.
- **Progress Accounting**: attach lemmas to every updater showing that relevant tokens strictly decrease the cursor’s `remaining` measure. Positionals pop from their queue once positional mode starts, eliminating the earlier cursor gymnastics.
- **Completion Pass**: after folding all tokens, run a final completion check that turns the partial state into the target value (or structured `.missing/.invalid` errors). Structural proofs focus on the fold; semantic proofs live in completion.
- **Tests & Docs**: broaden property coverage to exercise the updater fold (repeated flags, sentinel edges, mixed permutations, partially filled states) and document the token-classification → field-updater → completion pipeline.

- ❌ Attempted to prove progress for `consumeFlagList`/`consumeOptionList` by threading explicit removal counters through the recursion. Outcome: unwieldy inductions and failing tests; reverted and recorded for posterity.
- ✅ Maintained the parsed-token classifier from earlier work; it still serves as the normalisation front-end for the upcoming cursor interpreter.
- ✅ Added the initial `TokenCursor` scaffold with array-backed storage, cursor helpers, and build coverage so the iterator rewrite has a concrete foundation.
- ✅ Replaced the `TokenStream` primitives with cursor-based versions, updated the interpreter/tests, and removed the obsolete module.
- ✅ Split classification output into option/positional arrays, rewrote `TokenCursor` to operate on those arrays directly, and refreshed tests/documentation to match the simplified semantics.
- 🔁 Next milestone: prototype the field-updater fold on top of `TokenCursor`, then rebuild progress proofs in that setting before lifting them to higher-level combinators.
- 🚧 Added a first-cut `OptionHandler`/`PositionalHandler` dispatch module that folds classified tokens into an arbitrary partial state; integration with the public interpreter (and richer partial records) is the next step.

## Workstreams
1. **Cursor & Partial Foundations**
   - Maintain the array-backed `TokenCursor` helpers already landed and introduce a reusable `Partial α` abstraction plus field-updater combinators.
   - Relate cursor measures to partial-state obligations (e.g., remaining option tokens vs. unset fields). *(cursor groundwork complete; partial abstraction pending)*
2. **Updater Fold & Completion**
   - Express primitives as field updaters, build the fold that applies them over classified tokens, and add a completion pass that finalises the partial value or reports structured errors.
   - Ensure “last value wins”/positional semantics come directly from the fold (without bespoke cursor rewrites).
3. **Proof Rehabilitation**
   - Prove per-updater progress/length lemmas, then lift them to the fold and applicative combinators.
   - Re-establish error soundness/determinism for the completed parser.
4. **Ergonomics, Testing, and Migration Docs**
   - Expand regression/property tests to cover the updater pipeline (repeated options, sentinel boundaries, mixed permutations, incomplete partial states).
   - Document the new architecture and remaining gaps so downstream users can migrate confidently.

## Immediate Next Steps
1. Prototype the field-updater fold atop `TokenCursor`, capturing any negative experiments while shaping the partial-state abstraction.
2. Reintroduce progress proofs in the updater setting and thread them through the fold/combinators.
3. Extend property tests and docs to explain the updater-based architecture, logging successes and setbacks as they occur.

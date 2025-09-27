# Lean Native Parser Plan

## Objectives
- Deliver a Lean-native argument parser that consumes argv left-to-right and produces a single user-specified value via `Applicative`/`Alternative` composition.
- Prefer abstractions with straightforward proofs of progress, determinism, and error soundness, even if that means departing from previous implementations.
- Embrace “last value wins” semantics for options/flags and document any intentional differences from the legacy parser.
- Maintain an explicit activity log, including failed experiments and reverted approaches, so future agents can learn from them.

## Current Snapshot
- Normalised argv-to-token pass (`ParsedToken`) still splits argv into option tokens and positional strings after the first positional/`--`, feeding the new cursor-based interpreter.
- Interpreter primitives now run entirely through the field-updater bundle: `HandlerBundle.apply/run` fold classified options before positionals, while `PartialSpec` finalises the partial state.
- Lean tests were pared back to focus on the public interpreter API—flag/option/positional success paths, last-value-wins semantics, and optional/default helpers—dropping the legacy `TokenCursor.consume*` assertions that no longer reflect the design.
- Primitives now hydrate `Assigned` field slots (new in `Argparse/Native/Partial.lean`), giving each handler an explicit notion of "unset" versus "last value wins" while keeping completion logic local to the finalisation pass.

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
- ✅ Integrated the handler bundle with the interpreter: `HandlerBundle.run` plus `PartialSpec` now power all native primitives, giving an explicit folding surface for upcoming proofs.
- ✅ Retired low-level cursor consumer tests and replaced them with interpreter-level guards that exercise flag, option, positional, optional, and default behaviours under last-value-wins semantics.
- ✅ Introduced the `Assigned` partial-field helper and ported flag/option/positional primitives to it, so required-versus-default distinction lives in one reusable abstraction.

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
1. Reintroduce progress proofs in the updater setting and thread them through applicative combinators, documenting any stalled approaches.
2. Extend property tests and docs to explain the updater-based architecture, including repeated-option and sentinel permutations, while recording successes and setbacks.

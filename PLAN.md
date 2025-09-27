# Lean Native Parser Plan

## Objectives
- Deliver a Lean-native argument parser that consumes argv left-to-right and produces a single user-specified value via `Applicative`/`Alternative` composition.
- Prefer abstractions with straightforward proofs of progress, determinism, and error soundness, even if that means departing from previous implementations.
- Embrace “last value wins” semantics for options/flags and document any intentional differences from the legacy parser.
- Maintain an explicit activity log, including failed experiments and reverted approaches, so future agents can learn from them.

## Current Snapshot

- ✅ Typeclass audit resolved:
  - `Grammar` now exposes `Functor`/`Applicative`/`Alternative` instances and carries `LawfulFunctor`/`LawfulApplicative` lemmas, unlocking Lean’s rewriting rules for `<$>`/`<*>` without bespoke simplifiers (fresh `Usage.empty_append`/`Usage.append_empty` helpers power the proofs).
  - Native `Result` is an `Except Error` alias, inheriting the standard `Monad`/`Applicative` stack and eliminating the custom wrapper.
  - `Assigned` gained `LawfulMonad` (and therefore `LawfulApplicative`/`LawfulFunctor`) proofs, unlocking Lean’s simplification lemmas for the partial-field helpers.
- Normalised argv-to-token pass (`ParsedToken`) still splits argv into option tokens and positional strings after the first positional/`--`, feeding the new cursor-based interpreter.
- Interpreter primitives now run entirely through the field-updater bundle: `HandlerBundle.apply/run` fold classified options before positionals, while `PartialSpec` finalises the partial state.
- Lean tests were pared back to focus on the public interpreter API—flag/option/positional success paths, last-value-wins semantics, and optional/default helpers—dropping the legacy `TokenCursor.consume*` assertions that no longer reflect the design.
- Primitives now hydrate `Assigned` field slots (new in `Argparse/Native/Partial.lean`), giving each handler an explicit notion of "unset" versus "last value wins" while keeping completion logic local to the finalisation pass.
- New directive: audit the native stack for ad-hoc functor/applicative helpers and replace them with Lean’s type class instances (e.g. provide `Functor`/`Applicative`/`Monad` instances for the new `Assigned` partial fields and remove bespoke combinators where type classes suffice); continue logging negative results when attempts stall.
- ✅ `Assigned` now carries Lean-standard `Functor`, `Applicative`, and `Monad` instances so downstream code can lean on established combinators instead of bespoke helpers.
- Standing rule: future abstractions must default to Lean’s standard type classes (Functor, Applicative, Monad, etc.) instead of hand-rolled combinators; when we implement instances the supporting definitions should remain `private` or namespace-scoped helpers whenever possible to reduce API surface.

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
- ❌ Adding an `Alternative` instance for `Interpreter` is currently blocked: the handler fold mutates state destructively, so supporting `<|>` would require a backtracking story (or fresh token folds) we do not have yet. Documented here so future work can revisit once a reversible execution model exists.
- ❌ Tried to swap the option/positional folds over to `List` to simplify length/progress proofs, but Lean rejected the refactor mid-way (parser issues around the new helper defs), so the change was reverted. Proof work will need to proceed on the existing array-backed implementation for now.
- ❌ Follow-up attempt to encode handler monotonicity via `OptionHandler.respectsPositionals` (plus helper lemmas for `flag`/`option`) ran into missing library support for `List.Forall` in this environment; reverted the changes and noted that any future proof path will either need bespoke list predicates or a different measurement strategy.
- ❌ Attempted to prove positional-length monotonicity for the option handlers by introducing `respectsPositionals` lemmas (plus `flag`/`option` handler proofs), but Lean’s `Array.extract` defaults caused the helper lemma `dropHead_size_le` to fail to elaborate; reverted the code and logged the blocker for a future array-centric approach.
- ❌ Tried refactoring the handler progress story around a list-based `AllNonexpanding` predicate (rewriting `OptionHandler` to consume `List String` queues and threading proofs through `HandlerBundle.product`), but Lean’s elaborator could not infer the required implicit parameters and the recursion on mapped handler lists became unmanageable; rolled the patch back and recorded the failure for future reference.
- ❌ Follow-up attempt to keep the existing array-based handlers while proving a `popFront` length lemma and per-handler progress theorems stalled: Lean’s array length lemmas were awkward to apply and the handler proofs introduced brittle pattern-matching obligations. Reverted the edits and noted the dead end.

## Workstreams
1. **Cursor & Partial Foundations**
   - Maintain the array-backed `TokenCursor` helpers already landed and introduce a reusable `Partial α` abstraction plus field-updater combinators.
   - Relate cursor measures to partial-state obligations (e.g., remaining option tokens vs. unset fields). *(cursor groundwork complete; partial abstraction pending)*
   - ✅ Implement Lean-standard `Functor`/`Applicative` instances for partial-field helpers so downstream code can reuse the standard combinator ecosystem instead of hand-rolled utilities.
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

1. Reintroduce updater progress/length proofs and lift them through the applicative combinators, recording any stalled approaches.
2. Extend property tests and docs to explain the updater-based architecture, covering repeated options, sentinel permutations, and partial-state completion results while logging both successful and negative findings.

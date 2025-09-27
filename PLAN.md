# Lean Native Parser Plan

## Objectives
- Deliver a Lean-native argument parser that consumes argv left-to-right and produces a single user-specified value via `Applicative`/`Alternative` composition.
- Prefer abstractions with straightforward proofs of progress, determinism, and error soundness, even if that means departing from previous implementations.
- Embrace “last value wins” semantics for options/flags and document any intentional differences from the legacy parser.
- Maintain an explicit activity log, including failed experiments and reverted approaches, so future agents can learn from them.

## Current Snapshot

- ✅ Typeclass audit resolved:
  - `Grammar` continues to expose `Functor`/`Applicative`/`Alternative` instances with accompanying lawful proofs, so `<$>`/`<*>` rewrite support comes for free.
  - Native `Result` aliases `Except Error`, inheriting Lean’s standard monad stack.
- ✅ List-based core landed: `TokenCursor` now stores plain lists, `FieldUpdater`/`Partial`/`ArgStream` have been deleted, and the interpreter runs as a simple state transformer over the classified lists.
- ✅ Parser primitives (`flag`, `option`, `positional`, `optional`, `withDefault`) now delegate directly to list-based helpers; leftover option/positional checks live in `Interpreter.evalTokens` instead of the old handler stacks.
- ✅ Option helpers now use structural recursion (`consumeFlag`/`consumeValue`), and `ParsedName` derives `BEq` so matching no longer relies on accumulator-heavy folds.
- ✅ Tests compile against the new API (classification assertions updated for lists, interpreter evaluation now accepts raw argv).
- ✅ Directive upheld: prefer Lean’s built-in type classes over bespoke combinators; all new abstractions must either reuse existing instances or provide private instance declarations that piggy-back on the standard hierarchy.
- 📓 Failure log:
  - Counter-based cursor proofs, list-rewrite attempt, and `RespectsPositionals` lemmas all stalled due to brittle arithmetic or missing standard predicates.
  - **New:** Refactoring `consumeFlag`/`consumeValue` to return removal counters (`FlagSweep`/`ValueSweep`) looked promising for progress lemmas, but the recursive proofs required heavy case-splitting on `Except` and inline-value branches; the helper types were reverted after `lake test` failures.
  - **New:** A follow-up attempt to wrap the list helpers in `FlagSweepResult`/`ValueSweepResult` (returning removal counts plus kept lists) still ballooned into unwieldy `Except` pattern matching and could not discharge the inductive cases cleanly; the spike was rolled back immediately.
  - **New:** The direct `foldlM` approach for option consumption made length inequalities opaque; we will replace it with structural recursion to avoid reasoning about auxiliary accumulators.

The codebase compiles/tests with the new state-transformer interpreter; next steps focus on rebuilding structural proofs and richer property coverage.

## Architectural Direction (List-Based State Transformer)
- **Classify Once into Lists**: `classify : List String → ClassifiedTokens` produces list buckets for options and positionals; no arrays remain in the runtime pipeline.
- **State Transformer Interpreter**: `Interpreter` is now a `TokenCursor → Except Error (α × TokenCursor)` transformer, with leftover-option/positional checks applied after successful evaluation.
- **Option Semantics**: flag/value primitives will scan the option list via structural recursion, removing matches and consuming positional spillover on demand; last-value-wins follows from taking the most recent recursive branch that sees the target option.
- **Proof Surface**: upcoming proofs target `List.length` decreases for `consumeFlag`/`consumeValue` and `takePositional?`. We will rewrite these helpers using plain structural recursion so the lemmas collapse to case analyses instead of reasoning about `foldlM` accumulators.
- **Tests & Docs**: regression tests already build against the new API; property coverage and documentation still need to be expanded to describe the state-transformer architecture and last-value-wins behaviour.

- ❌ Legacy failures (kept for posterity): counter-based cursor proofs, `RespectsPositionals`, list/array churn, `popFront` lemma attempts. These remain relevant as reminders of why we abandoned the cursor/updater stack entirely.

## Workstreams
1. **Progress & Soundness Proofs**
   - Prove length/progress lemmas for the list-based helpers (`takePositional?`, `consumeFlag`, `consumeValue`).
   - Lift those lemmas through the applicative combinators and document any obstacles.
2. **Property Tests & Regression Coverage**
   - Add randomized scenarios for repeated short/long options, sentinel boundaries, and positional overflow under last-value-wins semantics.
   - Guard against unused option/positional leftovers by testing negative cases explicitly.
3. **Documentation & Migration Notes**
   - Describe the new state-transformer architecture, noting the shift away from handler bundles.
   - Keep logging negative proof/test attempts to maintain institutional memory.

## Immediate Next Steps

1. With the structural recursion in place, prove the length/progress lemmas for `takePositional?`, `consumeFlag`, and `consumeValue`, recording both successful proofs and any new obstacles (and be ready to rethink the helper signatures if direct `Except` induction remains stubborn).
2. Expand the native test suite with property-style coverage for repeated options and positional spillover, ensuring `lake test` stays green.
3. Update docs (including this plan) as results land, highlighting lessons learned and remaining proof obligations.

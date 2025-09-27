# Lean Native Parser Plan

## Objectives
- Deliver a Lean-native argument parser that consumes argv left-to-right and produces a single user-specified value via `Applicative`/`Alternative` composition.
- Prefer abstractions with straightforward proofs of progress, determinism, and error soundness, even if that means departing from previous implementations.
- Embrace “last value wins” semantics for options/flags and document any intentional differences from the legacy parser.
- Maintain an explicit activity log, including failed experiments and reverted approaches, so future agents can learn from them.

## Current Snapshot

- ✅ Typeclass audit resolved:
  - `Grammar` now exposes `Functor`/`Applicative`/`Alternative` instances with accompanying `LawfulFunctor`/`LawfulApplicative` lemmas, so `<$>`/`<*>` rewrite support comes for free.
  - Native `Result` aliases `Except Error`, inheriting Lean’s standard monad stack.
  - `Assigned` carries `LawfulMonad` (and therefore lawful functor/applicative) proofs, allowing the partial-field helpers to use Lean’s simplification lemmas.
- ❗ The existing implementation still routes through `TokenCursor`, `HandlerBundle`, and `PartialSpec`, but we have decided to retire that stack entirely because array arithmetic and destructive folds have made progress proofs painful.
- ✅ Directive upheld: prefer Lean’s built-in type classes over bespoke combinators; all new abstractions must either reuse existing instances or provide private instance declarations that piggy-back on the standard hierarchy.
- 📓 Failure log (unchanged): the counter-based cursor proofs, list-rewrite attempt, and `RespectsPositionals` lemmas all stalled due to brittle arithmetic or missing standard predicates; keeping them documented here prevents repetition once we pivot to a new structure.

The codebase still compiles/tests, but we expect large-scale churn as we replace the cursor/updater pipeline with the upcoming list-based architecture.

## Architectural Direction (List-Based Fold)
- **Classify Once into Lists**: keep the existing `classify : List String → ClassifiedTokens` front-end but immediately convert the result to a list of disjoint option tokens and a list of trailing positionals. Lists give simple structural recursion and `List.length` arithmetic for proofs.
- **Last-Value-Wins via Right Fold**: represent the parser as a right fold over the classified option list. Each option contributes a `State` transformer that updates a partial record (`Assigned` slots) while recording usage information. Later occurrences overwrite earlier field assignments by construction.
- **Positional Queue as List**: model remaining positionals as a `List String` carried alongside the partial state. Once a positional token is seen during classification, the classifier moves everything after (and the `--` sentinel) into this list so the fold never toggles modes at runtime.
- **Applicative Grammar on Partial Builders**: expose user-facing combinators as applicative operations on partial builders. The grammar type remains, but its primitives now expand to list folds rather than cursor/handler machinery. With `Result = Except Error`, the applicative stack reuses Lean’s laws directly.
- **Proof Surface**: length/progress lemmas reduce to `List.length` under `List.tail` or `List.drop`. The fold’s structural recursion automatically gives a measure for termination. Error soundness proofs operate on the list elements without array index gymnastics.
- **Tests & Docs**: update regression/property tests to exercise the two-pass list pipeline (classification + fold). Document the new semantics, emphasising last-value-wins and the simplified proof story.

- ❌ Legacy failures (kept for posterity): counter-based cursor proofs, `RespectsPositionals`, list/array churn, `popFront` lemma attempts. These remain relevant as reminders of why we abandoned the cursor/updater stack entirely.

All remaining cursor/handler modules are marked for deletion in the next workstream; they stay only long enough to keep the tree building while the list-based replacements land.

## Workstreams
1. **List-Based Parser Core**
   - Delete `TokenCursor`, `HandlerBundle`, and related modules.
   - Introduce a `ParserState` built on plain lists (`options : List ParsedOption`, `positionals : List String`).
   - Re-implement flag/value/positional primitives as structurally recursive functions over these lists, keeping progress lemmas local to the recursion.
2. **Applicative Grammar Rebuild**
   - Re-express `Grammar` primitives in terms of the new list-based parser core.
   - Ensure `Applicative`/`Alternative` usage leans on the lawful instances already established.
   - Provide completion helpers that assemble the final value from a partial state, emitting `Except Error` results.
3. **Proof Suite**
   - Prove progress/termination lemmas for the new list folds (length strictly decreases when consuming options or positionals).
   - Re-establish error soundness and determinism theorems on top of the simplified data structures.
4. **Tests & Documentation**
   - Update regression/property tests to cover repeated options, sentinel behaviour, and positional overflow under the list-based semantics.
   - Document the architecture, highlighting the shift away from cursors/arrays and summarising past dead ends.

## Immediate Next Steps

1. Remove the cursor/field-updater modules and stand up the list-based parser skeleton (data types + basic flag/value/positional consumers) while keeping the build green.
2. Thread the new parser core through the applicative grammar, updating existing primitives and recording any negative experiments.
3. Refresh tests and documentation (including this plan) to reflect the list-based pipeline, ensuring `lake test` passes and capturing lessons learned.

# Lean Native Parser Plan

## Objectives
- Deliver a Lean-native argument parser that consumes argv left-to-right and produces a single user-specified value via `Applicative`/`Alternative` composition.
- Prefer abstractions with straightforward proofs of progress, determinism, and error soundness, even if that means departing from previous implementations.
- Embrace “last value wins” semantics for options/flags and document any intentional differences from the legacy parser.
- Maintain an explicit activity log, including failed experiments and reverted approaches, so future agents can learn from them.

## Current Snapshot
- Normalised argv-to-token pass (`ParsedToken`) already exists and preserves original spellings while respecting `--`.
- Interpreter now consumes a `TokenCursor`; the legacy `TokenStream` helpers were deleted after porting positional/flag/value primitives onto the cursor.
- Regression tests cover the cursor pipeline plus short-option permutations, but structural proofs are presently missing.

## Architectural Direction (Iterator-Centric)
- **TokenCursor Core**: represent parser state as `TokenCursor := { data : Array Token, pos : Nat }`, exposing helpers to advance the cursor and certify that `pos ≤ data.size`. Lean’s array/list iterators (`Std.Data.Array.Iterator`, `Std.Data.List.Iterator`) provide the canonical forward-only traversal model we need.
- **Parser Type**: define `Parser α := StateT TokenCursor (Except ParseError)` (or `ExceptT` over `State TokenCursor`). Applicative/Alternative instances come for free; proofs reduce to arithmetic on the cursor index.
- **Primitives**: reimplement `flag`, `value`, `positional`, etc., as cursor actions. Each primitive carries lemmas of the form `progress : cursor.pos < cursor'.pos` or explicit `consumed` counts.
- **Derived Combinators**: rebuild `many`, `some`, optional helpers, and higher-level grammar in terms of the primitives. Proofs become simple inductions that compose the primitive progress results.
- **Interpreter Fold**: parse by folding the cursor while building the destination record. “Last value wins” is just overwriting fields. A final lemma shows successful parses end with `cursor.pos = cursor.data.size` and all failures report the earliest offending index.
- **Proof Toolkit**: base proofs on `Nat` inequalities (`pos` arithmetic) instead of list surgery. Cursor lemmas (advance monotonicity, progress implies strictly smaller remaining length) give the invariants needed for combinators and interpreters.
- **Tests & Docs**: extend property tests to repeated flags, sentinel edges, and mixed permutations under iterator semantics. Update documentation to describe the new cursor-based design and its proofs.

- ❌ Attempted to prove progress for `consumeFlagList`/`consumeOptionList` by threading explicit removal counters through the recursion. Outcome: unwieldy inductions and failing tests; reverted and recorded for posterity.
- ✅ Maintained the parsed-token classifier from earlier work; it still serves as the normalisation front-end for the upcoming cursor interpreter.
- ✅ Added the initial `TokenCursor` scaffold with array-backed storage, cursor helpers, and build coverage so the iterator rewrite has a concrete foundation.
- ✅ Replaced the `TokenStream` primitives with cursor-based versions, updated the interpreter/tests, and removed the obsolete module.
- 🔁 Next milestone: restore progress proofs for the cursor primitives before expanding to higher-level combinators.

## Workstreams
1. **Cursor Foundation**
   - Implement `TokenCursor`, import the relevant iterator modules from `Std`, and supply helper lemmas (`advance`, `remaining`, arithmetic bounds).
   - Rewrite primitive consumers on top of the cursor; delete obsolete `TokenStream`/consumer code. (completed)
2. **Proof Rehabilitation**
   - Establish progress/length lemmas for each primitive.
   - Lift those lemmas through applicative/alternative combinators (`many`, `some`, `<*>`, `<|>`) and the top-level interpreter fold.
   - Prove error soundness (first failure position, leftover tokens captured by `pos < data.size`).
3. **Ergonomics & Testing**
   - Update regression & property tests to match the cursor semantics (repeated options, sentinel handling, last-value-wins cases).
   - Expose applicative helpers and ensure docs show idiomatic Lean usage.
4. **Migration Narrative**
   - Document behavioural changes versus the legacy parser (e.g., last-value-wins) and provide guidance for downstream adoption.
   - Track any remaining gaps (shell completion, usage rendering) for later phases.

## Immediate Next Steps
1. Prove the cursor progress lemmas for positional/flag/value primitives so incremental progress properties are restored quickly.
2. Refresh combinator proofs and interpreter-level invariants using the new cursor lemmas.
3. Extend tests and docs alongside each milestone, capturing both successful and negative results.

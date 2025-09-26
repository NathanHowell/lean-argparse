# Lean Native Parser Plan

## Objectives
- Reduce reliance on ad-hoc runtime state while preserving current CLI behaviour.
- Provide structures that enable proofs of correctness, termination, and documentation coherence.
- Maintain ergonomic API for downstream users, with clear migration guidance.

## Phase 1 – Foundations
1. **Introduce `ArgStream`**
   - Replace `ParseState` with an inductive stream (`front` tokens processed structurally, tail captured explicitly) that encodes the `--` sentinel split; show that option-like tokens stay in the structural `front` until the separator appears, preserving today’s positional behaviour.
   - Define companion functions `ofList`, `toList`, `remaining`, and show they form inverses where applicable, including a proof that `remaining` reconstitutes the exact CLI sequence produced by `ParseState`.
2. **Pure Interpreter Skeleton**
   - Split parser description from evaluator: `Grammar α` (metadata only) and `Interpreter.eval : Grammar α → ArgStream → Result α`.
   - Model `Result` with indexed error codes to ease proofs (`Missing`, `Invalid`, `Unexpected`), separating human-readable rendering from proof-level reasoning.
3. **Lean Typeclass Alignment**
   - Re-express parser combinators as `StateT ArgStream (Except ErrorCode)` to inherit `Monad`, `Alternative`, and applicative laws from Lean’s core.
   - Provide wrappers so existing user code (`Parser.withDefault`, `switch`, etc.) continues to type-check.
4. **Token Semantics & Errors**
   - Port the `TokenSpec` machinery (long vs. short flags, inline value parsing, descriptive names) onto the new `ArgStream`, keeping diagnostic text stable.
   - Specify lemmas linking token recognition to stream constructors so later proofs can reason about `consume*` helpers without re-opening string parsing details.

## Phase 2 – Proof-Oriented API
1. **Structural Recursion & Termination**
   - Rewrite repetition combinators (`many`, `some`, `many1`) using structural recursion on `ArgStream`, eliminating manual fuel arguments.
   - Recast flag/option consumers (`consumeFromFront`, `consumeValue`, `takePositional?`) atop the structural stream, replacing while-loop fuel with proofs of progress and exclusivity between option-like and positional tokens.
   - Prove termination and bounds lemmas (`many` consumes no more tokens than available, `some` fails exactly when `many` produces `[]`), along with preservation of the `remaining` invariant for the consumer helpers.
2. **Error Soundness Proofs**
   - Establish that `Interpreter.eval` returns `Unexpected` iff `ArgStream` is non-empty, and `Missing` only when the grammar marks an entry as required.
   - Demonstrate equivalence between runtime leftovers and `ArgStream`’s structural remainder.
3. **Documentation Coherence**
   - Index `Grammar` with its `Usage` tree (e.g., `Grammar α (usage : Usage)`) and carry a proof that combinators update usage consistently.
   - Prove `renderHelp` enumerates exactly the options/arguments accepted by `eval`, leveraging the indexed structure.

## Phase 3 – Migration & Ergonomics
1. **Compatibility Layer**
   - Provide shims mapping legacy `Parser` constructors to the new grammar/interpreter, offering deprecation warnings.
   - Document migration steps for downstream projects; add comprehensive tests covering both interfaces.
2. **Extended Test Suite**
   - Augment `Tests/Main.lean` with property-style tests (QuickCheck-like via `Std` random) to validate round-trips alongside formal proofs.
   - Integrate proof checks into CI (`lake build && lake prove`), ensuring lemmas remain valid.
3. **Documentation & Tutorials**
   - Update README examples to highlight the Lean-native semantics-first design.
   - Provide a short guide on writing proofs over parsers (e.g., verifying custom combinators, ensuring docstrings stay in sync).

## Open Questions
- How far should we push dependently typed usage indexing without sacrificing ergonomics? Investigate a lightweight `UsageWitness` record versus full dependent types.
- Determine whether shell completion generation should operate on `Grammar` or remain a separate derivation.
- Explore integrating with `Std`’s parser combinator ecosystem if/when one lands, to avoid duplication.

## Next Steps
1. Spike `ArgStream` and the pure interpreter in a scratch namespace to validate design choices.
2. Draft proofs for `remaining`/length relationships to exercise the new state representation.
3. Circulate proposal with maintainers for feedback before large-scale migration.

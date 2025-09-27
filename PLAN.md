# Lean Native Parser Plan

## Objectives
- Reduce reliance on ad-hoc runtime state while preserving current CLI behaviour.
- Provide structures that enable proofs of correctness, termination, and documentation coherence.
- Maintain ergonomic API for downstream users, with clear migration guidance.

## Progress
- Implemented `Argparse.Native.ArgStream` as the structural representation of front tokens with proofs that `remaining` matches `ParseState` semantics.
- Added `Argparse.Native.Grammar` with a pure interpreter skeleton that evaluates primitive positional arguments via `ArgStream` while maintaining usage metadata and structured error codes.
- Ported the token parsing adapters (`TokenSpec`) into `Argparse.Native.Token`, preserving the long/short option analysis and diagnostics used by `ParseState`.
- Introduced `Argparse.Native.Consumer.takePositional?` to structurally recover the next positional token while keeping option-like front tokens intact.
- Added `Argparse.Native.Consumer.consume{Flag,Value}` helpers that reuse `TokenSpec` on `ArgStream`, enabling flag/option removal without mutating `ParseState`.
- Extended `Argparse.Native.Interpreter` with flag/option primitives wired to the new consumers, plus convenience constructors for short/long variants and usage metadata.
- Added a native example in `Tests/Main.lean` that mirrors the existing CLI parser, validating the new interpreter pipeline end-to-end.
- Added regression tests covering native flag/value consumers and short/long token handling.
- Implemented `Interpreter.many`/`Interpreter.some` using structural recursion over `ArgStream`, with tests covering empty and non-empty inputs.
- Ported the remaining applicative combinators (`optional`, `choice`, `withDefault`, lazy `orElse`) onto `Interpreter`, providing `Functor`/`Applicative`/`Alternative` instances that mirror the legacy parser API.
- Expanded native tests to exercise the new combinators in conjunction with `many`/`some`, ensuring missing/invalid error propagation remains structural.
- Added property-style test generators that permute long flag/option placements (including `--` sentinels) to validate `consume*` helpers and `many`/`some` progress invariants.
- Began formalisation work with lemmas relating `ArgStream.remaining` length to its structural components and proving `Interpreter.positional` can only emit `.missing` errors.
- Strengthened the proof toolbox with `ArgStream.next?` progress and `Interpreter.positional_ok_progress`, streamlining the existing missing-error lemma to be purely `simp`-driven.
- Added `Consumer.takePositional?` progress lemmas via `ArgStream.remaining`, letting structural consumers participate in interpreter termination proofs.
- Refactored flag consumption through a reusable `consumeFlagFront` helper and proved a front-length lemma, preparing the groundwork for a stream-level progress theorem.
- Completed the stream-level progress lemma for `consumeFlag`, showing that successful flag removal strictly decreases `ArgStream.remaining` length and tying the helper proofs into the structural invariant.
- Re-expressed `consumeValue` directly on `ArgStream`, removing the list-accumulator loop and aligning the option consumer with the existing structural lemmas.
- Proved the `consumeValue` progress lemma via the structural helper, completing the Phase 1 consumer progress work and ensuring option values also decrease `ArgStream.remaining` length.
- Reworked the native CLI sample to showcase the applicative/alternative helpers (`<*>`, `<|>`, `withDefault`), giving downstream users a concrete migration reference built on the structural consumers.
- Expanded the property-test generators to cover short-option permutations and validated `flagLongShort`/`optionLongShortNat` against structural expectations for mixed flag/value inputs.
- Introduced `Argparse.Native.ParsedToken` with a single-pass `classify` that normalises long/short option spellings, preserves inline values, and honours the `--` sentinel while preparing for a list-based interpreter.

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
1. **Structured Token Pass**
   - Replace ad-hoc stream rewrites with a dedicated token classification pass that traverses the raw CLI list once, producing a `ParsedToken` sequence (flags, options with/without inline values, positionals, `--` sentinel).
   - Adopt “last value wins” semantics while constructing the token list so short and long option canonicalisation is uniform and easy to reason about.
   - Preserve original spellings in the token metadata so diagnostics remain faithful to user input.
2. **Interpreter over Parsed Tokens**
   - Re-express `Interpreter.eval` as a fold over the `ParsedToken` list, eliminating the need for `restoreFront` bookkeeping and letting progress proofs rely on plain list recursion.
   - Port the existing consumer lemmas to the new representation and show that every successful step removes exactly one classified token (or keeps it for `missing` cases).
3. **Error Soundness Proofs**
   - Establish that `Interpreter.eval` returns `Unexpected` iff the parsed token list is non-empty, and `Missing` only when the grammar marks an entry as required.
   - Demonstrate equivalence between runtime leftovers and the unconsumed suffix of the classified token list.
4. **Documentation Coherence**
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
 - Confirm that “last value wins” aligns with downstream expectations; document any intentional divergence from the legacy parser’s first-hit behaviour.

## Next Steps
1. Re-express the native interpreter over the `ParsedToken` list with last-value-wins semantics, adapting the existing consumer proofs to the new fold.
2. Extend the property and regression tests to exercise the classifier/integrator pipeline (repeated options, sentinel boundaries, mixed short/long values).
3. Document the parsed-token pass and migration guidance so downstream users understand the semantic shift before broader interpreter changes land.

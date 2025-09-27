# ArgParse SPEC Alignment Plan

## Guiding Principles
- Ground all architecture and types in the reference design from `SPEC.md`; no legacy constraints apply.
- Prefer the simplest proof-friendly data structures (plain `List`/records) outlined in the spec, even if it requires deleting existing code.
- Every milestone ships with docs, tests, and proofs as prescribed, and `PLAN.md` records both successful and failed experiments.
- Reuse Lean standard type classes (`Functor`, `Applicative`, `Alternative`, etc.) and keep implementations private when possible.

## Snapshot & Gap Analysis
- Repository currently contains a bespoke "native" parser whose structure diverges from the spec (no unified AST, no Doc/Proof trees, limited module separation).
- Core types (`State`, `Result`, `Error`, `Expect`) do not match the spec’s definitions; normalization and parser semantics need to be rebuilt around the spec’s two-list token split with a flattened cursor.
- Module tree does not reflect the desired layout under `ArgParse/Core`, `Spec`, `Doc`, `Proofs`, `CLI`, `Examples`, and `Tests`.
- Documentation/help/completion generators, proof skeletons, and milestone tests described in the spec are absent.

## High-Level Roadmap (mirrors SPEC milestones)
1. **M0 – Scaffolding**
   - Create the module hierarchy under `ArgParse/` as listed in the spec.
   - Stub key files with module headers, docstrings, and TODO notes.
2. **M1 – Core Runtime**
   - Implement `Tokens`, `State`, `Result`, `Error`, `Expect`, `ErrorKind`, and the `Parser` type with `Functor`/`Applicative`/`Alternative` instances.
   - Provide initial proofs: Functor/Applicative laws (record under `Proofs/Laws.lean`).
3. **M2 – Normalization & Sentinel**
   - Implement `ArgParse.Core.Normalize.normalize` splitting on `--` and populating `State`.
   - Prove sentinel lemmas (`post_is_positional`, `stability`).
4. **M3 – Value Parsing & Spec AST**
   - Define `FromArg` class plus core instances in `Core/Value.lean`.
   - Introduce the Spec AST in `Spec/AST.lean` and ensure it can express flags/options/positionals/subcommands.
5. **M4 – Parser Elaboration**
   - Build applicative combinators and an elaborator (`Spec/Elab.lean`) producing `Parser` from the AST.
   - Implement runtime combinators in `Core/Combinators.lean` with correctness proofs in `Proofs/Soundness.lean`.
6. **M5 – Docs & Tooling**
   - Implement help/man/completion emitters and align them with the AST.
   - Ensure `CLI/Print.lean` provides the built-ins (`--help`, `--man`, `--generate-completions`).
7. **M6 – Proof Suite Expansion**
   - Add determinism, totality, and soundness proofs using the spec’s structure.
8. **M7 – Testing & Examples**
   - Populate `Tests/Unit.lean`, `Tests/Golden.lean`, and example apps (`Examples/Xargs0.lean`, `Examples/GitLike.lean`).
   - Maintain property tests for option permutations, sentinel handling, and positional overflow.
9. **M8 – Polish & Docs**
   - Finalize docstrings, README, migration guidance, and ensure CI (`lake build`, `lake test`) covers all milestones.

## Activity Log
- 2025-09-27: Re-read `SPEC.md`/`KNOWLEDGE.md`; prepared to align plan accordingly.
- 2025-09-27: Scaffolding audit complete — inventoried legacy modules under `Argparse/Basic` and `Argparse/Native`; marked them for removal during migration to the `ArgParse/` hierarchy.
- 2025-09-27: Created SPEC-aligned module skeleton under `ArgParse/` (Core, Spec, Doc, Proofs, CLI, Examples, Tests).
- 2025-09-27: Drafted core runtime types (`Tokens`, `State`, `Result`, `Error`, `Expect`, `ErrorKind`) per the spec in `Argparse/Core/Types.lean`.
- 2025-09-27: Implemented parser core (`Parser` alias plus Functor/Applicative/Alternative instances) in `Argparse/Core/Parser.lean`.
- 2025-09-27: Removed the legacy `Argparse.Basic`/`Argparse.Native` implementations and stubbed the new module tree in `Argparse.lean`, `Main.lean`, and `Tests/Main.lean`.
- 2025-09-27: Added normalization pass (`Argparse.Core.Normalize.normalize`) splitting tokens on `--` into the new state record.
- 2025-09-27: Introduced the `FromArg` class with baseline instances (String, Substring, Nat, Int, Bool) in `Argparse/Core/Value.lean`.
- _Please append future successes and failures here with short rationales._

## Immediate Next Steps
1. ✅ **Scaffolding audit** (2025-09-27): Inventoried legacy files under `Argparse/` and flagged them for removal during the migration to the `ArgParse/` hierarchy.
2. ✅ **Module skeleton** (2025-09-27): Generated empty modules for each file listed in the spec (Core, Spec, Doc, Proofs, CLI, Examples, Tests) with minimal docstrings and `section`s.
3. ✅ **Core type draft** (2025-09-27): Introduced the spec’s core types (`Tokens`, `State`, `Result`, `Error`, etc.) in `Argparse/Core/Types.lean`, matching the specification and noting diagnostic fields.
4. ✅ **Parser core** (2025-09-27): Defined `Parser := State → Result` with `pure`/`map`/`seq`/`fail` helpers and `Functor`/`Applicative`/`Alternative` instances.
5. ✅ **Legacy cleanup** (2025-09-27): Removed the old `Argparse.Basic`/`Argparse.Native` hierarchies and replaced aggregated imports with SPEC-aligned stubs.
6. ✅ **Normalization draft** (2025-09-27): Implemented `Argparse.Core.Normalize.normalize` to split tokens on the first `--` and populate the new `State` record.
7. ✅ **Value parsing scaffold** (2025-09-27): Added the `FromArg` class with baseline instances (String, Substring, Nat, Int, Bool) in `Argparse/Core/Value.lean`.
8. **Spec AST skeleton**: Outline the command specification tree in `Argparse/Spec/AST.lean`, covering flags, options, positionals, and subcommands.
9. Update this plan after each task, noting successes or blockers (including negative results) before proceeding to later milestones.

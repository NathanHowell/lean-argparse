# Plan

Working notes for `lean-argparse`. This file tracks current state and what's
next; the historical play-by-play lives in git history.

## Current state (2026-08-07)

- **Runtime**: argv normalization with `--` sentinel splitting; flag parsers
  with short-name bundling; option parsers with `--name value`, `--name=value`,
  inline `-n5` concatenation, and `.one`/`.many`/`.some` arities (last value
  wins for `.one`); positional parsers over pre/post streams; recursive
  subcommand dispatch (`Core.subcommand`); fuelled spec elaborator
  (`Spec.Elab`) folding into the `Partial` accumulator; runner with
  `--help`/`--man`/`--generate-completions` built-ins, leftover detection, and
  summary projection (`runSummary`).
- **Docs**: help/man/completion renderers driven by `Spec.Describe`, sharing
  `Doc.runtimeAnnotations` for runtime "current value" annotations.
- **Proofs** (no `sorry`, no extra axioms, zero warnings): lawful
  Functor/Applicative for `Parser`; flag totality and the generic collector
  cursor lemma (`collectStepsLoop_cursor`); determinism (outcome uniqueness,
  normalization congruence); sentinel factorization; accumulator/summary
  soundness; `Partial.merge` identity/associativity; merge-compatibility
  threaded through elaborator → runner → renderers.
- **Tooling**: demo CLI in `Main.lean` (greet/repeat), unit + golden tests,
  docstring/simp lint driver, doc-gen4 setup under `docbuild/`.

## Roadmap

1. **Completeness** — the missing half of the story: if argv conforms to a
   well-formed spec, parsing succeeds and yields the expected bindings.
2. **Fuel adequacy** — show `Spec.elaborateCommandCore` and the collector
   loops are fuel-independent above the chosen budget, discharging the
   trusted termination assumption.
3. **Builder layer** — typed folding of `Partial` into user records; revisit
   typed accumulators once it exists.
4. **Property tests** — lightweight runtime reassurance for annotated
   help/man output.
5. **Bundle-splitting edge cases** — inline bundles like `-n5v` with
   non-`String` payloads.

## Design notes / decisions pending

- `.one` options use last-value-wins; `.many`/`.some` accumulate
  chronological lists.
- `Partial` payloads are string-typed today; a typed variant is deferred
  until the builder layer needs richer folding.
- Parsing is front-of-stream applicative: composition order dictates argument
  order. Interleaved-order parsing would need a scanning combinator layer.

## Process guardrails

- Always run `lake build; lake test; lake lint` before committing.
- Small, focused commits; keep this file about current state and next steps.

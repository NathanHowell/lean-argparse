# Plan

Working notes for `lean-argparse`. This file tracks current state and what's
next; the historical play-by-play lives in git history.

## Current state (2026-08-07)

- **Runtime**: argv normalization with `--` sentinel splitting; flag parsers
  with short-name bundling; option parsers with `--name value`, `--name=value`,
  inline `-n5` concatenation, and `.one`/`.many`/`.some` arities (last value
  wins for `.one`); order-insensitive scanning combinators (`Core.flagScan`,
  `Core.optionScan` in `Core/Scan.lean`) that match anywhere in the pre
  stream, with `scopedPre` restricting a command's scan to the segment before
  the first subcommand name; positional parsers over pre/post streams;
  recursive subcommand dispatch (`Core.subcommand`); fuelled spec elaborator
  (`Spec.Elab`) folding into the `Partial` accumulator — items reordered by
  `orderItems` so scans run before positionals; runner with
  `--help`/`--man`/`--generate-completions` built-ins, leftover detection, and
  summary projection (`runSummary`).
- **Docs**: help/man/completion renderers driven by `Spec.Describe`, sharing
  `Doc.runtimeAnnotations` for runtime "current value" annotations.
- **Proofs** (no `sorry`, no extra axioms, zero warnings): lawful
  Functor/Applicative for `Parser`; flag totality and the generic collector
  cursor lemma (`collectStepsLoop_cursor`); determinism (outcome uniqueness,
  normalization congruence); sentinel factorization; accumulator/summary
  soundness; `Partial.merge` identity/associativity; merge-compatibility
  threaded through elaborator → runner → renderers; scan/front-of-stream
  agreement on syntactically canonical argv (`Proofs/Scan.lean`, unconditional
  given `Canonical`, with a computed non-vacuity witness).
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
6. **Scan agreement for flags and bundles** — `Canonical` covers options;
   the analogous syntactic canonicality story for flag scanning (and for
   `=`-form/concatenated option tokens, whose classification the kernel cannot
   evaluate because `String.startsWith` is opaque) is still open.

## Design notes / decisions pending

- `.one` options use last-value-wins; `.many`/`.some` accumulate
  chronological lists.
- `Partial` payloads are string-typed today; a typed variant is deferred
  until the builder layer needs richer folding.
- Scanning semantics: flags/options match anywhere within the current
  command's segment (bounded by the first subcommand name and the `--`
  sentinel); positionals stay front-of-stream over the residual tokens.
  Known ambiguity: a detached option value that lexes as a defined flag
  (`--message -v`) is claimed by the flag scan — `--name=value` forces the
  value reading. Elaboration order breaks remaining ties.

## Process guardrails

- Always run `lake build; lake test; lake lint` before committing.
- Small, focused commits; keep this file about current state and next steps.

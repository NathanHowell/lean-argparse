# lean-argparse

An applicative command-line argument parser for Lean 4, in the spirit of
Haskell's [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative) —
with a machine-checked proof suite covering the parser laws, runtime progress,
determinism, `--` sentinel handling, and merge soundness.

A CLI is driven by one set of specification records (`AppSpec`, `CmdSpec`,
`FlagSpec`, `OptSpec`, `PosSpec`): they feed the documentation renderers, the
shell-completion generator, and the built-in `--help`/`--man`/
`--generate-completions` handlers, while the applicative `Parser` combinators
(or the spec elaborator) produce typed results from the same metadata.

## Highlights

- Applicative `Parser` core with proved `LawfulFunctor`/`LawfulApplicative` instances
- Flags with short-name bundling; options with `--name value`, `--name=value`,
  and `-n5` concatenation plus `.one`/`.many`/`.some` arities; positionals;
  recursive subcommands
- `--` sentinel handling with proved token-factorization lemmas
- Structured errors (`unknownLong`, `missingValue`, `leftover`, …) carrying
  context tokens and expectation metadata
- Help text, mdoc man pages, and completion suggestions rendered from the spec,
  optionally annotated with runtime values via `Partial.Summary`
- A proof suite with no `sorry`, no extra axioms, and a lint-clean build

## Example

`Main.lean` ships a demo with `greet` and `repeat` subcommands. Abridged:

```lean
import ArgParse
open ArgParse ArgParse.Spec ArgParse.Core

structure GreetConfig where
  verbose : Bool
  count   : Nat
  name    : String

def greetVerboseFlag : FlagSpec :=
  { short? := some ⟨'v', by decide⟩
  , long?  := some "verbose"
  , «meta» := { name := "verbose", help? := some "Enable verbose output." } }

def greetCountOpt : OptSpec Nat :=
  { short? := some ⟨'n', by decide⟩
  , long?  := some "count"
  , «meta» := { name := "count", metavar? := some "COUNT", default? := some "1" }
  , arity  := .one }

def greetParser : Parser GreetConfig :=
  pure GreetConfig.mk
    <*> Core.flag greetVerboseFlag
    <*> Parser.map (·.getD 1) (Core.option greetCountOpt)
    <*> greetNameParser  -- positional NAME

def appParser : Parser AppCommand :=
  Core.subcommand
    [ { name := "greet",  parser := AppCommand.greet  <$> greetParser }
    , { name := "repeat", parser := AppCommand.repeat <$> repeatParser } ]
```

The entry point normalizes argv, lets `builtinOutcome?` intercept
`--help`/`--man`/`--generate-completions`, runs the parser, and reports
leftover tokens as structured errors:

```
$ lake exe argparse greet -v --count 2 Alice
Hello, Alice! (verbose)
Hello, Alice! (verbose)

$ lake exe argparse greet
error: missing value
  expected: argument NAME
```

Parsing is front-of-stream and applicative: arguments are consumed in the
order the parser is composed.

## Runtime summaries

The runner exposes `runSummary`/`runNormalizedSummary`, folding the raw
`Partial` accumulator into a `Partial.Summary` (last-write-wins flag lookups,
chronological option/positional lists). The renderers accept a summary to
annotate output with current values (`renderHelpWithSummary`,
`renderManWithSummary`, `renderCompletionsWithSummary`).

## What is proved

All theorems live under `ArgParse/Proofs/` and build with zero warnings:

- **Laws** (`Proofs/Laws.lean`) — `LawfulFunctor` and `LawfulApplicative` for
  `Parser`, by case analysis on results.
- **Totality/progress** (`Proofs/Totality.lean`) — flag parsers always succeed
  with explicit witnesses (`flag_result_ok`); the generic collector loop
  advances the cursor by exactly the tokens it consumes
  (`collectStepsLoop_cursor` and its option/positional corollaries).
- **Determinism** (`Proofs/Determinism.lean`) — successful runner outcomes are
  unique (`runRaw_ok_unique`, `run_ok_unique`, `runSummary_ok_unique`), and
  parsing depends only on the normalized token stream
  (`runRaw_congr_normalize`).
- **Sentinel** (`Proofs/Sentinel.lean`) — `normalize` factors tokens around
  the first `--` (`sentinel_present_normalize`, `sentinel_absent_post_nil`).
- **Soundness** (`Proofs/Soundness.lean`, `Proofs/Soundness/Summary.lean`) —
  summaries are faithful to the parsed payload
  (`runSummary_ok_exists_partial`); `Partial.merge` forms a monoid-like
  algebra; merge-compatibility is carried from item elaboration through
  subcommand recursion, the runner, and the help/man/completion renderers.

The roadmap (see `PLAN.md`) targets the remaining big results: completeness
(conforming argv always parses) and fuel adequacy for the elaborator.

## Development

```sh
lake build   # library + demo executable
lake test    # #guard-style unit and golden tests
lake lint    # docstring and simp-hygiene linting
```

Generate HTML documentation:

```sh
cd docbuild
DOCGEN_SRC=file lake build ArgParse:docs
```

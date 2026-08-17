# lean-argparse

An applicative command-line argument parser for Lean 4, in the spirit of
Haskell's [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative) —
with a machine-checked proof suite covering the parser laws, runtime progress,
determinism, `--` sentinel handling, scan agreement, and the correspondence
between what help says and what the parser accepts.

**Applications contain no help code.** Every item is declared once. The library
renders `--help`, usage, man pages, completions, and error text from the same
data the parser runs on, and the two cannot drift, because they are only ever
constructed together.

## The idea

The runtime parser stays an opaque function, `State → Result α`. Every public
combinator pairs it with a payload-free description of what it parses, and the
pair travels through one `Applicative` in lockstep:

```lean
structure P (α : Type) where
  doc : Doc        -- what help, usage, and completion read
  run : Parser α   -- State → Result α
```

`Doc` is the static skeleton of a free applicative with the payloads deleted —
which is everything a renderer ever reads. That buys library-owned help without
a universe bump, an interpreter, or restating the proof suite. `DESIGN.md` has
the full argument.

## Quick start

Derive the parser from the configuration record you already had to write:

```lean
import ArgParse
open ArgParse

structure GreetConfig where
  /-- Enable verbose output. -/
  verbose : Short Bool 'v' := ⟨false⟩
  /-- Number of times to greet. -/
  count   : Arg Nat { short? := some 'n', metavar? := some "COUNT" } := ⟨1⟩
  /-- Name to greet. -/
  name    : Positional String
  deriving ArgParse.Parseable

def app : Cmd GreetConfig :=
  .leaf "greet" { name := "greet", help? := some "Print a friendly greeting." }
    (parserFor GreetConfig)

def main (argv : List String) : IO UInt32 :=
  ArgParse.run app argv fun cfg => do
    for _ in [0:cfg.count.val] do
      IO.println s!"Hello, {cfg.name.val}!"
    pure 0
```

Field names become long options (kebab-cased, so `dryRun` is `--dry-run`),
doc-strings become help text, and structure defaults become parser defaults.
What a field *name* cannot say — a short form, that a field is positional, a
metavar — travels in its *type*.

Or write the builders directly, which is what the derive emits anyway:

```lean
open ArgParse.Builder

def greetP : P GreetConfig :=
  GreetConfig.mk
    <$> flag "verbose" (short := 'v') (help := "Enable verbose output.")
    <*> optionD "count" (default := 1) (short := 'n') (metavar := "COUNT")
          (help := "Number of times to greet.")
    <*> positional "NAME" (help := "Name to greet.")
```

Named arguments replace `optparse-applicative`'s `Mod` monoid, which exists only
because Haskell has no keyword arguments. Derived and hand-written commands mix
freely inside one `Cmd`.

## What you get

`Main.lean` ships the demo this output comes from:

```
$ lake exe argparse --help
lean-argparse - Demonstrates subcommands with applicative parsing.

Usage:
  lean-argparse <COMMAND>

Options:
  -h, --help              Show this help text and exit.
  --version               Show the version and exit.
  --man                   Print a man page and exit.
  --generate-completions  List completion candidates and exit.

Commands:
  greet                   Print a friendly greeting.
  repeat                  Repeat a message multiple times.

$ lake exe argparse greet --help          # help for the command you named
lean-argparse greet - Print a friendly greeting.

Usage:
  lean-argparse greet [--verbose] [--count COUNT] NAME

Arguments:
  NAME                    Name to greet.

Options:
  -v, --verbose           Enable verbose output.
  -n, --count COUNT       Number of times to greet. [default: 1]
  ...

$ lake exe argparse greet -v --count 2 Alice
Hello, Alice! (verbose)
Hello, Alice! (verbose)

$ lake exe argparse greet Alice --count 2 -v      # any argument order works
Hello, Alice! (verbose)
Hello, Alice! (verbose)

$ lake exe argparse greet
error: missing the argument NAME

Usage:
  lean-argparse greet [--verbose] [--count COUNT] NAME

For more information, try `lean-argparse greet --help`.

$ lake exe argparse gret Alice
error: unrecognised `gret`; did you mean `greet`?
```

## Features

- Applicative `P` with proved `LawfulFunctor`/`LawfulApplicative` instances on
  the underlying `Parser`
- Flags with short-name bundling; options with `--name value`, `--name=value`,
  and `-n5` concatenation plus `.one`/`.many`/`.some` arities; positionals;
  recursive subcommands with per-node global options
- Order-insensitive parsing: flags and options are *scanned* out of the current
  command's segment rather than consumed front-of-stream, so
  `greet --count 2 -v Alice` and `greet Alice -v --count 2` parse identically
- `--` sentinel handling with proved token-factorization lemmas, so
  `greet --count 1 -- -v` greets `-v`
- Structured errors carrying context tokens and expectations, rendered with
  usage and a nearest-match suggestion
- `--help` at every level, `--version`, mdoc man pages, and position-aware
  completion candidates — all owned by the runner
- Typed verbs: a `Cmd AppCommand` maps leaves straight into your own inductive,
  with no stringly recovery step
- A proof suite with no `sorry`, no extra axioms, and a lint-clean build

One documented ambiguity: a detached option value that lexes as a defined flag
(`--message -v`) is claimed by the flag scan first; write `--message=-v` to force
the value reading.

## Layers

| | |
|---|---|
| `ArgParse.Core` | `Parser = State → Result α`, scanning combinators, normalization |
| `ArgParse.P` | the paired applicative: `Doc` + `Parser` |
| `ArgParse.Builder` | the only place `doc` and `run` are zipped together |
| `ArgParse.Cmd` | the command tree, with `toParser` and `toCmdSpec` |
| `ArgParse.Exec` | the runner: builtins, usage, errors, completion |
| `ArgParse.Correspondence` | help says what the parser accepts |
| `ArgParse.Deriving` | `deriving Parseable` |

## What is proved

All theorems live under `ArgParse/Proofs/` and `ArgParse/Correspondence.lean`,
and build with zero warnings:

- **Laws** (`Proofs/Laws.lean`) — `LawfulFunctor` and `LawfulApplicative` for
  `Parser`, by case analysis on results.
- **Totality/progress** (`Proofs/Totality.lean`) — flag parsers always succeed
  with explicit witnesses (`flag_result_ok`, `flagScan_result_ok`); the generic
  collector loop advances the cursor by exactly the tokens it consumes
  (`collectStepsLoop_cursor` and its option/positional/scanning corollaries).
- **Determinism** (`Proofs/Determinism.lean`) — parsers are functions, so a
  parse result at a state is unique (`parser_ok_unique`, `parser_ext`).
- **Scan agreement** (`Proofs/Scan.lean`) — the scanning combinators reduce to
  the front-of-stream ones on canonically ordered argv, where "canonical" is a
  syntactic condition on the token stream rather than an assumption:
  `optionScan_eq_option_of_canonical` carries no further hypothesis, and
  `canonicalExample` witnesses it for a real named option.
- **Sentinel** (`Proofs/Sentinel.lean`) — `normalize` factors tokens around the
  first `--` (`sentinel_present_normalize`, `sentinel_absent_post_nil`).
- **Correspondence** (`Correspondence.lean`) — the sync guard as a theorem.
  Item agreement: the lexemes the scanner matches and the lexemes help
  advertises are the same lexemes, per builder. Behavioural acceptance: a flag
  accepts each form it advertises and ignores what it does not. Verb agreement:
  `toCmdSpec` lists exactly the names `toParser` dispatches on, at every depth.
  Help coverage: every visible item reaches the page.

Several correspondence proofs are `rfl`. That is the result, not a shortcut:
the two halves are the same data, so nothing is left to check. Against a
hand-maintained help declaration the same statements are false.

Help *totality* is deliberately not among them — nothing outside `Core` is
`partial` and Lean admits no non-terminating definition, so it holds by
construction, and asserting it would be a tautology.

## Development

```sh
lake build   # library + demo executable
lake test    # unit and integration checks
lake lint    # docstring and simp-hygiene linting
```

Generate HTML documentation:

```sh
cd docbuild
DOCGEN_SRC=file lake build ArgParse:docs
```

`DESIGN.md` is the design of record. `PLAN.md` tracks current state and what is
next.

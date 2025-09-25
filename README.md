# lean-argparse

`lean-argparse` provides an applicative command line argument parser for Lean 4, inspired by Haskell's [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative).

## Highlights

- Applicative combinators (`Parser`, `<*>`, `map`) with built-in `Functor`/`Applicative` instances
- Primitive parsers for options, flags, switches, positional arguments, and subcommands
- Usage metadata with automatic help text rendering (including a default `-h/--help` entry)
- Structured error reporting (missing arguments, invalid values, unexpected leftovers)
- Convenience value readers plus builder-style modifiers (`OptionSpec`, `FlagSpec`, `ParserInfo`)
- Higher-level combinators such as `many`, `some`, `choice`, `flag'`, and ready-made option helpers (`strOption`, `natOption`, …)
- Lightweight tests expressed via `#guard`

## Example

The executable bundled with the project demonstrates a small CLI.

```lean
import Argparse

open Argparse

structure Config where
  verbose : Bool
  count : Nat
  name : String

def configParser : Parser Config :=
  pure Config.mk
    <*> switch "verbose" (short? := some 'v') (help? := some "Enable verbose output")
    <*> Parser.withDefault
          (natOption [
            OptionSpec.long "count",
            OptionSpec.short 'n',
            OptionSpec.setMetavar "COUNT",
            OptionSpec.help "How many times to greet",
            OptionSpec.showDefault "1"
          ])
          1
    <*> rawArgument "NAME" (help? := some "Name to greet")

def info : ParserInfo Config :=
  ParserInfo.build configParser [
    ParserInfo.withProgName "lean-argparse",
    ParserInfo.withProgDesc "Demonstrates the Lean applicative argument parser"
  ]

def main (args : List String) : IO Unit :=
  match Argparse.ParserInfo.exec info args with
  | .success cfg => IO.println s!"Hello, {cfg.name}! (count := {cfg.count}, verbose := {cfg.verbose})"
  | .showHelp => IO.println (Argparse.ParserInfo.renderHelp info)
  | .failure err => do
      IO.eprintln (Argparse.ParserInfo.renderFailure info err)
      IO.Process.exit 1

def bashCompletionScript : String :=
  Argparse.ParserInfo.renderBashCompletion info

def zshCompletionScript : String :=
  Argparse.ParserInfo.renderZshCompletion info

def fishCompletionScript : String :=
  Argparse.ParserInfo.renderFishCompletion info
```

Running the compiled executable yields:

```
$ lake build
$ ./build/bin/lean-argparse Alice
Hello, Alice!

$ ./build/bin/lean-argparse --count 3 -v Bob
Hello, Bob! (verbose)
Hello, Bob! (verbose)
Hello, Bob! (verbose)

$ ./build/bin/lean-argparse --help
Lean argparse example
Usage: lean-argparse [--verbose] [--count COUNT] NAME
...
```

## Development

- Build the library and example executable:
  ```sh
  lake build
  ```

- Run the lightweight unit tests:
  ```sh
  lake test
  ```

Tests are located under `Tests/` and use `#guard` checks; compiling the test executable verifies the expectations without shipping test modules with the library.

- Generate HTML documentation (after running `lake update doc-gen4` once inside `docbuild/`):
  ```sh
  cd docbuild
  DOCGEN_SRC=file lake build Argparse:docs
  ```

## Shell Completions

Use the renderer that matches your shell to produce a completion script:

```lean
#eval IO.println (Argparse.ParserInfo.renderBashCompletion info)
#eval IO.println (Argparse.ParserInfo.renderZshCompletion info)
#eval IO.println (Argparse.ParserInfo.renderFishCompletion info)
```

Redirect the output to a file and source it (or copy it into the appropriate completion directory) to enable tab completion for options and subcommands.

## License

Apache-2.0
- `many`, `some`, `choice`, and `many1` allow repeated or alternative argument parsing without leaving the applicative world.
- Builder helpers (e.g. `OptionSpec.long`, `FlagSpec.long`, `ParserInfo.withProgDesc`) make it easy to mirror the ergonomic modifiers from `optparse-applicative`.
- Convenience wrappers (`strOption`, `natOption`, `intOption`, `flag'`) remove most manual record instantiations.
- Shell completion generators (`renderBashCompletion`, `renderZshCompletion`, `renderFishCompletion`) produce scripts for popular shells.

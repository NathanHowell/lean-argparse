# lean-argparse

`lean-argparse` provides an applicative command line argument parser for Lean 4, inspired by Haskell's [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative).

## Highlights

- Applicative combinators (`Parser`, `<*>`, `map`) with built-in `Functor`/`Applicative` instances
- Primitive parsers for options, flags, switches, positional arguments, and subcommands
- Usage metadata with automatic help text rendering (including a default `-h/--help` entry)
- Structured error reporting (missing arguments, invalid values, unexpected leftovers)
- Convenience value readers for common numeric types and helpers such as `withDefault`
- Lightweight tests expressed via `#guard`

## Example

The executable bundled with the project demonstrates a small CLI.

```lean
import LeanArgparse

open Argparse

structure Config where
  verbose : Bool
  count : Nat
  name : String

def configParser : Parser Config :=
  pure Config.mk
    <*> switch "verbose" (short? := some 'v') (help? := some "Enable verbose output")
    <*> withDefault
          (option {
            long? := some "count",
            short? := some 'n',
            metavar := "COUNT",
            help? := some "How many times to greet",
            reader := LeanArgparse.ValueReader.nat,
            showDefault? := some "1"
          })
          1
    <*> rawArgument "NAME" (help? := some "Name to greet")

def info : ParserInfo Config := {
  progName := "lean-argparse",
  parser := configParser,
  progDesc? := some "Demonstrates the Lean applicative argument parser"
}

def main (args : List String) : IO Unit :=
  match Argparse.exec info args with
  | .success cfg => IO.println s!"Hello, {cfg.name}! (count := {cfg.count}, verbose := {cfg.verbose})"
  | .showHelp => IO.println (Argparse.renderHelp info)
  | .failure err => do
      IO.eprintln (Argparse.renderFailure info err)
      IO.Process.exit 1
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

- Verify the lightweight unit tests:
  ```sh
  lake build LeanArgparse.Tests
  ```

The tests live in `LeanArgparse/Tests.lean` and exercise the public combinators via `#guard` checks.

## License

Apache-2.0

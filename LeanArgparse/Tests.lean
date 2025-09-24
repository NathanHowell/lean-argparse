import LeanArgparse

open Argparse
open Argparse.OptionSpec
open Argparse.FlagSpec

namespace LeanArgparse.Tests

structure ExampleCfg where
  verbose : Bool
  count : Nat
  name : String
  deriving Repr, DecidableEq

def exampleParser : Parser ExampleCfg :=
  pure ExampleCfg.mk
    <*> switch "verbose" (short? := some 'v')
    <*> Parser.withDefault
          (option {
            long? := some "count",
            short? := some 'n',
            metavar := "COUNT",
            reader := ValueReader.nat,
            help? := some "Number of repetitions"
          })
          1
    <*> rawArgument "NAME"

def exampleInfo : ParserInfo ExampleCfg := {
  progName := "example",
  parser := exampleParser
}

#guard (match ParserInfo.exec exampleInfo ["Alice"] with
  | .success cfg => decide (cfg = { verbose := false, count := 1, name := "Alice" })
  | _ => False)

#guard (match ParserInfo.exec exampleInfo ["--verbose", "--count", "3", "Bob"] with
  | .success cfg => decide (cfg = { verbose := true, count := 3, name := "Bob" })
  | _ => False)

#guard (match ParserInfo.exec exampleInfo ["--count", "5"] with
  | .failure err => decide (err.error.kind = .missing)
  | _ => False)

structure CommandResult where
  tag : String
  target? : Option String
  deriving Repr, DecidableEq

def commandParser : Parser CommandResult :=
  subcommand {
    metavar := "CMD",
    commands := [
      {
        name := "hello",
        description? := some "Say hello",
        parser := pure { tag := "hello", target? := none }
      },
      {
        name := "run",
        description? := some "Run against a target",
        parser := pure (fun target => { tag := "run", target? := some target }) <*> rawArgument "TARGET"
      }
    ]
  }

#guard (match ParserInfo.exec { progName := "cmd", parser := commandParser } ["hello"] with
  | .success cfg => decide (cfg = { tag := "hello", target? := none })
  | _ => False)

#guard (match ParserInfo.exec { progName := "cmd", parser := commandParser } ["run", "tests"] with
  | .success cfg => decide (cfg = { tag := "run", target? := some "tests" })
  | _ => False)

#guard (match ParserInfo.exec { progName := "cmd", parser := commandParser } ["unknown"] with
  | .failure err => decide (err.error.kind = .invalid)
  | _ => False)

#guard (match ParserInfo.exec exampleInfo ["--help"] with
  | .showHelp => True
  | _ => False)

def repeatedArgs : Parser (List String) :=
  Parser.many (rawArgument "ITEM")

#guard (match ParserInfo.exec { progName := "items", parser := repeatedArgs } ["one", "two", "three"] with
  | .success items => decide (items = ["one", "two", "three"])
  | _ => False)

#guard (match ParserInfo.exec { progName := "items", parser := repeatedArgs } [] with
  | .success items => decide (items = [])
  | _ => False)

#guard (match ParserInfo.exec { progName := "items", parser := Parser.some (rawArgument "ITEM") } [] with
  | .failure err => decide (err.error.kind = .missing)
  | _ => False)

def requiredFlag : Parser Bool :=
  flag' <|
    FlagSpec.build false true [
      FlagSpec.long "loud",
      FlagSpec.short 'L',
      FlagSpec.help "Enable loud mode"
    ]

#guard (match ParserInfo.exec { progName := "flags", parser := requiredFlag } ["--loud"] with
  | .success value => decide (value = true)
  | _ => False)

#guard (match ParserInfo.exec { progName := "flags", parser := requiredFlag } [] with
  | .failure err => decide (err.error.kind = .missing)
  | _ => False)

def choiceParser : Parser String :=
  Parser.choice [
    strOption [OptionSpec.long "name", OptionSpec.help "Primary name"],
    strOption [OptionSpec.long "alias", OptionSpec.help "Alias"]
  ]

#guard (match ParserInfo.exec { progName := "choice", parser := choiceParser } ["--alias", "Bob"] with
  | .success value => decide (value = "Bob")
  | _ => False)

#guard (match ParserInfo.exec { progName := "choice", parser := choiceParser } [] with
  | .failure err => decide (err.error.kind = .missing)
  | _ => False)

end LeanArgparse.Tests

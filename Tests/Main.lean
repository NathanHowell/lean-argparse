import Argparse

open Argparse
open Argparse.OptionSpec
open Argparse.FlagSpec
open Argparse.Completion

namespace ArgparseTests

private def containsSubstring (haystack needle : String) : Bool :=
  if needle.isEmpty then
    true
  else
    let target := needle.data
    let rec loop : List Char → Bool
      | [] => false
      | chars@(_ :: rest) =>
          if target.isPrefixOf chars then
            true
          else
            loop rest
    loop haystack.data

private structure ExampleCfg where
  verbose : Bool
  count : Nat
  name : String
  deriving Repr, DecidableEq

private def exampleParser : Parser ExampleCfg :=
  pure ExampleCfg.mk
    <*> switch "verbose" (short? := some 'v')
    <*> Parser.withDefault
          (option {
            long? := some "count",
            short? := some 'n',
            metavar := "COUNT",
            reader := Argparse.ValueReader.nat,
            help? := some "Number of repetitions"
          })
          1
    <*> rawArgument "NAME"

private def exampleInfo : ParserInfo ExampleCfg := {
  progName := "example",
  parser := exampleParser
}

#guard (match Argparse.ParserInfo.exec exampleInfo ["Alice"] with
  | .success cfg => decide (cfg = { verbose := false, count := 1, name := "Alice" })
  | _ => False)

#guard (match Argparse.ParserInfo.exec exampleInfo ["--verbose", "--count", "3", "Bob"] with
  | .success cfg => decide (cfg = { verbose := true, count := 3, name := "Bob" })
  | _ => False)

#guard (match Argparse.ParserInfo.exec exampleInfo ["--count", "5"] with
  | .failure err => decide (err.error.kind = .missing)
  | _ => False)

private structure CommandResult where
  tag : String
  target? : Option String
  deriving Repr, DecidableEq

private def commandParser : Parser CommandResult :=
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

#guard (match Argparse.ParserInfo.exec { progName := "cmd", parser := commandParser } ["hello"] with
  | .success cfg => decide (cfg = { tag := "hello", target? := none })
  | _ => False)

#guard (match Argparse.ParserInfo.exec { progName := "cmd", parser := commandParser } ["run", "tests"] with
  | .success cfg => decide (cfg = { tag := "run", target? := some "tests" })
  | _ => False)

#guard (match Argparse.ParserInfo.exec { progName := "cmd", parser := commandParser } ["unknown"] with
  | .failure err => decide (err.error.kind = .invalid)
  | _ => False)

#guard (match Argparse.ParserInfo.exec exampleInfo ["--help"] with
  | .showHelp => True
  | _ => False)

private def repeatedArgs : Parser (List String) :=
  Parser.many (rawArgument "ITEM")

#guard (match Argparse.ParserInfo.exec { progName := "items", parser := repeatedArgs } ["one", "two", "three"] with
  | .success items => decide (items = ["one", "two", "three"])
  | _ => False)

#guard (match Argparse.ParserInfo.exec { progName := "items", parser := repeatedArgs } [] with
  | .success items => decide (items = [])
  | _ => False)

#guard (match Argparse.ParserInfo.exec { progName := "items", parser := Parser.some (rawArgument "ITEM") } [] with
  | .failure err => decide (err.error.kind = .missing)
  | _ => False)

private def requiredFlag : Parser Bool :=
  flag' <|
    FlagSpec.build false true [
      FlagSpec.long "loud",
      FlagSpec.short 'L',
      FlagSpec.help "Enable loud mode"
    ]

#guard (match Argparse.ParserInfo.exec { progName := "flags", parser := requiredFlag } ["--loud"] with
  | .success value => decide (value = true)
  | _ => False)

#guard (match Argparse.ParserInfo.exec { progName := "flags", parser := requiredFlag } [] with
  | .failure err => decide (err.error.kind = .missing)
  | _ => False)

private def choiceParser : Parser String :=
  Parser.choice [
    strOption [OptionSpec.long "name", OptionSpec.help "Primary name"],
    strOption [OptionSpec.long "alias", OptionSpec.help "Alias"]
  ]

#guard (match Argparse.ParserInfo.exec { progName := "choice", parser := choiceParser } ["--alias", "Bob"] with
  | .success value => decide (value = "Bob")
  | _ => False)

#guard (match Argparse.ParserInfo.exec { progName := "choice", parser := choiceParser } [] with
  | .failure err => decide (err.error.kind = .missing)
  | _ => False)

#guard (containsSubstring (Argparse.ParserInfo.renderBashCompletion exampleInfo) "--count")
#guard (containsSubstring (Argparse.ParserInfo.renderZshCompletion exampleInfo) "_arguments")
#guard (containsSubstring (Argparse.ParserInfo.renderFishCompletion exampleInfo) "complete -c")
#guard (containsSubstring (Argparse.ParserInfo.renderManpage exampleInfo) ".SH OPTIONS")

private def completionOnlyParser : Parser Shell :=
  defaultShellOption

private def completionOnlyInfo : ParserInfo Shell := {
  progName := "complete-demo",
  parser := completionOnlyParser
}

#guard (match Argparse.ParserInfo.exec completionOnlyInfo ["--completions", "bash"] with
  | .success shell => decide (Shell.name shell = "bash")
  | _ => False)

#guard (match Argparse.ParserInfo.exec completionOnlyInfo ["--completions", "FISH"] with
  | .success shell => decide (Shell.name shell = "fish")
  | _ => False)

#guard (match Argparse.ParserInfo.exec completionOnlyInfo ["--completions", "unknown"] with
  | .failure err => decide (err.error.kind = .invalid)
  | _ => False)

private def optionalCompletionParser : Parser (Option Shell) :=
  defaultOptionalShellOption

#guard (match Argparse.ParserInfo.exec { progName := "opt-complete", parser := optionalCompletionParser } [] with
  | .success none => True
  | _ => False)

#guard (containsSubstring (Argparse.ParserInfo.renderCompletionFor (Option.get! (Shell.ofString? "bash")) completionOnlyInfo) "--completions")

end ArgparseTests

/-- Trivial entry point for the test executable. -/
def main : IO Unit :=
  pure ()

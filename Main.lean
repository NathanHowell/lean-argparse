import Argparse
import Init.System.IO

open Argparse

structure GreetConfig where
  verbose : Bool
  count : Nat
  name : String
  deriving Repr

structure RepeatConfig where
  times : Nat
  message : String
  deriving Repr

def greetParser : Parser GreetConfig :=
  pure GreetConfig.mk
    <*> Argparse.switch "verbose" (short? := some 'v') (help? := some "Enable verbose output.")
    <*> Parser.withDefault
          (Argparse.option {
            long? := some "count",
            short? := some 'n',
            metavar := "COUNT",
            help? := some "How many times to greet.",
            reader := Argparse.ValueReader.nat,
            showDefault? := some "1"
          })
          1
    <*> Argparse.rawArgument "NAME" (help? := some "Name to greet.")

def repeatParser : Parser RepeatConfig :=
  pure RepeatConfig.mk
    <*> Parser.withDefault
          (Argparse.option {
            long? := some "times",
            short? := some 't',
            metavar := "TIMES",
            help? := some "How many times to repeat the message.",
            reader := Argparse.ValueReader.nat,
            showDefault? := some "2"
          })
          2
    <*> Argparse.rawArgument "MESSAGE" (help? := some "Message to repeat.")

inductive AppCommand where
  | greet (cfg : GreetConfig)
  | repeat (cfg : RepeatConfig)
  | completions (shell : Argparse.Completion.Shell)
  deriving Repr

def completionCommand : Parser AppCommand :=
  AppCommand.completions <$> Argparse.Completion.defaultShellOption

def subcommandParser : Parser AppCommand :=
  Argparse.subcommand {
    metavar := "COMMAND",
    help? := some "Action to perform.",
    commands := [
      {
        name := "greet",
        description? := some "Print a friendly greeting.",
        parser := AppCommand.greet <$> greetParser
      },
      {
        name := "repeat",
        description? := some "Repeat a message multiple times.",
        parser := AppCommand.repeat <$> repeatParser
      }
    ]
  }

def appParser : Parser AppCommand :=
  Parser.choice [completionCommand, subcommandParser]

def appInfo : ParserInfo AppCommand :=
  Argparse.ParserInfo.build appParser [
    Argparse.ParserInfo.withProgName "lean-argparse",
    Argparse.ParserInfo.withHeader "Lean argparse example",
    Argparse.ParserInfo.withProgDesc "Demonstrates subcommands with the applicative argument parser."
  ]

def greetWith (cfg : GreetConfig) : IO Unit := do
  for _ in [0:cfg.count] do
    let line := if cfg.verbose then s!"Hello, {cfg.name}! (verbose)" else s!"Hello, {cfg.name}!"
    IO.println line

def repeatWith (cfg : RepeatConfig) : IO Unit := do
  for _ in [0:cfg.times] do
    IO.println cfg.message

def handleResult (info : ParserInfo AppCommand) : ParserResult AppCommand → IO Unit
  | .success (.greet cfg) => greetWith cfg
  | .success (.repeat cfg) => repeatWith cfg
  | .success (.completions shell) =>
      IO.println (Argparse.ParserInfo.renderCompletionFor shell info)
  | .showHelp => IO.println (Argparse.ParserInfo.renderHelp info)
  | .failure err => do
      IO.eprintln (Argparse.ParserInfo.renderFailure info err)
      IO.Process.exit 1

def main (args : List String) : IO Unit := do
  let info := appInfo
  let result := Argparse.ParserInfo.exec info args
  handleResult info result

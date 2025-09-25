import Argparse
import Init.System.IO

open Argparse

structure Config where
  verbose : Bool
  count : Nat
  name : String
  deriving Repr

def configParser : Parser Config :=
  pure Config.mk
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

inductive AppCommand where
  | run (cfg : Config)
  | completions (shell : Argparse.Completion.Shell)
  deriving Repr

def completionCommand : Parser AppCommand :=
  AppCommand.completions <$> Argparse.Completion.defaultShellOption

def runCommand : Parser AppCommand :=
  AppCommand.run <$> configParser

def appParser : Parser AppCommand :=
  Parser.choice [completionCommand, runCommand]

def appInfo : ParserInfo AppCommand :=
  Argparse.ParserInfo.build appParser [
    Argparse.ParserInfo.withProgName "lean-argparse",
    Argparse.ParserInfo.withHeader "Lean argparse example",
    Argparse.ParserInfo.withProgDesc "Demonstrates basic usage of the applicative argument parser."
  ]

def runWith (cfg : Config) : IO Unit := do
  for _ in [0:cfg.count] do
    let line := if cfg.verbose then s!"Hello, {cfg.name}! (verbose)" else s!"Hello, {cfg.name}!"
    IO.println line

def handleResult (info : ParserInfo AppCommand) : ParserResult AppCommand → IO Unit
  | .success (.run cfg) => runWith cfg
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

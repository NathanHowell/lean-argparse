import LeanArgparse
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
    <*> Argparse.withDefault
          (Argparse.option {
            long? := some "count",
            short? := some 'n',
            metavar := "COUNT",
            help? := some "How many times to greet.",
            reader := LeanArgparse.ValueReader.nat,
            showDefault? := some "1"
          })
          1
    <*> Argparse.rawArgument "NAME" (help? := some "Name to greet.")

def configInfo : ParserInfo Config := {
  progName := "lean-argparse",
  parser := configParser,
  header? := some "Lean argparse example",
  progDesc? := some "Demonstrates basic usage of the applicative argument parser."
}

def runWith (cfg : Config) : IO Unit := do
  for _ in [0:cfg.count] do
    let line := if cfg.verbose then s!"Hello, {cfg.name}! (verbose)" else s!"Hello, {cfg.name}!"
    IO.println line

def handleResult (info : ParserInfo Config) : ParserResult Config → IO Unit
  | .success cfg => runWith cfg
  | .showHelp => IO.println (Argparse.renderHelp info)
  | .failure err => do
      IO.eprintln (Argparse.renderFailure info err)
      IO.Process.exit 1

def main (args : List String) : IO Unit := do
  let info := configInfo
  let result := Argparse.exec info args
  handleResult info result

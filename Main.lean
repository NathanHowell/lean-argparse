import ArgParse

open ArgParse
open ArgParse.Builder

namespace MainApp

/-- Runtime configuration for the `greet` subcommand. -/
structure GreetConfig where
  /-- Whether to print the greeting with a verbose marker. -/
  verbose : Bool
  /-- How many times to print the greeting. -/
  count   : Nat
  /-- Name to greet. -/
  name    : String
  deriving Repr

/-- Runtime configuration for the `repeat` subcommand. -/
structure RepeatConfig where
  /-- How many times to print the message. -/
  times   : Nat
  /-- Message to print. -/
  message : String
  deriving Repr

/-- Enumerates the supported subcommands. -/
inductive AppCommand where
  /-- The `greet` subcommand with its parsed configuration. -/
  | greet (cfg : GreetConfig)
  /-- The `repeat` subcommand with its parsed configuration. -/
  | repeat (cfg : RepeatConfig)
  deriving Repr

/-- Parser for the `greet` payload. Each item is declared once; its help text
travels with it. -/
def greetP : P GreetConfig :=
  GreetConfig.mk
    <$> flag "verbose" (short := 'v') (help := "Enable verbose output.")
    <*> optionD "count" (default := 1) (short := 'n') (metavar := "COUNT")
          (help := "Number of times to greet.")
    <*> positional "NAME" (help := "Name to greet.")

/-- Parser for the `repeat` payload. -/
def repeatP : P RepeatConfig :=
  RepeatConfig.mk
    <$> optionD "times" (default := 2) (short := 't') (metavar := "TIMES")
          (help := "How many times to repeat the message.")
    <*> positional "MESSAGE" (help := "Message to repeat.")

/-- The whole command-line interface. Every verb appears exactly once. -/
def app : Cmd AppCommand :=
  .node "lean-argparse" { name := "lean-argparse"
                        , help? := some "Demonstrates subcommands with applicative parsing." }
    (pure id)
    [ .leaf "greet" { name := "greet", help? := some "Print a friendly greeting." }
        (AppCommand.greet <$> greetP)
    , .leaf "repeat" { name := "repeat", help? := some "Repeat a message multiple times." }
        (AppCommand.repeat <$> repeatP) ]

/-- Execute the `greet` command payload. -/
def runGreet (cfg : GreetConfig) : IO UInt32 := do
  let suffix := if cfg.verbose then " (verbose)" else ""
  for _ in [0:cfg.count] do
    IO.println s!"Hello, {cfg.name}!{suffix}"
  pure 0

/-- Execute the `repeat` command payload. -/
def runRepeat (cfg : RepeatConfig) : IO UInt32 := do
  for _ in [0:cfg.times] do
    IO.println cfg.message
  pure 0

end MainApp

/-- Entry point. There is no help, usage, version, or error-rendering code here
and there is nowhere for one to hide: `ArgParse.run` owns all of it. -/
def main (argv : List String) : IO UInt32 :=
  ArgParse.run MainApp.app argv (cfg := { version? := some "0.2.0" }) fun
    | .greet cfg  => MainApp.runGreet cfg
    | .repeat cfg => MainApp.runRepeat cfg

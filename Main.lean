import ArgParse

open ArgParse
open ArgParse.Builder

namespace MainApp

/-- Runtime configuration for the `greet` subcommand.

Deriving `Parseable` is the whole parser. Field names become long options, these
doc-strings become the help text, and the defaults become the parser's defaults;
the type wrappers carry what a field name cannot say -- the short form, the
positional, the metavar. -/
structure GreetConfig where
  /-- Enable verbose output. -/
  verbose : Short Bool 'v' := ⟨false⟩
  /-- Number of times to greet. -/
  count   : Arg Nat { short? := some 'n', metavar? := some "COUNT" } := ⟨1⟩
  /-- Name to greet. -/
  name    : Positional String
  deriving Repr, ArgParse.Parseable

/-- Runtime configuration for the `repeat` subcommand. -/
structure RepeatConfig where
  /-- How many times to repeat the message. -/
  times   : Arg Nat { short? := some 't', metavar? := some "TIMES" } := ⟨2⟩
  /-- Message to repeat. -/
  message : Positional String
  deriving Repr, ArgParse.Parseable

/-- Enumerates the supported subcommands. -/
inductive AppCommand where
  /-- The `greet` subcommand with its parsed configuration. -/
  | greet (cfg : GreetConfig)
  /-- The `repeat` subcommand with its parsed configuration. -/
  | repeat (cfg : RepeatConfig)
  deriving Repr

/-- The whole command-line interface. Every verb appears exactly once. -/
def app : Cmd AppCommand :=
  .node "lean-argparse" { name := "lean-argparse"
                        , help? := some "Demonstrates subcommands with applicative parsing." }
    (pure id)
    [ .leaf "greet" { name := "greet", help? := some "Print a friendly greeting." }
        (AppCommand.greet <$> parserFor GreetConfig)
    , .leaf "repeat" { name := "repeat", help? := some "Repeat a message multiple times." }
        (AppCommand.repeat <$> parserFor RepeatConfig) ]

/-- Execute the `greet` command payload. -/
def runGreet (cfg : GreetConfig) : IO UInt32 := do
  let suffix := if cfg.verbose.val then " (verbose)" else ""
  for _ in [0:cfg.count.val] do
    IO.println s!"Hello, {cfg.name.val}!{suffix}"
  pure 0

/-- Execute the `repeat` command payload. -/
def runRepeat (cfg : RepeatConfig) : IO UInt32 := do
  for _ in [0:cfg.times.val] do
    IO.println cfg.message.val
  pure 0

end MainApp

/-- Entry point. There is no help, usage, version, or error-rendering code here
and there is nowhere for one to hide: `ArgParse.run` owns all of it. -/
def main (argv : List String) : IO UInt32 :=
  ArgParse.run MainApp.app argv (cfg := { version? := some "0.2.0" }) fun
    | .greet cfg  => MainApp.runGreet cfg
    | .repeat cfg => MainApp.runRepeat cfg

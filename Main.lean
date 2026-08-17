import ArgParse
import Std

open ArgParse
open ArgParse.Spec
open ArgParse.Core

namespace MainApp

/-- Metadata helper used when populating spec entries. -/
def mkMeta (name : String) (help? : Option String := none)
    (metavar? : Option String := none) (default? : Option String := none) : Meta :=
  { name := name, help? := help?, metavar? := metavar?, default? := default? }

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

/-! ### Specification entries shared by docs and the applicative parser -/

/-- `--verbose` / `-v` flag for the greet command. -/
def greetVerboseFlag : FlagSpec :=
  { short? := some ⟨'v', by decide⟩
  , long?  := some "verbose"
  , «meta» := mkMeta "verbose" (help? := some "Enable verbose output.") }

/-- `--count` / `-n` option for the greet command. -/
def greetCountOpt : OptSpec Nat :=
  { short? := some ⟨'n', by decide⟩
  , long?  := some "count"
  , «meta» := mkMeta "count"
      (help? := some "Number of times to greet.")
      (metavar? := some "COUNT")
      (default? := some "1")
  , arity  := .one }

/-- Required NAME positional for the greet command. -/
def greetNamePos : PosSpec String :=
  { «meta» := mkMeta "NAME" (help? := some "Name to greet."), arity := .one }

/-- `--times` / `-t` option for the repeat command. -/
def repeatTimesOpt : OptSpec Nat :=
  { short? := some ⟨'t', by decide⟩
  , long?  := some "times"
  , «meta» := mkMeta "times"
      (help? := some "How many times to repeat the message.")
      (metavar? := some "TIMES")
      (default? := some "2")
  , arity  := .one }

/-- Required MESSAGE positional for the repeat command. -/
def repeatMessagePos : PosSpec String :=
  { «meta» := mkMeta "MESSAGE" (help? := some "Message to repeat."), arity := .one }

/-- Specification for the `greet` subcommand. -/
def greetCmdSpec : CmdSpec :=
  { name := "greet"
  , «meta» := mkMeta "greet" (help? := some "Print a friendly greeting.")
  , args :=
      [ ItemSpec.flag greetVerboseFlag
      , ItemSpec.opt greetCountOpt
      , ItemSpec.pos greetNamePos ] }

/-- Specification for the `repeat` subcommand. -/
def repeatCmdSpec : CmdSpec :=
  { name := "repeat"
  , «meta» := mkMeta "repeat" (help? := some "Repeat a message multiple times.")
  , args :=
      [ ItemSpec.opt repeatTimesOpt
      , ItemSpec.pos repeatMessagePos ] }

/-- Root command specification used for docs and built-ins. -/
def rootCmdSpec : CmdSpec :=
  { name := "lean-argparse"
  , «meta» := mkMeta "lean-argparse"
      (help? := some "Demonstrates subcommands with applicative parsing.")
  , subs := [greetCmdSpec, repeatCmdSpec] }

/-- Application specification exposed to the CLI helpers. -/
def appSpec : AppSpec :=
  { name := "lean-argparse"
  , version? := some "0.2.0"
  , about? := some "Applicative demo rebuilt on the SPEC-aligned core"
  , root := rootCmdSpec }

/-! ### Applicative parser helpers -/

/-- Parse `--count`, defaulting to one greeting. -/
def greetCountParser : Parser Nat :=
  Parser.map (fun opt => opt.getD 1) (Core.optionScan greetCountOpt)

/-- Parse the required NAME positional. -/
def greetNameParser : Parser String := fun st =>
  match Core.positional greetNamePos st with
  | .err err => .err err
  | .ok (some value) st' => .ok value st'
  | .ok none _ =>
      let err : Error :=
        { kind := .missingValue
        , context := []
        , expect := [Expect.positional greetNamePos.«meta».name] }
      .err err

/-- Parse `--times`, defaulting to two repetitions. -/
def repeatTimesParser : Parser Nat :=
  Parser.map (fun opt => opt.getD 2) (Core.optionScan repeatTimesOpt)

/-- Parse the required MESSAGE positional. -/
def repeatMessageParser : Parser String := fun st =>
  match Core.positional repeatMessagePos st with
  | .err err => .err err
  | .ok (some value) st' => .ok value st'
  | .ok none _ =>
      let err : Error :=
        { kind := .missingValue
        , context := []
        , expect := [Expect.positional repeatMessagePos.«meta».name] }
      .err err

/-- Parser for the `greet` subcommand payload. -/
def greetParser : Parser GreetConfig :=
  pure GreetConfig.mk
    <*> Core.flagScan greetVerboseFlag
    <*> greetCountParser
    <*> greetNameParser

/-- Parser for the `repeat` subcommand payload. -/
def repeatParser : Parser RepeatConfig :=
  pure RepeatConfig.mk
    <*> repeatTimesParser
    <*> repeatMessageParser

/-- Parse the subcommand token and dispatch to the appropriate parser. -/
def appParser : Parser AppCommand :=
  let entries : List (Core.Subcommand AppCommand) :=
    [ { name := "greet", parser := AppCommand.greet <$> greetParser }
    , { name := "repeat", parser := AppCommand.repeat <$> repeatParser } ]
  Core.subcommand entries

/-! ### Runtime helpers -/

/-- Render a structured parse error for display. -/
def renderError (err : Error) : String :=
  let kindStr :=
    match err.kind with
    | .unknownShort => "unknown short flag"
    | .unknownLong  => "unknown long option"
    | .missingValue => "missing value"
    | .leftover     => "unexpected leftover arguments"
    | .conflict     => "conflicting options"
    | .custom       => "application error"
  let context :=
    match err.context with
    | [] => ""
    | tokens => s!"\n  context: {String.intercalate " " tokens}"
  let expects :=
    match err.expect with
    | [] => ""
    | es =>
        let rendered := es.map fun
          | Expect.flag short? long? =>
              let shortStr := short?.map (fun c => s!"-{c}")
              let longStr  := long?.map (fun name => s!"--{name}")
              String.intercalate " or " (List.filterMap id [shortStr, longStr])
          | Expect.optionVal name  => s!"value for option {name}"
          | Expect.positional name => s!"argument {name}"
          | Expect.subcommand name => s!"subcommand {name}"
          | Expect.endOfInput      => "end of input"
        s!"\n  expected: {String.intercalate ", " rendered}"
  s!"error: {kindStr}{context}{expects}"

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

/-- Execute the parsed command. -/
def runCommand : AppCommand → IO UInt32
  | .greet cfg  => runGreet cfg
  | .repeat cfg => runRepeat cfg

end MainApp

/-- Entry point mirroring the applicative example from the legacy repository. -/
def main (argv : List String) : IO UInt32 := do
  let st₀ := ArgParse.Core.normalize argv
  match ArgParse.builtinOutcome? (α := Unit) MainApp.appSpec st₀ with
  | some outcome =>
      match outcome.result with
      | ArgParse.RunResult.help text => IO.println text; pure 0
      | ArgParse.RunResult.man text => IO.println text; pure 0
      | ArgParse.RunResult.completions text => IO.println text; pure 0
      | ArgParse.RunResult.ok _ => pure 0
      | ArgParse.RunResult.err err => IO.eprintln (MainApp.renderError err); pure 2
  | none =>
      match MainApp.appParser st₀ with
      | .err err => IO.eprintln (MainApp.renderError err); pure 2
      | .ok command st₁ =>
          if st₁.pre ≠ [] ∨ st₁.post ≠ [] then
            let leftovers := st₁.pre ++ st₁.post
            let err : Error :=
              { kind := .leftover, context := leftovers, expect := [Expect.endOfInput] }
            IO.eprintln (MainApp.renderError err)
            pure 2
          else
            MainApp.runCommand command

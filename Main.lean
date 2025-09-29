import ArgParse
import Std

open ArgParse
open ArgParse.Spec

namespace MainApp

/-- Convenience helper for constructing `Meta` records. -/
def mkMeta (name : String) (help? : Option String := none)
    (metavar? : Option String := none) (default? : Option String := none) : Meta :=
  { name := name, help? := help?, metavar? := metavar?, default? := default? }

/-- Short flag for verbose output. -/
def verboseShort : Short := { c := 'v', ok := by decide }

/-- Short flag for the count option. -/
def countShort : Short := { c := 'n', ok := by decide }

/-- Flag enabling verbose output. -/
def verboseFlag : FlagSpec :=
  { short? := some verboseShort
  , long?  := some "verbose"
  , «meta» := mkMeta "verbose" (help? := some "Enable verbose output") }

/-- Option recording how many times to repeat the greeting. -/
def countOpt : OptSpec Nat :=
  { short? := some countShort
  , long?  := some "count"
  , «meta» := mkMeta "count"
      (help? := some "Number of times to greet")
      (metavar? := some "COUNT")
      (default? := some "1")
  , arity  := .one }

/-- Required positional argument capturing the names to greet. -/
def namePos : PosSpec String :=
  { «meta» := mkMeta "NAME" (help? := some "Name to greet")
  , arity  := .some }

/-- Root command specification for the demo application. -/
def rootCommand : CmdSpec :=
  { name := "lean-argparse"
  , «meta» := mkMeta "lean-argparse"
      (help? := some "Demonstrates the Lean argparse runtime")
  , args := [ItemSpec.flag verboseFlag, ItemSpec.opt countOpt, ItemSpec.pos namePos] }

/-- Complete application specification surfaced to the runner. -/
def appSpec : AppSpec :=
  { name := "lean-argparse"
  , version? := some "0.2.0"
  , about? := some "Small greeting CLI rebuilt during the SPEC-aligned rewrite"
  , root := rootCommand }

/-- Derived configuration extracted from the parse summary. -/
structure Config where
  verbose : Bool
  count   : Nat
  names   : List String
  deriving Repr

/-- Convert a payload summary into a structured configuration. -/
def summaryToConfig (summary : Partial.Summary) : Config :=
  let verbose := summary.flagValue? "verbose" |>.getD false
  let countVals := summary.optionValues "count"
  let count :=
    match countVals.getLast?.bind String.toNat? with
    | some n => n
    | none => 1
  let names := summary.positionalValues "NAME"
  { verbose, count, names }

/-- Render a human-readable description of an `ErrorKind`. -/
def describeKind : ErrorKind → String
  | .unknownShort => "unknown short flag"
  | .unknownLong  => "unknown long flag"
  | .missingValue => "missing required value"
  | .leftover     => "unexpected leftover arguments"
  | .conflict     => "mutually exclusive options used together"
  | .custom       => "application error"

/-- Format an expectation hint for display alongside errors. -/
def describeExpect : Expect → String
  | .flag short? long? =>
      let shortStr := short?.map (fun c => s!"-{c}")
      let longStr  := long?.map (fun n => s!"--{n}")
      String.intercalate " or " (List.filterMap id [shortStr, longStr])
  | .optionVal name   => s!"value for option {name}"
  | .positional name  => s!"argument {name}"
  | .subcommand name  => s!"subcommand {name}"
  | .endOfInput       => "no additional arguments"

/-- Pretty-print a structured parse error. -/
def renderError (err : Error) : String :=
  let header := s!"error: {describeKind err.kind}"
  let ctx :=
    match err.context with
    | [] => ""
    | tokens => s!"\n  context: {String.intercalate " " tokens}"
  let expects :=
    match err.expect.map describeExpect with
    | [] => ""
    | hints => s!"\n  expected: {String.intercalate ", " hints}"
  header ++ ctx ++ expects

/-- Emit greetings according to the parsed configuration. -/
def runGreetings (cfg : Config) : IO UInt32 := do
  match cfg.names.head? with
  | none =>
      IO.eprintln "error: expected at least one NAME argument"
      pure 2
  | some _ => do
      let suffix := if cfg.verbose then " (verbose)" else ""
      for _ in List.range cfg.count do
        for name in cfg.names do
          IO.println s!"Hello, {name}!{suffix}"
      pure 0

/-- Run the parser on raw argv tokens and execute the application logic. -/
def run (argv : List String) : IO UInt32 := do
  let outcome := ArgParse.runSummary appSpec argv
  match outcome.result with
  | .ok summary => runGreetings (summaryToConfig summary)
  | .help text => IO.println text; pure 0
  | .man text => IO.println text; pure 0
  | .completions text => IO.println text; pure 0
  | .err err => IO.eprintln (renderError err); pure 2

end MainApp

/-- Entry point that reuses the SPEC-driven runner. -/
def main (argv : List String) : IO UInt32 :=
  MainApp.run argv

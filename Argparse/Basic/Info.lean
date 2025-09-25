import Std
import Argparse.Basic.Parser
import Argparse.Basic.Docs
import Argparse.Basic.ParseState

namespace Argparse

open Std
open Usage

/-- Result returned when parsing fails. -/
structure ParserFailure where
  /-- The underlying structured parse error. -/
  error : ParseError
  /-- Usage information to display alongside the error. -/
  usage : Usage
  /-- Unconsumed arguments at the point of failure. -/
  leftovers : List String := []
  deriving Repr

/-- Outcome of invoking a parser. -/
inductive ParserResult (α : Type u)
  /-- Parsing succeeded and produced a value. -/
  | success (value : α)
  /-- Parsing was cancelled in order to display help text. -/
  | showHelp
  /-- Parsing failed with diagnostic information. -/
  | failure (err : ParserFailure)
  deriving Repr

/-- Metadata bundled with a parser for rendering help, errors, and completions. -/
structure ParserInfo (α : Type u) where
  /-- Program name displayed in usage text. -/
  progName : String
  /-- The parser to evaluate. -/
  parser : Parser α
  /-- Optional header shown above usage information. -/
  header? : Option String := none
  /-- Optional description describing the program. -/
  progDesc? : Option String := none
  /-- Optional footer appended to help output. -/
  footer? : Option String := none

namespace ParserInfo

/-- A transformation applied to a `ParserInfo`. -/
abbrev InfoMod (α : Type u) := ParserInfo α → ParserInfo α

/-- Override the stored program name. -/
def withProgName (name : String) (info : ParserInfo α) : ParserInfo α :=
  { info with progName := name }

/-- Attach a header line to help output. -/
def withHeader (text : String) (info : ParserInfo α) : ParserInfo α :=
  { info with header? := some text }

/-- Attach a descriptive paragraph to help output. -/
def withProgDesc (text : String) (info : ParserInfo α) : ParserInfo α :=
  { info with progDesc? := some text }

/-- Attach a footer to help output. -/
def withFooter (text : String) (info : ParserInfo α) : ParserInfo α :=
  { info with footer? := some text }

/-- Apply a sequence of metadata modifiers. -/
def applyMods (mods : List (InfoMod α)) (info : ParserInfo α) : ParserInfo α :=
  mods.foldl (fun acc f => f acc) info

/-- Construct a `ParserInfo` from a parser and modifiers. -/
def build (parser : Parser α) (mods : List (InfoMod α)) : ParserInfo α :=
  applyMods mods { progName := "", parser }

private def renderOptionNames (long? : Option String) (short? : Option Char) : String :=
  let parts :=
    (match short? with
     | some c => ["-" ++ String.mk [c]]
     | none => []) ++
    (match long? with
     | some s => ["--" ++ s]
     | none => [])
  parts.foldl (fun acc part => if acc.isEmpty then part else acc ++ ", " ++ part) ""

private def synopsisElement (o : OptionDoc) : String :=
  let name := renderOptionNames o.long? o.short?
  let base :=
    match o.metavar? with
    | some mv => if name.isEmpty then mv else name ++ " " ++ mv
    | none => name
  if base.isEmpty then ""
  else if o.required then base else "[" ++ base ++ "]"

private def positionalElement (p : PositionalDoc) : String :=
  if p.required then p.metavar else "[" ++ p.metavar ++ "]"

private def synopsis (u : Usage) : String :=
  let optionParts := u.options.map synopsisElement |>.filter (· ≠ "")
  let positionalParts := u.positionals.map positionalElement
  String.intercalate " " (optionParts ++ positionalParts)

private def formatOptionHeading (o : OptionDoc) : String :=
  let base := renderOptionNames o.long? o.short?
  match o.metavar? with
  | some mv => if base.isEmpty then mv else base ++ " " ++ mv
  | none => base

private def spaces (n : Nat) : String := String.mk (List.replicate n ' ')

private def indentLines (lines : List String) (indent : Nat) : List String :=
  let pad := spaces indent
  lines.map fun line =>
    if line.isEmpty then line else pad ++ line

private def renderSection (title : String) (rows : List (String × Option String)) : List String :=
  if rows.isEmpty then []
  else
    let leftWidth := rows.foldl (fun acc (name, _) => max acc name.length) 0
    let formatted := rows.map (fun (name, help?) =>
      let pad := leftWidth - name.length
      let padding := spaces pad
      match help? with
      | some text => s!"  {name}{padding}  {text}"
      | none => s!"  {name}{padding}"
    )
    (title :: formatted)

private def synopsisWithPrefix (pref : String) (u : Usage) : String :=
  let synopsisStr := synopsis u
  if synopsisStr.isEmpty then pref else s!"{pref} {synopsisStr}"

private def renderCommandSummary (commands : List CommandDoc) : List (String × Option String) :=
  commands.map fun cmd => (cmd.name, cmd.description?)

private partial def renderCommandDetailsAux (info : ParserInfo α) (indent : Nat) : List CommandDoc → List String
  | [] => []
  | cmd :: rest =>
    let header := indentLines [s!"Command: {cmd.name}"] indent
    let desc := indentLines (cmd.description?.toList) (indent + 2)
    let usagePrefix :=
      let prog := if info.progName.isEmpty then cmd.name else s!"{info.progName} {cmd.name}"
      synopsisWithPrefix (s!"Usage: {prog}") cmd.usage
    let usage := indentLines [usagePrefix] (indent + 2)
    let optionRows := cmd.usage.options.map fun opt => (formatOptionHeading opt, opt.help?)
    let positionalRows := cmd.usage.positionals.map fun pos => (pos.metavar, pos.help?)
    let commandRows := renderCommandSummary cmd.usage.commands
    let optionsSection := indentLines (renderSection "Options:" optionRows) (indent + 2)
    let argumentsSection := indentLines (renderSection "Arguments:" positionalRows) (indent + 2)
    let commandsSection := indentLines (renderSection "Commands:" commandRows) (indent + 2)
    let nested := renderCommandDetailsAux info (indent + 4) cmd.usage.commands
    let current :=
      let segments := [header, desc, usage, optionsSection, argumentsSection, commandsSection]
      List.intercalate [""] segments ++ (if nested.isEmpty then [] else [""] ++ nested)
    match renderCommandDetailsAux info indent rest with
    | [] => current
    | tail => current ++ [""] ++ tail

private def renderCommandDetails (info : ParserInfo α) (commands : List CommandDoc) : List String :=
  let details := renderCommandDetailsAux info 2 commands
  if details.isEmpty then []
  else ["Command Details:"] ++ details

/-- Render a help message for the given parser metadata. -/
def renderHelp (info : ParserInfo α) (includeHelpOption : Bool := true) : String :=
  let usage := info.parser.usage
  let usageLine :=
    let synopsisStr := synopsis usage
    if synopsisStr.isEmpty then s!"Usage: {info.progName}" else s!"Usage: {info.progName} {synopsisStr}"
  let header := info.header?.toList
  let desc := info.progDesc?.toList
  let optionsDocs :=
    let opts := usage.options
    let extra :=
      if includeHelpOption then
        [{ long? := some "help", short? := some 'h', help? := some "Show this help message", required := false : OptionDoc }]
      else []
    let all := extra ++ opts
    renderSection "Options:" (all.map (fun opt => (formatOptionHeading opt, opt.help?)))
  let argsDocs := renderSection "Arguments:" (usage.positionals.map fun arg => (arg.metavar, arg.help?))
  let commandsDocs :=
    if usage.commands.isEmpty then []
    else
      let rows := usage.commands.map fun cmd => (cmd.name, cmd.description?)
      renderSection "Commands:" rows
  let commandDetails := renderCommandDetails info usage.commands
  String.intercalate "\n" <|
    header ++ [usageLine] ++ desc ++ optionsDocs ++ argsDocs ++ commandsDocs ++ commandDetails ++ info.footer?.toList

private def containsHelp (args : List String) : Bool :=
  args.any fun arg => arg = "--help" || arg = "-h"

/-- Evaluate a parser against the provided command-line arguments. -/
def exec (info : ParserInfo α) (argv : List String) : ParserResult α :=
  if containsHelp argv then
    ParserResult.showHelp
  else
    let init := ParseState.ofList argv
    match info.parser.run init with
    | .error err =>
      ParserResult.failure { error := err, usage := info.parser.usage }
    | .ok (value, state) =>
      match state.remaining with
      | [] => ParserResult.success value
      | leftovers =>
        let message :=
          s!"Unrecognized arguments: {String.intercalate " " leftovers}"
        let err := {
          kind := .unexpected,
          message,
          context? := none
        }
        ParserResult.failure { error := err, usage := info.parser.usage, leftovers }

/-- Format a parse failure along with usage information. -/
def renderFailure (info : ParserInfo α) (failure : ParserFailure) : String :=
  let ctx := failure.error.context?.map (fun c => s!"[{c}] ") |>.getD ""
  let primary := s!"{ctx}{failure.error.message}"
  let notes := failure.error.notes.map (fun n => s!"  note: {n}")
  let details :=
    if failure.leftovers.isEmpty then []
    else [s!"  leftovers: {String.intercalate " " failure.leftovers}"]
  String.intercalate "\n" <|
    primary :: (notes ++ details ++ [renderHelp info false])

end ParserInfo

end Argparse

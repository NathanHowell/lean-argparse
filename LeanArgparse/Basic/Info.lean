import Std
import LeanArgparse.Basic.Parser
import LeanArgparse.Basic.Docs
import LeanArgparse.Basic.ParseState

namespace LeanArgparse

open Std
open Usage

structure ParserFailure where
  error : ParseError
  usage : Usage
  leftovers : List String := []
  deriving Repr

inductive ParserResult (α : Type u)
  | success (value : α)
  | showHelp
  | failure (err : ParserFailure)
  deriving Repr

structure ParserInfo (α : Type u) where
  progName : String
  parser : Parser α
  header? : Option String := none
  progDesc? : Option String := none
  footer? : Option String := none

namespace ParserInfo

abbrev InfoMod (α : Type u) := ParserInfo α → ParserInfo α

def withProgName (name : String) (info : ParserInfo α) : ParserInfo α :=
  { info with progName := name }

def withHeader (text : String) (info : ParserInfo α) : ParserInfo α :=
  { info with header? := some text }

def withProgDesc (text : String) (info : ParserInfo α) : ParserInfo α :=
  { info with progDesc? := some text }

def withFooter (text : String) (info : ParserInfo α) : ParserInfo α :=
  { info with footer? := some text }

def applyMods (mods : List (InfoMod α)) (info : ParserInfo α) : ParserInfo α :=
  mods.foldl (fun acc f => f acc) info

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
  String.intercalate "\n" <|
    header ++ [usageLine] ++ desc ++ optionsDocs ++ argsDocs ++ commandsDocs ++ info.footer?.toList

private def containsHelp (args : List String) : Bool :=
  args.any fun arg => arg = "--help" || arg = "-h"

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

end LeanArgparse

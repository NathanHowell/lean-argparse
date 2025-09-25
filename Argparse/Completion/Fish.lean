import Argparse.Completion.Core

namespace Argparse
namespace Completion
namespace Fish

open Argparse Completion Std

private def concatMap {α β} (xs : List α) (f : α → List β) : List β :=
  xs.foldr (fun x acc => f x ++ acc) []

private def escape (s : String) : String := escapeSingleQuotesForShell s

private def renderOption (prog : String) (condition : String) (opt : OptionEntry) : String :=
  let base := s!"complete -c {prog}"
  let base := if condition.isEmpty then base else s!"{base} -n '{condition}'"
  let base := match opt.long? with
    | some long => s!"{base} -l {long}"
    | none => base
  let base := match opt.short? with
    | some c => s!"{base} -s {c}"
    | none => base
  let base := if opt.takesValue then s!"{base} -r" else base
  match opt.help? with
  | some desc => s!"{base} -d '{escape desc}'"
  | none => base

private def renderCommand (prog : String) (condition : String) (cmd : CommandEntry) : String :=
  let base := s!"complete -c {prog} -f"
  let base := if condition.isEmpty then base else s!"{base} -n '{condition}'"
  let base := s!"{base} -a '{escape cmd.name}'"
  match cmd.description? with
  | some desc => s!"{base} -d '{escape desc}'"
  | none => base

private partial def collectCommands (entries : List CommandEntry) (pathPrefix : List String := []) : List (List String × CommandEntry) :=
  concatMap entries fun entry =>
    let path := pathPrefix ++ [entry.name]
    (path, entry) :: collectCommands entry.subcommands path

private def fishCondition (path : List String) : String :=
  match path with
  | [] => "__fish_use_subcommand"
  | names =>
      String.intercalate "; and " (names.map (fun name => s!"__fish_seen_subcommand_from {name}"))

private def renderData (d : Data) : String :=
  let rootCondition := fishCondition []
  let optionLines := d.options.map (renderOption d.progName rootCondition)
  let commandLines := d.commands.map (renderCommand d.progName rootCondition)
  let commandEntries := collectCommands d.commands
  let nestedLines := concatMap commandEntries fun (path, cmd) =>
    let condition := fishCondition path
    let optionLines := cmd.options.map (renderOption d.progName condition)
    let subcommands := cmd.subcommands.map (renderCommand d.progName condition)
    optionLines ++ subcommands
  String.intercalate "\n" (optionLines ++ commandLines ++ nestedLines)

def module : Module := {
  name := "fish",
  render := renderData
}

def render (info : ParserInfo α) : String :=
  renderWithModule module info

end Fish
end Completion
end Argparse

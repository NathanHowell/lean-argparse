import Argparse.Completion.Core

namespace Argparse
namespace Completion
namespace Zsh

open Argparse Completion Std

private def concatMap {α β} (xs : List α) (f : α → List β) : List β :=
  xs.foldr (fun x acc => f x ++ acc) []

private def formatOption (opt : OptionEntry) (flag : String) : String :=
  let desc := opt.help?.map escapeSingleQuotesForShell |>.getD ""
  let descPart := if desc.isEmpty then "" else s!"[{desc}]"
  let metavar := opt.metavar?.map escapeSingleQuotesForShell |>.getD "VALUE"
  let argPart := if opt.takesValue then s!":{metavar}:" else ""
  s!"'{flag}{descPart}{argPart}'"

private def collectOptionSpecs (opts : List OptionEntry) : List String :=
  concatMap opts (fun opt => optionFlagsFor opt |>.map (formatOption opt))

private def commandValue (cmd : CommandEntry) : String :=
  let desc := cmd.description?.map escapeSingleQuotesForShell |>.getD ""
  if desc.isEmpty then s!"{cmd.name}" else s!"{cmd.name}:{desc}"

private partial def collectCommands (entries : List CommandEntry) (pathPrefix : List String := []) : List (List String × CommandEntry) :=
  concatMap entries fun entry =>
    let path := pathPrefix ++ [entry.name]
    (path, entry) :: collectCommands entry.subcommands path

private def commandStateName (path : List String) : String :=
  "command_" ++ String.intercalate "_" path

private def commandSelectState (path : List String) : String :=
  "command_select_" ++ String.intercalate "_" path

private def renderCommandClause (path : List String) (cmd : CommandEntry) : List String :=
  let state := commandStateName path
  let optionSpecs := collectOptionSpecs cmd.options
  let subState := commandSelectState path
  let baseArgs :=
    if cmd.subcommands.isEmpty then optionSpecs ++ ["'*::arg:->args'"]
    else optionSpecs ++ [s!"'1:command:->{subState}'", "'*::arg:->args'"]
  [s!"  {state})",
   s!"    _arguments -s {String.intercalate " " baseArgs}",
   "  ;;"]

private def renderCommandSelectClause (path : List String) (cmd : CommandEntry) : List String :=
  if cmd.subcommands.isEmpty then []
  else
    let state := commandSelectState path
    let values := cmd.subcommands.map commandValue
    [s!"  {state})",
     s!"    _values 'commands' {String.intercalate " " values}",
     "  ;;"]

private def renderData (d : Data) : String :=
  let rootOptions := collectOptionSpecs d.options
  let baseArgs :=
    if d.commands.isEmpty then rootOptions ++ ["'*::arg:->args'"]
    else rootOptions ++ ["'1:command:->command'", "'*::arg:->args'"]
  let baseLines : List String :=
    [s!"#compdef {d.progName}",
     "local context state state_descr line",
     "typeset -A opt_args",
     s!"_arguments -s {String.intercalate " " baseArgs}"]
  let commandEntries := collectCommands d.commands
  let commandClauses := concatMap commandEntries fun (path, cmd) =>
    renderCommandClause path cmd ++ renderCommandSelectClause path cmd
  let rootSelect :=
    if d.commands.isEmpty then []
    else
      let values := d.commands.map commandValue
      ["  command)",
       s!"    _values 'commands' {String.intercalate " " values}",
       "  ;;"]
  let caseLines :=
    if d.commands.isEmpty then []
    else
      ["case $state in"] ++ rootSelect ++ commandClauses ++ ["esac"]
  String.intercalate "\n" (baseLines ++ caseLines)

def module : Module := {
  name := "zsh",
  render := renderData
}

def render (info : ParserInfo α) : String :=
  renderWithModule module info

end Zsh
end Completion
end Argparse

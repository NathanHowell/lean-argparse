import LeanArgparse.Completion.Core

namespace LeanArgparse
namespace Completion
namespace Zsh

open LeanArgparse Completion

private def formatOption (opt : OptionEntry) (flag : String) : String :=
  let desc := opt.help?.map escapeSingleQuotesForShell |>.getD ""
  let descPart := if desc.isEmpty then "" else s!"[{desc}]"
  let metavar := opt.metavar?.map escapeSingleQuotesForShell |>.getD "VALUE"
  let argPart := if opt.takesValue then s!":{metavar}:" else ""
  s!"'{flag}{descPart}{argPart}'"

private def renderData (d : Data) : String :=
  let rec collectOptions : List OptionEntry → List String
    | [] => []
    | opt :: rest =>
        (optionFlagsFor opt |>.map (formatOption opt)) ++ collectOptions rest
  let optionEntries := collectOptions d.options
  let argumentLine :=
    String.intercalate " " (optionEntries ++ ["'*::arg:->args'"])
  let baseLines : List String :=
    [s!"#compdef {d.progName}",
     "local context state state_descr line",
     "typeset -A opt_args",
     s!"_arguments -s {argumentLine}"]
  let commandLines :=
    if d.commands.isEmpty then []
    else
      let entries := d.commands.map fun (name, desc?) =>
        let desc := desc?.map escapeSingleQuotesForShell |>.getD ""
        if desc.isEmpty then s!"{name}" else s!"{name}:{desc}"
      ["case $state in",
       "  args)",
       s!"    _values 'commands' {String.intercalate " " entries}",
       "  ;;",
       "esac"]
  String.intercalate "\n" (baseLines ++ commandLines)

def module : Module := {
  name := "zsh",
  render := renderData
}

def render (info : ParserInfo α) : String :=
  renderWithModule module info

end Zsh
end Completion
end LeanArgparse

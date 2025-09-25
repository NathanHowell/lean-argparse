import Argparse.Completion.Core

namespace Argparse
namespace Completion
namespace Fish

open Argparse Completion

private def renderOption (prog : String) (opt : OptionEntry) : String :=
  let base := s!"complete -c {prog}"
  let base := match opt.long? with
    | some long => s!"{base} -l {long}"
    | none => base
  let base := match opt.short? with
    | some c => s!"{base} -s {c}"
    | none => base
  let base := if opt.takesValue then s!"{base} -r" else base
  match opt.help? with
  | some desc => s!"{base} -d '{escapeSingleQuotesForShell desc}'"
  | none => base

private def renderData (d : Data) : String :=
  let optionLines := d.options.map (renderOption d.progName)
  let commandLines := d.commands.map fun (name, desc?) =>
    let descPart := desc?.map escapeSingleQuotesForShell |>.map (fun s => s!" -d '{s}'") |>.getD ""
    s!"complete -c {d.progName} -n '__fish_use_subcommand' -f -a '{escapeSingleQuotesForShell name}'{descPart}"
  String.intercalate "\n" (optionLines ++ commandLines)

def module : Module := {
  name := "fish",
  render := renderData
}

def render (info : ParserInfo α) : String :=
  renderWithModule module info

end Fish
end Completion
end Argparse

import Argparse.Completion.Core

namespace Argparse
namespace Completion
namespace Bash

open Argparse Completion

private def joinWords (ws : List String) : String := String.intercalate " " ws

private def renderData (d : Data) : String :=
  let ident := sanitizeProgramName d.progName
  let functionName := s!"_{ident}_completion"
  let longOpts := d.options.filterMap optionLongFlag
  let shortOpts := d.options.filterMap optionShortFlag
  let commands := d.commands.map Prod.fst
  let valuedLong := d.options.filterMap (fun opt => if opt.takesValue then optionLongFlag opt else none)
  let valuedShort := d.options.filterMap (fun opt => if opt.takesValue then optionShortFlag opt else none)
  let longWords := joinWords longOpts
  let shortWords := joinWords shortOpts
  let commandWords := joinWords commands
  let valuedWords := joinWords (valuedLong ++ valuedShort)
  let combinedWords := joinWords ((longOpts ++ shortOpts ++ commands).eraseDups)
  let startLine := functionName ++ "() {"
  String.intercalate "\n"
    [startLine,
     "  local cur prev",
     "  COMPREPLY=()",
     "  cur=\"${COMP_WORDS[COMP_CWORD]}\"",
     "  prev=\"${COMP_WORDS[COMP_CWORD-1]}\"",
     s!"  local opts_long=\"{longWords}\"",
     s!"  local opts_short=\"{shortWords}\"",
     s!"  local commands=\"{commandWords}\"",
     s!"  local all_words=\"{combinedWords}\"",
     s!"  local value_opts=\"{valuedWords}\"",
     "  for opt in $value_opts; do",
     "    if [[ $prev == $opt ]]; then",
     "      return 0",
     "    fi",
     "  done",
     "  if [[ $cur == --* ]]; then",
     "    COMPREPLY=( $(compgen -W \"$opts_long\" -- \"$cur\") )",
     "    return 0",
     "  fi",
     "  if [[ $cur == -* ]]; then",
     "    COMPREPLY=( $(compgen -W \"$opts_short\" -- \"$cur\") )",
     "    return 0",
     "  fi",
     "  COMPREPLY=( $(compgen -W \"$all_words\" -- \"$cur\") )",
     "  return 0",
     "}",
     s!"complete -F {functionName} {d.progName}"]

def module : Module := {
  name := "bash",
  render := renderData
}

def render (info : ParserInfo α) : String :=
  renderWithModule module info

end Bash
end Completion
end Argparse

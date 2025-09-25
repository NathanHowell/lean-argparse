import Argparse.Completion.Core

namespace Argparse
namespace Completion
namespace Zsh

open Argparse Completion Std

private def concatMap {α β} (xs : List α) (f : α → List β) : List β :=
  xs.foldr (fun x acc => f x ++ acc) []

private def optionFlags (opt : OptionEntry) : List String :=
  (optionShortFlag opt).toList ++ (optionLongFlag opt).toList

private def groupPart (flags : List String) : Option String :=
  if flags.length ≤ 1 then
    none
  else
    some ("'(" ++ String.intercalate " " flags ++ ")'")

private def descriptionPart (help? : Option String) : String :=
  match help? with
  | some desc =>
      let escaped := escapeSingleQuotesForShell desc
      s!"[{escaped}]"
  | none => ""

private def argumentPart (opt : OptionEntry) : String :=
  if opt.takesValue then
    let metavar := opt.metavar?.map escapeSingleQuotesForShell |>.getD "VALUE"
    s!":{metavar}:"
  else
    ""

private def formatOptionSpec (opt : OptionEntry) : String :=
  let flags := optionFlags opt
  let group := groupPart flags |>.getD ""
  let desc := descriptionPart opt.help?
  let arg := argumentPart opt
  match flags with
  | [] =>
      let body := "'--" ++ desc ++ arg ++ "'"
      group ++ body
  | [flag] =>
      let body := "'" ++ flag ++ desc ++ arg ++ "'"
      group ++ body
  | _ =>
      let names := "{" ++ String.intercalate "," flags ++ "}"
      let tail :=
        let content := desc ++ arg
        if content.isEmpty then ""
        else "'" ++ content ++ "'"
      group ++ names ++ tail

private def commandValue (cmd : CommandEntry) : String :=
  let name := escapeSingleQuotesForShell cmd.name
  let desc := cmd.description?.map escapeSingleQuotesForShell |>.getD ""
  let payload := if desc.isEmpty then name else s!"{name}:{desc}"
  s!"'{payload}'"

private def renderArgumentsBlock (indent : String) (flags : String) (specs : List String) : List String :=
  if specs.isEmpty then
    [indent ++ "_arguments " ++ flags, indent ++ "&& ret=0"]
  else
    let cont : String := "\\"
    let header := indent ++ "_arguments " ++ flags ++ " " ++ cont
    let body := specs.map fun spec => indent ++ "  " ++ spec ++ " " ++ cont
    let footer := indent ++ "  && ret=0"
    header :: body ++ [footer]

private def renderCommandsBlock (indent label : String) (cmds : List CommandEntry) : List String :=
  if cmds.isEmpty then [] else
    let entries := cmds.map (commandValue)
    let header := indent ++ "local -a commands"
    let openParen := indent ++ "commands=("
    let body := entries.map fun entry => indent ++ "  " ++ entry
    let closeParen := indent ++ ")"
    let describe := indent ++ s!"_describe '{label}' commands && ret=0"
    [header, openParen] ++ body ++ [closeParen, describe]

private def positionalSpecs (positionals : List PositionalDoc) : List String :=
  ((List.range positionals.length).zip positionals).map fun ⟨idx, pos⟩ =>
    let number := toString (idx + 1)
    let metavar := escapeSingleQuotesForShell pos.metavar
    let desc := pos.help?.map escapeSingleQuotesForShell |>.getD ""
    let descPart := if desc.isEmpty then "" else s!"[{desc}]"
    s!"'{number}:{metavar}{descPart}'"

private def commandStateName (path : List String) : String :=
  match path with
  | [] => "command"
  | parts => "command_" ++ String.intercalate "_" parts

private def commandSelectState (path : List String) : String :=
  match path with
  | [] => "command_select"
  | parts => "command_select_" ++ String.intercalate "_" parts

private def renderCaseBlock (state : String) (body : List String) : String :=
  if body.isEmpty then ""
  else
    let lines := [s!"  {state})"] ++ body ++ ["  ;;"]
    String.intercalate "\n" lines

private partial def renderCommandCases (path : List String) (cmd : CommandEntry) : List String :=
  let state := commandStateName path
  let subState := commandSelectState path
  let optionSpecs := cmd.options.map formatOptionSpec
  let positional := positionalSpecs cmd.positionals
  let subcommandSpec :=
    if cmd.subcommands.isEmpty then []
    else [s!"'1:command:->{subState}'"]
  let specs := optionSpecs ++ subcommandSpec ++ positional ++ ["'*::arg:->args'"]
  let arguments := renderArgumentsBlock "    " "-s -S" specs
  let caseBlocks := [renderCaseBlock state arguments]
  let nestedCommands :=
    if cmd.subcommands.isEmpty then []
    else
      let describe := renderCommandsBlock "    " "command" cmd.subcommands
      [renderCaseBlock subState describe]
  caseBlocks ++ nestedCommands ++ concatMap cmd.subcommands (fun sub => renderCommandCases (path ++ [sub.name]) sub)

private def renderData (d : Data) : String :=
  let header := [
    s!"#compdef {d.progName}",
    "local curcontext=\"$curcontext\" state state_descr line ret=1",
    "typeset -A opt_args"
  ]
  let topSpecs :=
    (d.options.map formatOptionSpec) ++
    ["'1:command:->command'", "'*::arg:->args'"]
  let topArguments := renderArgumentsBlock "" "-s -S -C" topSpecs
  let topCase := renderCaseBlock "command" (renderCommandsBlock "    " "command" d.commands)
  let commandCases := concatMap d.commands (fun cmd => renderCommandCases [cmd.name] cmd)
  let caseSection :=
    if topCase.isEmpty ∧ commandCases.isEmpty then []
    else ["", "case $state in"] ++
      ([topCase] ++ commandCases |>.filter (· ≠ "")) ++
      ["esac"]
  let footer := ["", "return ret"]
  String.intercalate "\n" (header ++ topArguments ++ caseSection ++ footer)

def module : Module := {
  name := "zsh",
  render := renderData
}

def render (info : ParserInfo α) : String :=
  renderWithModule module info

end Zsh
end Completion
end Argparse

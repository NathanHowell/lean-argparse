import Std
import Argparse.Basic

namespace Argparse
namespace Manpage

open Std

structure Config where
  sectionName : String := "1"
  date? : Option String := none
  source? : Option String := none
  manual? : Option String := none

private def escape (s : String) : String :=
  let step := s.replace "\\" "\\\\"
  step.replace "\"" "\\\""

private def renderTh (info : ParserInfo α) (cfg : Config) : String :=
  let prog := escape (if info.progName.isEmpty then "program" else info.progName)
  let sectionStr := escape cfg.sectionName
  let date := escape (cfg.date?.getD "")
  let source := escape (cfg.source?.getD "")
  let manual := escape (cfg.manual?.getD "")
  s!".TH \"{prog}\" \"{sectionStr}\" \"{date}\" \"{source}\" \"{manual}\""

private def formatOptionHeading (o : OptionDoc) : String :=
  let renderNames : List String :=
    (match o.short? with | some c => ["-" ++ String.mk [c]] | none => []) ++
    (match o.long? with | some s => ["--" ++ s] | none => [])
  let base := String.intercalate ", " renderNames
  match o.metavar? with
  | some mv => if base.isEmpty then mv else base ++ " " ++ mv
  | none => base

private def synopsisElement (o : OptionDoc) : String :=
  let heading := formatOptionHeading o
  if heading.isEmpty then ""
  else if o.required then heading else s!"[{heading}]"

private def positionalElement (p : PositionalDoc) : String :=
  if p.required then p.metavar else s!"[{p.metavar}]"

private def synopsis (info : ParserInfo α) : String :=
  let usage := info.parser.usage
  let optionParts := usage.options.map synopsisElement |>.filter (· ≠ "")
  let positionalParts := usage.positionals.map positionalElement
  String.intercalate " " (optionParts ++ positionalParts)

private def synopsisWithPrefix (pref : String) (usage : Usage) : String :=
  let optionTail := usage.options.map synopsisElement |>.filter (· ≠ "")
  let positionalTail := usage.positionals.map positionalElement
  let tail := optionTail ++ positionalTail
  if tail.isEmpty then pref
  else
    let joined := String.intercalate " " tail
    s!"{pref} {joined}"

private def bold (s : String) : String := s!"\\fB{s}\\fR"

private def sectionBlock (title : String) : List String := [s!".SH {title}"]

private def concatMap {α β} (xs : List α) (f : α → List β) : List β :=
  xs.foldr (fun x acc => f x ++ acc) []

private def renderOptions (usage : Usage) : List String :=
  let entries := usage.options.map fun opt =>
    let heading := if formatOptionHeading opt = "" then "[option]" else formatOptionHeading opt
    let defaultLines := opt.default?.map (fun d => s!"(default: {d})") |>.toList
    let helpLines := opt.help?.toList
    [".TP", bold heading] ++ helpLines ++ defaultLines
  if entries.isEmpty then [] else sectionBlock "OPTIONS" ++ entries.foldr (· ++ ·) []

private def renderArguments (usage : Usage) : List String :=
  let entries := usage.positionals.map fun arg =>
    let heading := if arg.metavar.isEmpty then "ARG" else arg.metavar
    let helpLines := arg.help?.toList
    [".TP", bold heading] ++ helpLines
  if entries.isEmpty then [] else sectionBlock "ARGUMENTS" ++ entries.foldr (· ++ ·) []

private def renderCommands (usage : Usage) : List String :=
  let entries := usage.commands.map fun cmd =>
    let descLine := cmd.description?.map fun d => s!"- {d}"
    [".TP", bold cmd.name] ++ descLine.toList
  if entries.isEmpty then [] else sectionBlock "COMMANDS" ++ entries.foldr (· ++ ·) []

private def renderCommandOptions (usage : Usage) : List String :=
  concatMap usage.options fun opt =>
    let heading := if formatOptionHeading opt = "" then "[option]" else formatOptionHeading opt
    let defaultLines := opt.default?.map (fun d => s!"(default: {d})") |>.toList
    let helpLines := opt.help?.toList
    [".TP", bold heading] ++ helpLines ++ defaultLines

private def renderCommandArguments (usage : Usage) : List String :=
  concatMap usage.positionals fun arg =>
    let heading := if arg.metavar.isEmpty then "ARG" else arg.metavar
    let helpLines := arg.help?.toList
    [".TP", bold heading] ++ helpLines

private def renderCommandSummary (usage : Usage) : List String :=
  concatMap usage.commands fun cmd =>
    let descLine := cmd.description?.map fun d => s!"- {d}"
    [".TP", bold cmd.name] ++ descLine.toList

private partial def renderCommandDetails (info : ParserInfo α) (commands : List CommandDoc) (topLevel : Bool := true) : List String :=
  if commands.isEmpty then []
  else
    let rec renderDetail (cmd : CommandDoc) : List String :=
      let heading := [s!".SS {escape cmd.name}"]
      let desc := cmd.description?.map escape |>.toList
      let prog := if info.progName.isEmpty then cmd.name else s!"{info.progName} {cmd.name}"
      let synopsisLine := synopsisWithPrefix (bold (escape prog)) cmd.usage
      let options := renderCommandOptions cmd.usage
      let args := renderCommandArguments cmd.usage
      let subs := renderCommandSummary cmd.usage
      let nested := renderCommandDetails info cmd.usage.commands false
      heading ++ desc ++ [synopsisLine] ++
        (if options.isEmpty then [] else [".PP", bold "Options"] ++ options) ++
        (if args.isEmpty then [] else [".PP", bold "Arguments"] ++ args) ++
        (if subs.isEmpty then [] else [".PP", bold "Commands"] ++ subs) ++ nested
    let body := commands.map renderDetail |>.foldr (· ++ ·) []
    (if topLevel then sectionBlock "COMMAND DETAILS" else []) ++ body

private def renderFooter (info : ParserInfo α) : List String :=
  match info.footer? with
  | some text => sectionBlock "FOOTER" ++ [text]
  | none => []

private def nameSection (info : ParserInfo α) : List String :=
  let prog := if info.progName.isEmpty then "program" else info.progName
  let desc := match info.progDesc? with
    | some d => some d
    | none => info.header?
  let body :=
    match desc with
    | some text => s!"{prog} \\- {text}"
    | none => prog
  sectionBlock "NAME" ++ [body]

private def synopsisSection (info : ParserInfo α) : List String :=
  let prog := if info.progName.isEmpty then "program" else info.progName
  let synopsisStr := synopsis info
  let line :=
    if synopsisStr.isEmpty then bold prog
    else s!"{bold prog} {synopsisStr}"
  sectionBlock "SYNOPSIS" ++ [line]

private def descriptionSection (info : ParserInfo α) : List String :=
  if let some desc := info.progDesc? then
    sectionBlock "DESCRIPTION" ++ [desc]
  else []

/-- Render a troff man page for `info`. -/
def render (info : ParserInfo α) (cfg : Config := {}) : String :=
  let usage := info.parser.usage
  let parts : List String :=
    [renderTh info cfg] ++
    nameSection info ++
    synopsisSection info ++
    descriptionSection info ++
    renderOptions usage ++
    renderArguments usage ++
    renderCommands usage ++
    renderCommandDetails info usage.commands ++
    renderFooter info
  String.intercalate "\n" parts

end Manpage

namespace ParserInfo

/-- Render a troff man page given parser metadata. -/
def renderManpage (info : ParserInfo α) (cfg : Manpage.Config := {}) : String :=
  Manpage.render info cfg

end ParserInfo

end Argparse

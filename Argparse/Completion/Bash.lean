import Argparse.Completion.Core

namespace Argparse
namespace Completion
namespace Bash

open Argparse Completion Std

private def joinWords (ws : List String) : String :=
  String.intercalate " " (ws.eraseDups.filter (· ≠ ""))

private def concatMap {α β} (xs : List α) (f : α → List β) : List β :=
  xs.foldr (fun x acc => f x ++ acc) []

private def contextKey (path : List String) : String :=
  match path with
  | [] => "_root"
  | _ => String.intercalate ":" path

private def longFlags (opts : List OptionEntry) : List String :=
  opts.filterMap optionLongFlag

private def shortFlags (opts : List OptionEntry) : List String :=
  opts.filterMap optionShortFlag

private def valuedFlags (opts : List OptionEntry) : List String :=
  opts.foldl
    (fun acc opt =>
      if opt.takesValue then
        acc
          ++ (opt.long?.map (s!"--{·}") |>.toList)
          ++ (opt.short?.map (fun c => s!"-{String.mk [c]}") |>.toList)
      else
        acc)
    []

structure ContextData where
  key : String
  children : List (String × String)
  long : List String
  short : List String
  valued : List String
  deriving Inhabited

private partial def buildContexts (commands : List CommandEntry) (pathPrefix : List String := []) : List ContextData :=
  concatMap commands fun cmd =>
    let path := pathPrefix ++ [cmd.name]
    let key := contextKey path
    let childContexts := buildContexts cmd.subcommands path
    let children :=
      cmd.subcommands.map fun child =>
        let childKey := contextKey (path ++ [child.name])
        (child.name, childKey)
    [{
        key,
        children,
        long := longFlags cmd.options,
        short := shortFlags cmd.options,
        valued := valuedFlags cmd.options
      }] ++ childContexts

private def allContexts (d : Data) : List ContextData :=
  let rootChildren :=
    d.commands.map fun child => (child.name, contextKey [child.name])
  let root : ContextData := {
    key := contextKey [],
    children := rootChildren,
    long := longFlags d.options,
    short := shortFlags d.options,
    valued := valuedFlags d.options
  }
  root :: buildContexts d.commands []

private def escapeKey (key : String) : String :=
  s!"'{escapeSingleQuotesForShell key}'"

private def escapeValue (value : String) : String :=
  escapeSingleQuotesForShell value

private def renderAssoc (name : String) (entries : List (String × String)) : List String :=
  let header := s!"  local -A {name}=("
  let body := entries.map fun (k, v) => s!"    [{escapeKey k}]='{escapeValue v}'"
  header :: (body ++ ["  )"])

private def renderData (d : Data) : String :=
  let ident := sanitizeProgramName d.progName
  let functionName := s!"_{ident}_completion"
  let contexts := allContexts d
  let commandNames := contexts.map fun ctx =>
    (ctx.key, joinWords (ctx.children.map (·.fst)))
  let childPairs := contexts.map fun ctx =>
    (ctx.key, joinWords (ctx.children.map fun (name, key) => s!"{name}={key}"))
  let longEntries := contexts.map fun ctx => (ctx.key, joinWords ctx.long)
  let shortEntries := contexts.map fun ctx => (ctx.key, joinWords ctx.short)
  let valuedEntries := contexts.map fun ctx => (ctx.key, joinWords ctx.valued)
  let longMapName := s!"_{ident}_long_opts"
  let shortMapName := s!"_{ident}_short_opts"
  let valuedMapName := s!"_{ident}_valued_opts"
  let commandMapName := s!"_{ident}_commands"
  let childMapName := s!"_{ident}_child_keys"
  let pairsLine := "    local pairs=\"${" ++ childMapName ++ "[$command]}\""
  let waitingLine := "  local waiting=\"${" ++ valuedMapName ++ "[$command]}\""
  let longLine := "  local long=\"${" ++ longMapName ++ "[$command]}\""
  let shortLine := "  local short=\"${" ++ shortMapName ++ "[$command]}\""
  let commandsLine := "  local commands=\"${" ++ commandMapName ++ "[$command]}\""
  let headerStart := functionName ++ "() {"
  let headerLines : List String :=
    headerStart ::
    "  local cur prev command" ::
    "  COMPREPLY=()" ::
    "  cur=\"${COMP_WORDS[COMP_CWORD]}\"" ::
    "  prev=\"${COMP_WORDS[COMP_CWORD-1]:-}\"" ::
    "  command='_root'" ::
    []
  let assocLines :=
    renderAssoc longMapName longEntries ++
    renderAssoc shortMapName shortEntries ++
    renderAssoc valuedMapName valuedEntries ++
    renderAssoc commandMapName commandNames ++
    renderAssoc childMapName childPairs
  let loopLines : List String :=
    "  for ((i=1; i<COMP_CWORD; i++)); do" ::
    "    local word=\"${COMP_WORDS[i]}\"" ::
    pairsLine ::
    "    for pair in $pairs; do" ::
    "      local child=${pair%%=*}" ::
    "      local next=${pair#*=}" ::
    "      if [[ $word == $child ]]; then" ::
    "        command=$next" ::
    "        break" ::
    "      fi" ::
    "    done" ::
    "  done" ::
    waitingLine ::
    "  for opt in $waiting; do" ::
    "    if [[ $prev == $opt ]]; then" ::
    "      return 0" ::
    "    fi" ::
    "  done" ::
    longLine ::
    shortLine ::
    commandsLine ::
    "  if [[ $cur == --* ]]; then" ::
    "    COMPREPLY=( $(compgen -W \"$long\" -- \"$cur\") )" ::
    "    return 0" ::
    "  fi" ::
    "  if [[ $cur == -* ]]; then" ::
    "    COMPREPLY=( $(compgen -W \"$short\" -- \"$cur\") )" ::
    "    return 0" ::
    "  fi" ::
    "  local words=\"$long $short $commands\"" ::
    "  COMPREPLY=( $(compgen -W \"$words\" -- \"$cur\") )" ::
    "  return 0" ::
    "}" ::
    []
  let lines := headerLines ++ assocLines ++ loopLines ++ [s!"complete -F {functionName} {d.progName}"]
  String.intercalate "\n" lines

def module : Module := {
  name := "bash",
  render := renderData
}

def render (info : ParserInfo α) : String :=
  renderWithModule module info

end Bash
end Completion
end Argparse

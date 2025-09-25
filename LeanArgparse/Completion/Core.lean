import LeanArgparse.Basic

namespace LeanArgparse
namespace Completion

open Std LeanArgparse

structure OptionEntry where
  long? : Option String := none
  short? : Option Char := none
  metavar? : Option String := none
  takesValue : Bool := false
  help? : Option String := none
  deriving Repr

structure Data where
  progName : String
  options : List OptionEntry := []
  commands : List (String × Option String) := []
  deriving Repr

structure Module where
  name : String
  render : Data → String

structure OptionKey where
  long? : Option String
  short? : Option Char
  takesValue : Bool
  deriving BEq, Hashable

private def fromOptionDoc (doc : OptionDoc) : OptionEntry :=
  {
    long? := doc.long?,
    short? := doc.short?,
    metavar? := doc.metavar?,
    takesValue := doc.metavar?.isSome,
    help? := doc.help?
  }

private def usageOptions (usage : Usage) : List OptionEntry :=
  let docs := usage.options
  let init : Array OptionEntry × HashSet OptionKey :=
    (Array.mkEmpty docs.length, HashSet.emptyWithCapacity docs.length)
  let (acc, _) := docs.foldl
    (fun (acc, seen) doc =>
      let opt := fromOptionDoc doc
      let key : OptionKey := {
        long? := opt.long?,
        short? := opt.short?,
        takesValue := opt.takesValue
      }
      if seen.contains key then
        (acc, seen)
      else
        (acc.push opt, seen.insert key))
    init
  acc.toList

private def usageCommands (usage : Usage) : List (String × Option String) :=
  let cmds := usage.commands
  let init : Array (String × Option String) × HashSet String :=
    (Array.mkEmpty cmds.length, HashSet.emptyWithCapacity cmds.length)
  let (acc, _) := cmds.foldl
    (fun (acc, seen) cmd =>
      if seen.contains cmd.name then
        (acc, seen)
      else
        (acc.push (cmd.name, cmd.description?), seen.insert cmd.name))
    init
  acc.toList

private def escapeSingleQuotes (s : String) : String :=
  s.replace "'" "'\\''"

private def sanitizeIdentifier (s : String) : String :=
  let chars := s.data.map fun c => if c.isAlphanum || c = '_' then c else '_'
  let result := String.mk chars
  if result.isEmpty then "program" else result

private def ensureHelpOption (opts : List OptionEntry) : List OptionEntry :=
  if opts.any (fun opt => opt.long? = some "help" || opt.short? = some 'h') then
    opts
  else
    opts ++ [{
      long? := some "help",
      short? := some 'h',
      metavar? := none,
      takesValue := false,
      help? := some "Show this help message"
    }]

private def buildOptions (usage : Usage) : List OptionEntry :=
  ensureHelpOption (usageOptions usage)

private def buildCommands (usage : Usage) : List (String × Option String) :=
  usageCommands usage

private def optionLong (opt : OptionEntry) : Option String :=
  opt.long?.map fun n => s!"--{n}"

private def optionShort (opt : OptionEntry) : Option String :=
  opt.short?.map fun c => s!"-{String.mk [c]}"

private def optionFlags (opt : OptionEntry) : List String :=
  let longs := match optionLong opt with
    | some flag => [flag]
    | none => []
  let shorts := match optionShort opt with
    | some flag => [flag]
    | none => []
  longs ++ shorts

private def buildDataCore (info : ParserInfo α) : Data :=
  let usage := info.parser.usage
  {
    progName := if info.progName.isEmpty then "program" else info.progName,
    options := buildOptions usage,
    commands := buildCommands usage
  }

def renderWithModule (mod : Module) (info : ParserInfo α) : String :=
  mod.render (buildDataCore info)

abbrev sanitizeProgramName := sanitizeIdentifier
abbrev escapeSingleQuotesForShell := escapeSingleQuotes
abbrev optionFlagsFor := optionFlags
abbrev optionLongFlag := optionLong
abbrev optionShortFlag := optionShort

end Completion
end LeanArgparse

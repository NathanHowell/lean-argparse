import Argparse.Basic

namespace Argparse
namespace Completion

open Std Argparse
open Argparse.OptionSpec

/-- Lightweight representation of an option for completion generation. -/
structure OptionEntry where
  /-- Long flag name (e.g. `--count`). -/
  long? : Option String := none
  /-- Short flag name (e.g. `-c`). -/
  short? : Option Char := none
  /-- Placeholder shown when the option expects a value. -/
  metavar? : Option String := none
  /-- Whether the option consumes a value. -/
  takesValue : Bool := false
  /-- Help text attached to the option. -/
  help? : Option String := none
  deriving Repr, Inhabited

/-- Completion metadata for a command with its options and subcommands. -/
structure CommandEntry where
  /-- Command name used on the command line. -/
  name : String
  /-- Optional description displayed in help listings. -/
  description? : Option String := none
  /-- Options available within this command. -/
  options : List OptionEntry := []
  /-- Nested subcommands. -/
  subcommands : List CommandEntry := []
  /-- Positional arguments accepted by the command. -/
  positionals : List PositionalDoc := []
  deriving Repr, Inhabited

/-- Gathered completion metadata for an entire program. -/
structure Data where
  /-- Program name the completion script targets. -/
  progName : String
  /-- Top-level options available for the program. -/
  options : List OptionEntry := []
  /-- Top-level commands available for the program. -/
  commands : List CommandEntry := []
  deriving Repr

/-- Backend capable of rendering a completion script. -/
structure Module where
  /-- Identifier for the completion backend. -/
  name : String
  /-- Function that renders a completion script given collected metadata. -/
  render : Data → String

/-- Key used to deduplicate options when collecting metadata. -/
structure OptionKey where
  /-- Optional long flag name. -/
  long? : Option String
  /-- Optional short flag name. -/
  short? : Option Char
  /-- Whether the option expects an argument. -/
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

private partial def usageCommands (usage : Usage) : List CommandEntry :=
  let rec buildEntry (cmd : CommandDoc) : CommandEntry :=
    {
      name := cmd.name,
      description? := cmd.description?,
      options := ensureHelpOption (usageOptions cmd.usage),
      subcommands := usageCommands cmd.usage,
      positionals := cmd.usage.positionals
    }
  let docs := usage.commands
  let init : Array CommandEntry × HashSet String :=
    (Array.mkEmpty docs.length, HashSet.emptyWithCapacity docs.length)
  let (acc, _) := docs.foldl
    (fun (acc, seen) doc =>
      if seen.contains doc.name then
        (acc, seen)
      else
        (acc.push (buildEntry doc), seen.insert doc.name))
    init
  acc.toList

private def escapeSingleQuotes (s : String) : String :=
  s.replace "'" "'\\''"

private def sanitizeIdentifier (s : String) : String :=
  let chars := s.data.map fun c => if c.isAlphanum || c = '_' then c else '_'
  let result := String.mk chars
  if result.isEmpty then "program" else result

private def buildOptions (usage : Usage) : List OptionEntry :=
  ensureHelpOption (usageOptions usage)

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
    commands := usageCommands usage
  }

/-- Shells that have built-in completion support. -/
inductive Shell where
  /-- GNU bash. -/
  | bash
  /-- Z shell. -/
  | zsh
  /-- fish shell. -/
  | fish
  deriving DecidableEq, Repr, Inhabited

namespace Shell

/-- Normalize a shell name for case-insensitive comparison. -/
def normalize (name : String) : String :=
  let lowered := name.data.map Char.toLower
  String.mk lowered

/-- Mapping from normalized shell names to constructors. -/
def table : List (String × Shell) :=
  [ ("bash", .bash),
    ("zsh", .zsh),
    ("fish", .fish) ]

/-- All supported shells. -/
def all : List Shell := table.map (·.snd)

/-- List of shell names as strings. -/
def names : List String := table.map (·.fst)

/-- Comma-separated list of supported shells. -/
def namesList : String := String.intercalate ", " names

/-- Human-readable name for a shell constructor. -/
def name : Shell → String
  | .bash => "bash"
  | .zsh => "zsh"
  | .fish => "fish"

/-- Parse a shell identifier from user input. -/
def ofString? (name : String) : Option Shell :=
  let key := normalize name
  (table.find? (fun entry => entry.fst = key)).map (·.snd)

/-- Reader for the built-in completion shell enumeration. -/
def reader : ValueReader Shell :=
  ⟨fun raw =>
    match ofString? raw with
    | some shell => .ok shell
    | none => .error s!"unknown completion shell '{raw}'. expected one of: {namesList}"⟩

end Shell

/-- Specification for the standard `--completions` option. -/
def defaultShellOptionSpec : OptionSpec Shell :=
  OptionSpec.build Shell.reader [
    long "completions",
    setMetavar "SHELL",
    help s!"Generate a completion script for one of: {Shell.namesList}"
  ]

/-- Parser for the mandatory `--completions` option. -/
def defaultShellOption : Parser Shell :=
  option defaultShellOptionSpec

/-- Parser for an optional `--completions` option. -/
def defaultOptionalShellOption : Parser (Option Shell) :=
  Parser.optional defaultShellOption

/-- Render completion output using a given backend module. -/
def renderWithModule (mod : Module) (info : ParserInfo α) : String :=
  mod.render (buildDataCore info)

/-- Sanitise a program name for inclusion in shell functions. -/
abbrev sanitizeProgramName := sanitizeIdentifier
/-- Escape single quotes for safe inclusion in shell scripts. -/
abbrev escapeSingleQuotesForShell := escapeSingleQuotes
/-- Retrieve all flag spellings for an `OptionEntry`. -/
abbrev optionFlagsFor := optionFlags
/-- Extract the long form of an option flag. -/
abbrev optionLongFlag := optionLong
/-- Extract the short form of an option flag. -/
abbrev optionShortFlag := optionShort

end Completion
end Argparse

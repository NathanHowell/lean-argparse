import Std
import Argparse.Basic.Reader
import Argparse.Basic.Parser

namespace Argparse

open Std

/-- Specification describing how to parse an option that consumes a value. -/
structure OptionSpec (α : Type u) where
  /-- Long flag name (e.g. `--count`). -/
  long? : Option String := none
  /-- Short flag name (e.g. `-c`). -/
  short? : Option Char := none
  /-- Placeholder displayed in help for the option's value. -/
  metavar : String := "VALUE"
  /-- Help text shown alongside the option. -/
  help? : Option String := none
  /-- Reader that converts the raw string into the desired type. -/
  reader : ValueReader α
  /-- Default value used when the option is omitted. -/
  default? : Option α := none
  /-- String representation of the default value shown in help. -/
  showDefault? : Option String := none

namespace OptionSpec

/-- Modifier applied to an option specification before it is materialized. -/
abbrev Mod (α : Type u) := OptionSpec α → OptionSpec α

/-- Create a baseline option specification given a reader. -/
def base (reader : ValueReader α) : OptionSpec α :=
  { reader := reader }

/-- Apply a sequence of modifiers to an option specification. -/
def applyMods (mods : List (Mod α)) (spec : OptionSpec α) : OptionSpec α :=
  mods.foldl (fun acc f => f acc) spec

/-- Build an option specification from a reader and modifiers. -/
def build (reader : ValueReader α) (mods : List (Mod α)) : OptionSpec α :=
  applyMods mods (base reader)

/-- Set the long flag name on an option specification. -/
def long (name : String) : Mod α := fun spec => { spec with long? := some name }

/-- Set the short flag name on an option specification. -/
def short (c : Char) : Mod α := fun spec => { spec with short? := some c }

/-- Override the metavar displayed in help output. -/
def setMetavar (name : String) : Mod α := fun spec => { spec with metavar := name }

/-- Supply help text describing the flag. -/
def help (text : String) : Mod α := fun spec => { spec with help? := some text }

/-- Provide a default value for the option. -/
def default (value : α) : Mod α := fun spec => { spec with default? := some value }

/-- Provide a stringified default for inclusion in help text. -/
def showDefault (value : String) : Mod α := fun spec => { spec with showDefault? := some value }

end OptionSpec

/-- Specification describing how to parse a flag. -/
structure FlagSpec (α : Type u) where
  /-- Long flag name (e.g. `--verbose`). -/
  long? : Option String := none
  /-- Short flag name (e.g. `-v`). -/
  short? : Option Char := none
  /-- Help text describing the flag. -/
  help? : Option String := none
  /-- Value produced when the flag is absent. -/
  default : α
  /-- Value produced when the flag is present. -/
  active : α

namespace FlagSpec

/-- Modifier applied to a flag specification before it is materialized. -/
abbrev Mod (α : Type u) := FlagSpec α → FlagSpec α

/-- Create a baseline flag specification from default and active values. -/
def base (default active : α) : FlagSpec α :=
  { default := default, active := active }
/-- Apply a sequence of modifiers to a flag specification. -/
def applyMods (mods : List (Mod α)) (spec : FlagSpec α) : FlagSpec α :=
  mods.foldl (fun acc f => f acc) spec
/-- Build a flag specification from defaults and modifiers. -/
def build (default active : α) (mods : List (Mod α)) : FlagSpec α :=
  applyMods mods (base default active)
/-- Set the long flag name on a flag specification. -/
def long (name : String) : Mod α := fun spec => { spec with long? := some name }
/-- Set the short flag name on a flag specification. -/
def short (c : Char) : Mod α := fun spec => { spec with short? := some c }

/-- Supply help text describing the flag. -/
def help (text : String) : Mod α := fun spec => { spec with help? := some text }

end FlagSpec

/-- Specification describing a positional argument. -/
structure ArgumentSpec (α : Type u) where
  /-- Placeholder displayed in help for the positional argument. -/
  metavar : String
  /-- Optional help text describing the argument. -/
  help? : Option String := none
  /-- Reader that converts the raw string into the desired type. -/
  reader : ValueReader α

/-- Description of a single subcommand and its parser. -/
structure Subcommand (α : Type u) where
  /-- Name of the subcommand. -/
  name : String
  /-- Optional description shown in help listings. -/
  description? : Option String := none
  /-- Parser invoked when the subcommand is selected. -/
  parser : Parser α

/-- Collection of subcommands available at a parser branch. -/
structure SubcommandSpec (α : Type u) where
  /-- Placeholder displayed in help for the subcommand. -/
  metavar : String := "COMMAND"
  /-- Optional description explaining the subcommand group. -/
  help? : Option String := none
  /-- Subcommands available beneath this node. -/
  commands : List (Subcommand α)

end Argparse
import Std
import LeanArgparse.Basic.Reader
import LeanArgparse.Basic.Parser

namespace LeanArgparse

open Std

structure OptionSpec (α : Type u) where
  long? : Option String := none
  short? : Option Char := none
  metavar : String := "VALUE"
  help? : Option String := none
  reader : ValueReader α
  default? : Option α := none
  showDefault? : Option String := none

namespace OptionSpec

abbrev Mod (α : Type u) := OptionSpec α → OptionSpec α

def base (reader : ValueReader α) : OptionSpec α :=
  { reader := reader }

def applyMods (mods : List (Mod α)) (spec : OptionSpec α) : OptionSpec α :=
  mods.foldl (fun acc f => f acc) spec

def build (reader : ValueReader α) (mods : List (Mod α)) : OptionSpec α :=
  applyMods mods (base reader)

def long (name : String) : Mod α := fun spec => { spec with long? := some name }

def short (c : Char) : Mod α := fun spec => { spec with short? := some c }

def setMetavar (name : String) : Mod α := fun spec => { spec with metavar := name }

def help (text : String) : Mod α := fun spec => { spec with help? := some text }

def default (value : α) : Mod α := fun spec => { spec with default? := some value }

def showDefault (value : String) : Mod α := fun spec => { spec with showDefault? := some value }

end OptionSpec

structure FlagSpec (α : Type u) where
  long? : Option String := none
  short? : Option Char := none
  help? : Option String := none
  default : α
  active : α

namespace FlagSpec

abbrev Mod (α : Type u) := FlagSpec α → FlagSpec α

def base (default active : α) : FlagSpec α :=
  { default := default, active := active }

def applyMods (mods : List (Mod α)) (spec : FlagSpec α) : FlagSpec α :=
  mods.foldl (fun acc f => f acc) spec

def build (default active : α) (mods : List (Mod α)) : FlagSpec α :=
  applyMods mods (base default active)

def long (name : String) : Mod α := fun spec => { spec with long? := some name }

def short (c : Char) : Mod α := fun spec => { spec with short? := some c }

def help (text : String) : Mod α := fun spec => { spec with help? := some text }

end FlagSpec

structure ArgumentSpec (α : Type u) where
  metavar : String
  help? : Option String := none
  reader : ValueReader α

structure Subcommand (α : Type u) where
  name : String
  description? : Option String := none
  parser : Parser α

structure SubcommandSpec (α : Type u) where
  metavar : String := "COMMAND"
  help? : Option String := none
  commands : List (Subcommand α)

end LeanArgparse

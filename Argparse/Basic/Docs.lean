import Std

namespace Argparse

open Std

/-- Documentation for a single option-style argument. -/
structure OptionDoc where
  /-- Long flag name (e.g. `--count`). -/
  long? : Option String := none
  /-- Short flag name (e.g. `-c`). -/
  short? : Option Char := none
  /-- Placeholder displayed in help for the option's value. -/
  metavar? : Option String := none
  /-- Help text shown alongside the option. -/
  help? : Option String := none
  /-- Whether the option must be provided by the user. -/
  required : Bool := true
  /-- String representation of the default value, when any. -/
  default? : Option String := none
  deriving Repr

/-- Documentation for a positional argument. -/
structure PositionalDoc where
  /-- Placeholder displayed in help for the argument. -/
  metavar : String
  /-- Optional help text describing the argument. -/
  help? : Option String := none
  /-- Whether the argument must be present. -/
  required : Bool := true
  deriving Repr

mutual
  /-- Describes usage information for a parser. -/
  structure Usage where
    /-- All documented options. -/
    options : List OptionDoc := []
    /-- All documented positional arguments. -/
    positionals : List PositionalDoc := []
    /-- Nested command documentation. -/
    commands : List CommandDoc := []
    deriving Repr

  /-- Documentation for a single subcommand. -/
  structure CommandDoc where
    /-- Name that must be typed to select the subcommand. -/
    name : String
    /-- Optional short description shown in help listings. -/
    description? : Option String := none
    /-- Usage tree describing the subcommand's own arguments. -/
    usage : Usage
    deriving Repr
end

namespace Usage

/-- An empty usage description with no arguments. -/
def empty : Usage := { options := [], positionals := [], commands := [] }

/-- Concatenate two usage descriptions. -/
def append (a b : Usage) : Usage := {
  options := a.options ++ b.options,
  positionals := a.positionals ++ b.positionals,
  commands := a.commands ++ b.commands
}

/-- Mark every argument in a usage description as optional. -/
def optional (u : Usage) : Usage := {
  u with
    options := u.options.map (fun opt => { opt with required := false }),
    positionals := u.positionals.map (fun pos => { pos with required := false })
}

/-- Append the documentation for a single option. -/
def mergeOption (opt : OptionDoc) (u : Usage) : Usage := {
  u with options := u.options ++ [opt]
}

/-- Append the documentation for a positional argument. -/
def mergePositional (pos : PositionalDoc) (u : Usage) : Usage := {
  u with positionals := u.positionals ++ [pos]
}

/-- Append the documentation for a subcommand. -/
def mergeCommand (cmd : CommandDoc) (u : Usage) : Usage := {
  u with commands := u.commands ++ [cmd]
}

end Usage

end Argparse

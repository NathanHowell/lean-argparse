import Std

namespace Argparse

open Std

/-- Documentation for a single option-style argument. -/
structure OptionDoc where
  long? : Option String := none
  short? : Option Char := none
  metavar? : Option String := none
  help? : Option String := none
  required : Bool := true
  default? : Option String := none
  deriving Repr

/-- Documentation for a positional argument. -/
structure PositionalDoc where
  metavar : String
  help? : Option String := none
  required : Bool := true
  deriving Repr

mutual
  /-- Describes usage information for a parser. -/
  structure Usage where
    options : List OptionDoc := []
    positionals : List PositionalDoc := []
    commands : List CommandDoc := []
    deriving Repr

  /-- Documentation for a single subcommand. -/
  structure CommandDoc where
    name : String
    description? : Option String := none
    usage : Usage
    deriving Repr
end

namespace Usage

def empty : Usage := { options := [], positionals := [], commands := [] }

def append (a b : Usage) : Usage := {
  options := a.options ++ b.options,
  positionals := a.positionals ++ b.positionals,
  commands := a.commands ++ b.commands
}

def optional (u : Usage) : Usage := {
  u with
    options := u.options.map (fun opt => { opt with required := false }),
    positionals := u.positionals.map (fun pos => { pos with required := false })
}

def mergeOption (opt : OptionDoc) (u : Usage) : Usage := {
  u with options := u.options ++ [opt]
}

def mergePositional (pos : PositionalDoc) (u : Usage) : Usage := {
  u with positionals := u.positionals ++ [pos]
}

def mergeCommand (cmd : CommandDoc) (u : Usage) : Usage := {
  u with commands := u.commands ++ [cmd]
}

end Usage

end Argparse

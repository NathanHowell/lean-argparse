import Argparse.Core.Parser
import Argparse.Spec.AST

namespace ArgParse.Spec

open ArgParse

structure Partial where
  flags : List (String × Bool) := []
  options : List (String × String) := []
  positionals : List (String × String) := []
deriving Repr

namespace Partial

def empty : Partial := {}

def addFlag (name : String) (value : Bool) (p : Partial) : Partial :=
  { p with flags := (name, value) :: p.flags }

def addOption (name : String) (value : String) (p : Partial) : Partial :=
  { p with options := (name, value) :: p.options }

def addPositional (name : String) (value : String) (p : Partial) : Partial :=
  { p with positionals := (name, value) :: p.positionals }

end Partial

/-- Placeholder elaborators pending the real interpreter. -/
def elaborateItem (_ : Unit) : Parser (Partial → Partial) :=
  fun st => .ok id st

def elaborateItems (_ : Unit) : Parser (Partial → Partial) :=
  fun st => .ok id st

def elaborateCommand (_ : Unit) : Parser Partial :=
  fun st => .ok Partial.empty st

def elaborateApp (_ : AppSpec) : Parser Partial :=
  fun st => .ok Partial.empty st

end ArgParse.Spec

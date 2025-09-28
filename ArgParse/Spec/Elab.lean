import ArgParse.Core.Parser
import ArgParse.Spec.AST

namespace ArgParse.Spec

open ArgParse

/-- Parser runtime accumulator storing intermediate flag/option/positional values. -/
structure Partial where
  /-- Recorded flag values, newest entry first. -/
  flags : List (String × Bool) := []
  /-- Recorded option key/value pairs. -/
  options : List (String × String) := []
  /-- Recorded positional key/value pairs. -/
  positionals : List (String × String) := []
deriving Repr

namespace Partial

/-- Empty accumulator with no recorded values. -/
def empty : Partial := {}

/-- Record a boolean flag in the accumulator. -/
def addFlag (name : String) (value : Bool) (p : Partial) : Partial :=
  { p with flags := (name, value) :: p.flags }

/-- Record an option key/value pair in the accumulator. -/
def addOption (name : String) (value : String) (p : Partial) : Partial :=
  { p with options := (name, value) :: p.options }

/-- Record a positional key/value pair in the accumulator. -/
def addPositional (name : String) (value : String) (p : Partial) : Partial :=
  { p with positionals := (name, value) :: p.positionals }

/-- Summary view derived from `Partial` for downstream consumers. -/
structure Summary where
  /-- Snapshot of flag values derived from `Partial`. -/
  flags : List (String × Bool) := []
  /-- Snapshot of option values derived from `Partial`. -/
  options : List (String × String) := []
  /-- Snapshot of positional values derived from `Partial`. -/
  positionals : List (String × String) := []
deriving Repr

namespace Summary

/-- Lookup the current boolean value for a flag. -/
def flagValue? (summary : Summary) (name : String) : Option Bool :=
  (summary.flags.find? (fun entry => entry.fst = name)).map (·.snd)

/-- Collect all values provided for a particular option. -/
def optionValues (summary : Summary) (name : String) : List String :=
  summary.options.filterMap (fun entry => if entry.fst = name then some entry.snd else none)

/-- Collect all positional values stored under a given key. -/
def positionalValues (summary : Summary) (name : String) : List String :=
  summary.positionals.filterMap (fun entry => if entry.fst = name then some entry.snd else none)

end Summary

/-- Convert the accumulated partial state into a summary view. -/
def toSummary (p : Partial) : Summary :=
  { flags := p.flags, options := p.options, positionals := p.positionals }

end Partial

/-- Placeholder elaborator for a single item; currently a no-op. -/
def elaborateItem (_ : Unit) : Parser (Partial → Partial) :=
  fun st => .ok id st

/-- Placeholder elaborator for a list of items; currently a no-op. -/
def elaborateItems (_ : Unit) : Parser (Partial → Partial) :=
  fun st => .ok id st

/-- Placeholder elaborator for a command; currently returns `Partial.empty`. -/
def elaborateCommand (_ : Unit) : Parser Partial :=
  fun st => .ok Partial.empty st

/-- Placeholder elaborator for the application; currently returns `Partial.empty`. -/
def elaborateApp (_ : AppSpec) : Parser Partial :=
  fun st => .ok Partial.empty st

end ArgParse.Spec

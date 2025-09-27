import Argparse.Core.Combinators
import Argparse.Core.Parser
import Argparse.Spec.AST

/-!
# ArgParse.Spec.Elab

Scaffold for elaborating the specification AST into runtime parsers.
-/

namespace ArgParse.Spec

open ArgParse
open ArgParse.Core

/-- Intermediate record collecting the outputs of primitive parsers. -/
structure Partial where
  flags : List (String × Bool) := []
  options : List (String × String) := []
  positionals : List (String × String) := []
deriving Repr

namespace Partial

@[simp] def empty : Partial := {}

@[simp] def addFlag (name : String) (value : Bool) (p : Partial) : Partial :=
  { p with flags := (name, value) :: p.flags }

@[simp] def addOption (name : String) (value : String) (p : Partial) : Partial :=
  { p with options := (name, value) :: p.options }

@[simp] def addPositional (name : String) (value : String) (p : Partial) : Partial :=
  { p with positionals := (name, value) :: p.positionals }

def flagValue? (p : Partial) (name : String) : Option Bool :=
  (p.flags.find? (fun entry => entry.fst = name)).map (·.snd)

def optionValues (p : Partial) (name : String) : List String :=
  p.options.filterMap (fun entry => if entry.fst = name then some entry.snd else none)

def positionalValues (p : Partial) (name : String) : List String :=
  p.positionals.filterMap (fun entry => if entry.fst = name then some entry.snd else none)

end Partial

/-- Interpret a flag spec and record the boolean result. -/
def interpretFlag (spec : FlagSpec) : Parser (Partial → Partial) :=
  flag spec |>.map fun enabled => fun p =>
    Partial.addFlag spec.meta.name enabled p

/-- Interpret a single-valued option (arity `.one`). -/
def interpretOption (spec : OptSpec String) : Parser (Partial → Partial) :=
  option spec |>.map fun value? => fun p =>
    match value? with
    | some value => Partial.addOption spec.meta.name value p
    | none => p

/-- Interpret a positional value and record it. -/
def interpretPositional (spec : PosSpec String) : Parser (Partial → Partial) :=
  positional spec |>.map fun value? => fun p =>
    match value? with
    | some value => Partial.addPositional spec.meta.name value p
    | none => p

/-- Elaborate a single command item to a partial-state transformer. -/
def elaborateItem : ItemSpec → Parser (Partial → Partial)
  | .flag spec => interpretFlag spec
  | .opt spec =>
      -- TODO: handle general arities; currently assumes `.one` yielding `String` values.
      interpretOption (α := String) spec
  | .pos spec => interpretPositional spec

/-- Compose the transformers for a list of items. -/
private def foldItems : List ItemSpec → Parser (Partial → Partial)
  | [] => Parser.pure id
  | item :: rest =>
      Parser.seq
        (Parser.map (fun f g => fun p => g (f p)) (elaborateItem item))
        (foldItems rest)

/-- Elaborate a command by folding its items; subcommands unimplemented. -/
def elaborateCommand (cmd : CmdSpec) : Parser Partial :=
  Parser.map (fun t => t Partial.mk) (foldItems cmd.args)

/-- Entry point: elaborate the root application spec. -/
def elaborateApp (app : AppSpec) : Parser Partial :=
  elaborateCommand app.root

end ArgParse.Spec

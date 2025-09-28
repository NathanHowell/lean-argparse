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

@[simp] lemma flagValue?_addFlag_self
    (p : Partial) (name : String) (value : Bool) :
    flagValue? (addFlag name value p) name = some value := by
  simp [flagValue?, addFlag]

@[simp] lemma flagValue?_addFlag_of_ne
    (p : Partial) {name name' : String} (value : Bool)
    (h : name' ≠ name) :
    flagValue? (addFlag name value p) name' = flagValue? p name' := by
  simp [flagValue?, addFlag, h.symm]

@[simp] lemma optionValues_addOption_self
    (p : Partial) (name value : String) :
    optionValues (addOption name value p) name = value :: optionValues p name := by
  classical
  simp [optionValues, addOption]

@[simp] lemma optionValues_addOption_of_ne
    (p : Partial) {name name' value : String}
    (h : name' ≠ name) :
    optionValues (addOption name value p) name' = optionValues p name' := by
  classical
  simp [optionValues, addOption, h.symm]

@[simp] lemma positionalValues_addPositional_self
    (p : Partial) (name value : String) :
    positionalValues (addPositional name value p) name =
      value :: positionalValues p name := by
  classical
  simp [positionalValues, addPositional]

@[simp] lemma positionalValues_addPositional_of_ne
    (p : Partial) {name name' value : String}
    (h : name' ≠ name) :
    positionalValues (addPositional name value p) name' =
      positionalValues p name' := by
  classical
  simp [positionalValues, addPositional, h.symm]

end Partial

/-- Result of elaborating a command, optionally paired with a selected subcommand. -/
structure CommandResult where
  self   : Partial := Partial.empty
  child? : Option (String × CommandResult) := none
deriving Repr

namespace CommandResult

@[simp] def empty : CommandResult := {}

end CommandResult

/-- Interpret a flag spec and record the boolean result. -/
def interpretFlag (spec : FlagSpec) : Parser (Partial → Partial) :=
  flag spec |>.map fun enabled => fun p =>
    Partial.addFlag spec.meta.name enabled p

/-- Interpret an option according to its arity, recording values. -/
def interpretOption (spec : OptSpec String) : Parser (Partial → Partial) :=
  match spec.arity with
  | .zero =>
      option spec |>.map (fun _ => id)
  | .one =>
      option spec |>.map fun value? => fun p =>
        match value? with
        | some value => Partial.addOption spec.meta.name value p
        | none => p
  | .many =>
      option spec |>.map fun values =>
        fun p => values.foldl (fun acc value => Partial.addOption spec.meta.name value acc) p
  | .some =>
      option spec |>.map fun values =>
        fun p => values.foldl (fun acc value => Partial.addOption spec.meta.name value acc) p

/-- Interpret a positional value and record it according to arity. -/
def interpretPositional (spec : PosSpec String) : Parser (Partial → Partial) :=
  match spec.arity with
  | .zero =>
      positional spec |>.map (fun _ => id)
  | .one =>
      positional spec |>.map fun value? => fun p =>
        match value? with
        | some value => Partial.addPositional spec.meta.name value p
        | none => p
  | .many =>
      positional spec |>.map fun values =>
        fun p => values.foldl (fun acc value => Partial.addPositional spec.meta.name value acc) p
  | .some =>
      positional spec |>.map fun values =>
        fun p => values.foldl (fun acc value => Partial.addPositional spec.meta.name value acc) p

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

private def lookupChild?
    (entries : List (String × Parser CommandResult))
    (token : String) : Option (Parser CommandResult) :=
  (entries.find? (fun entry => entry.fst = token)).map (·.snd)

/-- Elaborate a command, folding local items before descending into a child subcommand. -/
def elaborateCommand (cmd : CmdSpec) : Parser CommandResult :=
  fun st =>
    match foldItems cmd.args st with
    | .err err => .err err
    | .ok transformer st' =>
        let partial := transformer Partial.empty
        let children := cmd.subs.map fun sub => (sub.name, elaborateCommand sub)
        match st'.pre with
        | [] => .ok { self := partial } st'
        | token :: rest =>
            match lookupChild? children token with
            | none => .ok { self := partial } st'
            | some parser =>
                let stAfter : State := { st' with pre := rest, cursor := st'.cursor + 1 }
                match parser stAfter with
                | .ok childResult st'' =>
                    .ok { self := partial, child? := some (token, childResult) } st''
                | .err err => .err err

/-- Entry point: elaborate the root application spec. -/
def elaborateApp (app : AppSpec) : Parser Partial :=
  fun st =>
    match elaborateCommand app.root st with
    | .ok result st' => .ok result.self st'
    | .err err => .err err

end ArgParse.Spec

import ArgParse.Core.Parser
import ArgParse.Core.Combinators
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

/-- Combine two partial payloads, appending records in left-to-right order. -/
def merge (a b : Partial) : Partial :=
  { flags := a.flags ++ b.flags
  , options := a.options ++ b.options
  , positionals := a.positionals ++ b.positionals }

end Partial


/-- Elaborate a single item specification into a transformer over `Partial`. -/
def elaborateItem : ItemSpec → Parser (Partial → Partial)
  | .flag spec =>
      -- Parse a boolean flag and record the result under the meta name.
      ArgParse.Core.flag spec |>.map (fun b => fun p => p.addFlag spec.«meta».name b)
  | @ItemSpec.opt α _ spec =>
      -- Handle options by arity; values are recorded using `repr`.
      match spec.arity with
      | .zero =>
          -- Zero-arity option: nothing to record; do not consume tokens here.
          Parser.pure id
      | .one =>
          -- Use the arity-agnostic helper to avoid dependent type equalities.
          (Parser.map (fun (ov : Option α) =>
            fun p =>
              match ov with
              | none => p
              | some _ => p.addOption spec.«meta».name "<val>")
            (fun st =>
            match ArgParse.Core.takeOptionValue? (α := α) spec st with
            | .ok (ov, st') => ArgParse.Result.ok ov st'
            | .error err => ArgParse.Result.err err))
      | .many =>
          (Parser.map (fun (vs : List α) =>
            fun p => vs.foldl (fun acc _ => acc.addOption spec.«meta».name "<val>") p)
            (fun st =>
            match ArgParse.Core.collectOptionValues (α := α) spec st with
            | .ok (vs, st') => ArgParse.Result.ok vs st'
            | .error err => ArgParse.Result.err err))
      | .some =>
          (Parser.map (fun (vs : List α) =>
            fun p => vs.foldl (fun acc _ => acc.addOption spec.«meta».name "<val>") p)
            (fun st =>
            match ArgParse.Core.collectOptionValues (α := α) spec st with
            | .ok (vs, st') => ArgParse.Result.ok vs st'
            | .error err => ArgParse.Result.err err))
  | @ItemSpec.pos α _ spec =>
      match spec.arity with
      | .zero => Parser.pure id
      | .one  =>
          (Parser.map (fun (ov : Option α) =>
            fun p => match ov with
              | none => p
              | some _ => p.addPositional spec.«meta».name "<val>")
            (fun st =>
            match ArgParse.Core.takePositionalValue? (α := α) spec st with
            | .ok (ov, st') => ArgParse.Result.ok ov st'
            | .error err => ArgParse.Result.err err))
      | .many =>
          (Parser.map (fun (vs : List α) =>
            fun p => vs.foldl (fun acc _ => acc.addPositional spec.«meta».name "<val>") p)
            (fun st =>
            match ArgParse.Core.collectPositionalValues (α := α) spec st with
            | .ok (vs, st') => ArgParse.Result.ok vs st'
            | .error err => ArgParse.Result.err err))
      | .some =>
          (Parser.map (fun (vs : List α) =>
            fun p => vs.foldl (fun acc _ => acc.addPositional spec.«meta».name "<val>") p)
            (fun st =>
            match ArgParse.Core.collectPositionalValues (α := α) spec st with
            | .ok (vs, st') => ArgParse.Result.ok vs st'
            | .error err => ArgParse.Result.err err))

/-- Elaborate a list of items, sequencing their transformers left-to-right. -/
def elaborateItems (items : List ItemSpec) : Parser (Partial → Partial) :=
  let rec go : List ItemSpec → Parser (Partial → Partial)
    | [] => Parser.pure id
    | item :: rest =>
        let head := elaborateItem item
        let tail := go rest
        -- Compose the two transformers: apply head, then tail.
        Parser.seq (Parser.map (fun f => fun (g : Partial → Partial) => g ∘ f) head) (fun _ => tail)
  go items

/-- Elaborate a command by folding its items into an initial `Partial`. -/
def elaborateCommand (cmd : CmdSpec) : Parser Partial :=
  let itemsP := elaborateItems cmd.args
  let subP : Parser Partial := fun st =>
    match st.pre with
    | token :: rest =>
        if token.startsWith "-" then
          ArgParse.Result.ok Partial.empty st
        else
          match cmd.subs.find? (fun c => c.name = token) with
          | some _ =>
              let st' : ArgParse.State := { st with pre := rest, cursor := st.cursor + 1 }
              -- For now, consume the subcommand token and stop (no recursion).
              ArgParse.Result.ok Partial.empty st'
          | none => ArgParse.Result.ok Partial.empty st
    | [] => ArgParse.Result.ok Partial.empty st
  Parser.seq (Parser.map (fun (f : Partial → Partial) => fun (child : Partial) => Partial.merge (f Partial.empty) child) itemsP) (fun _ => subP)

/-- Elaborate the application; currently delegates to the root command. -/
def elaborateApp (app : AppSpec) : Parser Partial :=
  elaborateCommand app.root

end ArgParse.Spec

import ArgParse.Core.Parser
import ArgParse.Core.Combinators
import ArgParse.Core.Scan
import ArgParse.Spec.AST

namespace ArgParse.Spec

open ArgParse
open Classical

/- Parser runtime accumulator storing intermediate flag/option/positional values. -/
structure Partial where
  /-- Recorded flag values in chronological order. -/
  flags : List (String × Bool) := []
  /-- Recorded option key/value pairs in chronological order. -/
  options : List (String × String) := []
  /-- Recorded positional key/value pairs in chronological order. -/
  positionals : List (String × String) := []
deriving Repr

namespace Partial

/-- Empty accumulator with no recorded values. -/
def empty : Partial := {}

/-- Record a boolean flag in the accumulator. -/
def addFlag (name : String) (value : Bool) (p : Partial) : Partial :=
  { p with flags := p.flags ++ [(name, value)] }

/-- Record an option key/value pair in the accumulator. -/
def addOption (name : String) (value : String) (p : Partial) : Partial :=
  { p with options := p.options ++ [(name, value)] }

/-- Record a positional key/value pair in the accumulator. -/
def addPositional (name : String) (value : String) (p : Partial) : Partial :=
  { p with positionals := p.positionals ++ [(name, value)] }

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

/-- Lookup the current boolean value for a flag using last-value-wins semantics. -/
def flagValue? (summary : Summary) (name : String) : Option Bool :=
  summary.flags.foldl
    (fun latest entry => if entry.fst = name then some entry.snd else latest)
    none

/-- Collect all values provided for a particular option. -/
def optionValues (summary : Summary) (name : String) : List String :=
  summary.options.filterMap (fun entry => if entry.fst = name then some entry.snd else none)

/-- Collect all positional values stored under a given key. -/
def positionalValues (summary : Summary) (name : String) : List String :=
  summary.positionals.filterMap (fun entry => if entry.fst = name then some entry.snd else none)

end Summary

/-- Convert the accumulated partial state into a summary view using stored erasures. -/
def toSummary (p : Partial) : Summary :=
  { flags := p.flags, options := p.options, positionals := p.positionals }

/-- Combine two partial payloads, appending records left-to-right. -/
def merge (earlier later : Partial) : Partial :=
  { flags := earlier.flags ++ later.flags
  , options := earlier.options ++ later.options
  , positionals := earlier.positionals ++ later.positionals }

end Partial


/- Helper error for `.some` options when no value is provided. -/
private def missingOptionError {α : Type} [ArgParse.FromArg α]
    (spec : ArgParse.Spec.OptSpec α) : ArgParse.Error :=
  { kind := ArgParse.ErrorKind.missingValue
  , context := []
  , expect := [ArgParse.Expect.optionVal spec.«meta».name] }


-- Subcommand recursion uses a simple token-derived measure to ensure
-- termination without relying on explicit proofs about the spec tree.

private def stateFuel (st : ArgParse.State) : Nat :=
  st.pre.length + st.post.length + 1

/-- Elaborate a single item specification into a transformer over `Partial`. -/
def elaborateItem : ItemSpec → Parser (Partial → Partial)
  | .flag spec =>
      -- Scan for the flag anywhere in the stream and record it under the meta name.
      ArgParse.Core.flagScan spec |>.map (fun b => fun p => if b then p.addFlag spec.«meta».name true else p)
  | @ItemSpec.opt α _ spec =>
      -- Handle options by arity; values are recorded using `repr`.
      match spec.arity with
      | .zero =>
          -- Zero-arity option: nothing to record; do not consume tokens here.
          Parser.pure id
      | .one =>
          (Parser.map (fun (payload : List α × List String) =>
            let raws := payload.snd
            fun p => raws.foldl (fun acc raw => acc.addOption spec.«meta».name raw) p)
            (fun st =>
            match ArgParse.Core.collectOptionScanValues (α := α) spec st with
            | .ok (values, raws, st') => ArgParse.Result.ok (values, raws) st'
            | .error err => ArgParse.Result.err err))
      | .many =>
          (Parser.map (fun (payload : List α × List String) =>
            let raws := payload.snd
            fun p => raws.foldl (fun acc raw => acc.addOption spec.«meta».name raw) p)
            (fun st =>
            match ArgParse.Core.collectOptionScanValues (α := α) spec st with
            | .ok (values, raws, st') => ArgParse.Result.ok (values, raws) st'
            | .error err => ArgParse.Result.err err))
      | .some =>
          (Parser.map (fun (payload : List α × List String) =>
            let raws := payload.snd
            fun p => raws.foldl (fun acc raw => acc.addOption spec.«meta».name raw) p)
            (fun st =>
            match ArgParse.Core.collectOptionScanValues (α := α) spec st with
            | .ok (values, raws, st') =>
                match values with
                | [] => ArgParse.Result.err (missingOptionError spec)
                | _ => ArgParse.Result.ok (values, raws) st'
            | .error err => ArgParse.Result.err err))
  | @ItemSpec.pos α _ spec =>
      match spec.arity with
      | .zero => Parser.pure id
      | .one  =>
          (Parser.map (fun (ov : Option (α × String)) =>
            fun p => match ov with
              | none => p
              | some (_, raw) => p.addPositional spec.«meta».name raw)
            (fun st =>
            match ArgParse.Core.takePositionalValue? (α := α) spec st with
            | .ok (ov, st') => ArgParse.Result.ok ov st'
            | .error err => ArgParse.Result.err err))
      | .many =>
          (Parser.map (fun (payload : List α × List String) =>
            let raws := payload.snd
            fun p => raws.foldl (fun acc raw => acc.addPositional spec.«meta».name raw) p)
            (fun st =>
            match ArgParse.Core.collectPositionalValues (α := α) spec st with
            | .ok (values, raws, st') => ArgParse.Result.ok (values, raws) st'
            | .error err => ArgParse.Result.err err))
      | .some =>
          (Parser.map (fun (payload : List α × List String) =>
            let raws := payload.snd
            fun p => raws.foldl (fun acc raw => acc.addPositional spec.«meta».name raw) p)
            (fun st =>
            match ArgParse.Core.collectPositionalValues (α := α) spec st with
            | .ok (values, raws, st') => ArgParse.Result.ok (values, raws) st'
            | .error err => ArgParse.Result.err err))

/-- Reorder items so scanning flags/options run before front-of-stream
positionals, making item declaration order irrelevant to token order. -/
def orderItems (items : List ItemSpec) : List ItemSpec :=
  let isPos : ItemSpec → Bool
    | @ItemSpec.pos _ _ _ => true
    | _ => false
  items.filter (fun item => !(isPos item)) ++ items.filter isPos

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

def elaborateCommandCore : (fuel : Nat) → CmdSpec → Parser Partial
  | 0, _ => Parser.pure Partial.empty
  | fuel+1, cmd =>
      -- Restrict item scanning to the segment before the first subcommand name
      -- so a parent's flags/options never reach into a child's arguments.
      let itemsP :=
        ArgParse.Core.scopedPre (cmd.subs.map (·.name))
          (elaborateItems (orderItems cmd.args))
      let childParsers : List (ArgParse.Core.Subcommand Partial) :=
        cmd.subs.map fun child =>
          { name := child.name
            , parser := elaborateCommandCore fuel child : ArgParse.Core.Subcommand Partial }
      let subP : Parser Partial :=
        match childParsers with
        | [] => Parser.pure Partial.empty
        | _ =>
            fun st =>
              match st.pre with
              | token :: _ =>
                  if token.startsWith "-" then
                    ArgParse.Result.ok Partial.empty st
                  else
                    ArgParse.Core.subcommand childParsers st
              | [] => ArgParse.Result.ok Partial.empty st
      Parser.seq
        (Parser.map (fun (f : Partial → Partial) => fun (child : Partial) => Partial.merge (f Partial.empty) child) itemsP)
        (fun _ => subP)

@[simp] theorem elaborateCommandCore_zero (cmd : CmdSpec) :
    elaborateCommandCore 0 cmd = Parser.pure Partial.empty := rfl


/-- Elaborate a command by folding its items into an initial `Partial`. -/
def elaborateCommand (cmd : CmdSpec) : Parser Partial :=
  fun st =>
    let fuel := stateFuel st
    (elaborateCommandCore fuel cmd) st

/-- Elaborate the application; currently delegates to the root command. -/
def elaborateApp (app : AppSpec) : Parser Partial :=
  elaborateCommand app.root

end ArgParse.Spec

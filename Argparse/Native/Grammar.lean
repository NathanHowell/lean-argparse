import Std
import Init.Control.Lawful
import Argparse.Basic.Docs
import Argparse.Native.Error
import Argparse.Native.Token
import Argparse.Native.TokenCursor

namespace Argparse
namespace Native

open Token

namespace Usage

@[simp] theorem empty_append (u : Usage) : Usage.append Usage.empty u = u := by
  cases u
  simp [Usage.append, Usage.empty]

@[simp] theorem append_empty (u : Usage) : Usage.append u Usage.empty = u := by
  cases u
  simp [Usage.append, Usage.empty]

end Usage

/-- Result type returned by the native interpreter. -/
abbrev Result (α : Type) := Except Error α

/-- Metadata-only grammar description. -/
structure Grammar (α : Type) where
  usage : Usage

namespace Grammar

/-- Embed a pure value (no usage contribution). -/
def pure {α : Type} (_ : α) : Grammar α :=
  { usage := Usage.empty }

/-- Record a failure branch (no usage contribution). -/
def fail {α : Type} : Grammar α :=
  { usage := Usage.empty }

/-- Map over a grammar without changing usage. -/
def map {α β : Type} (_ : α → β) (g : Grammar α) : Grammar β :=
  { usage := g.usage }

/-- Sequential application combines usage from left to right. -/
def seq {α β : Type} (gf : Grammar (α → β)) (ga : Grammar α) : Grammar β :=
  { usage := Usage.append gf.usage ga.usage }

/-- Primitive positional argument metadata. -/
def positional (doc : PositionalDoc) : Grammar String :=
  { usage := Usage.mergePositional doc Usage.empty }

/-- Flag metadata contributes the provided option doc. -/
def flag (doc : OptionDoc) : Grammar Bool :=
  { usage := Usage.mergeOption doc Usage.empty }

/-- Option metadata contributes the provided option doc. -/
def option (doc : OptionDoc) : Grammar (Option String) :=
  { usage := Usage.mergeOption doc Usage.empty }

/-- Grammar that always fails. Mirrors `Alternative.failure`. -/
def failure {α : Type} : Grammar α := fail

/-- Grammar-level alternative combining usage information for both branches. -/
def orElse {α : Type} (ga : Grammar α) (gb : Unit → Grammar α) : Grammar α :=
  let gb' := gb ()
  { usage := Usage.optional (Usage.append ga.usage gb'.usage) }

/-- Strict helper for `orElse`. -/
def orElse' {α : Type} (ga gb : Grammar α) : Grammar α :=
  orElse ga (fun _ => gb)

end Grammar

instance : Functor Grammar where
  map := Grammar.map
  mapConst := fun {_ _} _ g => { usage := g.usage }

instance : Pure Grammar := ⟨Grammar.pure⟩

instance : Seq Grammar where
  seq gf ga := Grammar.seq gf (ga ())

instance : Applicative Grammar where
  pure := Grammar.pure
  seq := Seq.seq
  map := Functor.map

instance : Alternative Grammar where
  failure := Grammar.failure
  orElse := Grammar.orElse

instance : LawfulFunctor Grammar where
  map_const := by
    intro (_ : Type) (_ : Type); funext _ g; cases g; rfl
  id_map := by
    intro (_ : Type) g; cases g; rfl
  comp_map := by
    intro (_ : Type) (_ : Type) (_ : Type) g h x; cases x; rfl

instance : LawfulApplicative Grammar where
  seqLeft_eq := by
    intro _ _ x y
    cases x with
    | mk ux =>
      cases y with
      | mk uy =>
        simp [SeqLeft.seqLeft, Seq.seq, Functor.map, Grammar.seq, Grammar.map, Usage.append]
  seqRight_eq := by
    intro _ _ x y
    cases x with
    | mk ux =>
      cases y with
      | mk uy =>
        simp [SeqRight.seqRight, Seq.seq, Functor.map, Grammar.seq, Grammar.map, Usage.append]
  pure_seq := by
    intro _ _ g x
    cases x with
    | mk ux =>
      have hx : Usage.append (Grammar.pure g).usage ux = ux := by
        simpa [Grammar.pure, Usage.append] using Usage.empty_append ux
      simpa [Seq.seq, Grammar.seq, Functor.map, Grammar.map, Grammar.pure, Usage.append, hx]
  map_pure := by
    intro _ _ g x
    change Grammar.map g (Grammar.pure x) = Grammar.pure (g x)
    simp [Grammar.map, Grammar.pure, Usage.empty]
  seq_pure := by
    intro _ _ g x
    cases g with
    | mk ug =>
      have hg : Usage.append ug (Grammar.pure x).usage = ug := by
        simpa [Grammar.pure, Usage.append] using Usage.append_empty ug
      simpa [Seq.seq, Grammar.seq, Functor.map, Grammar.map, Grammar.pure, Usage.append, hg]
  seq_assoc := by
    intro _ _ _ x g h
    cases x with
    | mk ux =>
      cases g with
      | mk ug =>
        cases h with
        | mk uh =>
          simp [Seq.seq, Grammar.seq, Functor.map, Grammar.map]
          cases ux
          cases ug
          cases uh
          simp [Usage.append, List.append_assoc]

private def leftoverOptionError (opt : ParsedOption) : Error :=
  { code := .unexpected
    , subject? := some opt.original
    , detail? := some "no parser field accepted this option" }

private def leftoverPositionalError (arg : String) : Error :=
  { code := .unexpected
    , subject? := some arg
    , detail? := some "too many positional arguments" }

private def missingPositionalError (doc : PositionalDoc) : Error :=
  { code := .missing
    , subject? := some doc.metavar }

/-- Interpreter pairs usage metadata with a state transformer that consumes
classified tokens left-to-right. -/
structure Interpreter (α : Type) where
  grammar : Grammar α
  run : TokenCursor → Except Error (α × TokenCursor)

namespace Interpreter

@[inline] def evalTokens {α : Type}
    (i : Interpreter α) (cursor : TokenCursor) : Result α := do
  let (value, cursor') ← i.run cursor
  match cursor'.options with
  | opt :: _ => throw (leftoverOptionError opt)
  | [] =>
      match cursor'.positionals with
      | arg :: _ => throw (leftoverPositionalError arg)
      | [] => pure value

@[inline] def eval {α : Type} (i : Interpreter α) (argv : List String) : Result α :=
  evalTokens i (TokenCursor.fromArgv argv)

/-- Functorial map over interpreter results. -/
def map {α β : Type} (i : Interpreter α) (f : α → β) : Interpreter β :=
  { grammar := Grammar.map f i.grammar
    , run := fun cursor => do
        let (value, cursor') ← i.run cursor
        pure (f value, cursor') }

/-- Pure value interpreter. -/
def pure {α : Type} (value : α) : Interpreter α :=
  { grammar := Grammar.pure value
    , run := fun cursor => Except.ok (value, cursor) }

/-- Interpreter that immediately fails with `err`. -/
def fail {α : Type} (err : Error) : Interpreter α :=
  { grammar := Grammar.fail
    , run := fun _ => Except.error err }

/-- Sequential application of interpreters with thunked right branch. -/
def seq {α β : Type} (ifn : Interpreter (α → β)) (ival : Unit → Interpreter α)
    : Interpreter β :=
  let ival' := ival ()
  { grammar := Grammar.seq ifn.grammar ival'.grammar
    , run := fun cursor => do
        let (fn, cursor') ← ifn.run cursor
        let (arg, cursor'') ← ival'.run cursor'
        Except.ok (fn arg, cursor'') }

/-- Strict helper for `seq`. -/
def seqApply {α β : Type} (ifn : Interpreter (α → β)) (ival : Interpreter α)
    : Interpreter β :=
  seq ifn (fun _ => ival)

/-- Primitive positional argument. -/
def positional (doc : PositionalDoc) : Interpreter String :=
  { grammar := Grammar.positional doc
    , run := fun cursor =>
        match TokenCursor.takePositional? cursor with
        | some (value, cursor') => Except.ok (value, cursor')
        | none => Except.error (missingPositionalError doc) }

/-- Boolean flag that reports presence of the given token. -/
def flag {α : Type} [TokenSpec α] [TokenCursor.ToParsedName α]
    (doc : OptionDoc) (name : α) : Interpreter Bool :=
  { grammar := Grammar.flag doc
    , run := fun cursor => do
        let (present, cursor') ← TokenCursor.consumeFlag name cursor
        Except.ok (present, cursor') }

/-- Option parser returning the associated value when present. -/
def option {α : Type} [TokenSpec α] [TokenCursor.ToParsedName α]
    (doc : OptionDoc) (name : α) : Interpreter (Option String) :=
  { grammar := Grammar.option doc
    , run := fun cursor => do
        let (value?, cursor') ← TokenCursor.consumeValue name cursor
        Except.ok (value?, cursor') }

/-- Produce an optional result, returning `none` on missing errors. -/
def optional {α : Type} (p : Interpreter α) : Interpreter (Option α) :=
  { grammar := { usage := Usage.optional p.grammar.usage }
    , run := fun cursor =>
        match p.run cursor with
        | Except.ok (value, cursor') => Except.ok (some value, cursor')
        | Except.error err =>
            if err.code = .missing then
              Except.ok (none, cursor)
            else
              Except.error err }

/-- Supply a constant default when a parser reports a missing error. -/
def withDefault {α : Type} (p : Interpreter α) (value : α) : Interpreter α :=
  map (optional p) (fun opt : Option α => opt.getD value)

/-- Long flag convenience helper. -/
def longFlag (name : String)
    (doc : OptionDoc := { long? := some name, required := false }) : Interpreter Bool :=
  flag doc name

/-- Short flag convenience helper. -/
def shortFlag (name : Char)
    (doc : OptionDoc := { short? := some name, required := false }) : Interpreter Bool :=
  flag doc name

/-- Long option convenience helper. -/
def longOption (name : String)
    (doc : OptionDoc := { long? := some name, required := false }) : Interpreter (Option String) :=
  option doc name

/-- Short option convenience helper. -/
def shortOption (name : Char)
    (doc : OptionDoc := { short? := some name, required := false }) : Interpreter (Option String) :=
  option doc name

instance : Functor Interpreter where
  map f p := map p f

instance : Pure Interpreter := ⟨pure⟩

instance : Seq Interpreter where
  seq pf pa := seq pf pa

instance : Applicative Interpreter where
  pure := pure
  map f p := map p f
  seq := Seq.seq

end Interpreter

end Native
end Argparse

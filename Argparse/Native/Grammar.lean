import Std
import Argparse.Basic.Docs
import Argparse.Native.Error
import Argparse.Native.Token
import Argparse.Native.TokenCursor

namespace Argparse
namespace Native

open Token
open TokenCursor

/-- Result type returned by the native interpreter. -/
inductive Result (α : Type) where
  | ok (value : α) (rest : TokenCursor)
  | error (info : Error)
  deriving Repr

namespace Result

@[simp] def map {α β : Type} (f : α → β) : Result α → Result β
  | .ok value rest => .ok (f value) rest
  | .error err => .error err

end Result

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
def map {α β : Type} (g : Grammar α) (_ : α → β) : Grammar β :=
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

end Grammar

/-- Interpreter pairs metadata with an evaluator on `TokenCursor`. -/
structure Interpreter (α : Type) where
  grammar : Grammar α
  eval : TokenCursor → Result α

namespace Interpreter

/-- Remaining tokens count, used as a structural measure. -/
def remainingSize (cursor : TokenCursor) : Nat :=
  cursor.remaining

/-- Error emitted when a combinator fails to make progress. -/
def progressError : Error :=
  { code := .invalid
    , subject? := none
    , detail? := some "Interpreter combinator failed to consume input" }

/-- Access the usage metadata for an interpreter. -/
def usage {α : Type} (i : Interpreter α) : Usage :=
  i.grammar.usage

/-- Pure value interpreter. -/
def pure {α : Type} (value : α) : Interpreter α :=
  { grammar := Grammar.pure value
    , eval := fun stream => .ok value stream }

/-- Interpreter that immediately fails with `err`. -/
def fail {α : Type} (err : Error) : Interpreter α :=
  { grammar := Grammar.fail
    , eval := fun _ => .error err }

/-- Functorial map over interpreter results. -/
def map {α β : Type} (i : Interpreter α) (f : α → β) : Interpreter β :=
  { grammar := Grammar.map i.grammar f
    , eval := fun stream => Result.map f (i.eval stream) }

/-- Sequential application of interpreters with thunked right branch. -/
def seq {α β : Type} (ifn : Interpreter (α → β)) (ival : Unit → Interpreter α) : Interpreter β :=
  let ival' := ival ()
  { grammar := Grammar.seq ifn.grammar ival'.grammar
    , eval := fun stream =>
        match ifn.eval stream with
        | .ok fn stream' => Result.map fn (ival'.eval stream')
        | .error err => .error err }

/-- Strict helper for `seq`. -/
def seqApply {α β : Type} (ifn : Interpreter (α → β)) (ival : Interpreter α) : Interpreter β :=
  seq ifn (fun _ => ival)

/-- Left-biased sequencing returning the first result. -/
def seqLeft {α β : Type} (ia : Interpreter α) (ib : Unit → Interpreter β) : Interpreter α :=
  let ib' := ib ()
  { grammar := {
      usage := Usage.append ia.grammar.usage ib'.grammar.usage
    }
    , eval := fun stream =>
        match ia.eval stream with
        | .ok a stream' =>
            match ib'.eval stream' with
            | .ok _ stream'' => .ok a stream''
            | .error err => .error err
        | .error err => .error err }

/-- Strict helper for `seqLeft`. -/
def seqLeftApply {α β : Type} (ia : Interpreter α) (ib : Interpreter β) : Interpreter α :=
  seqLeft ia (fun _ => ib)

/-- Right-biased sequencing returning the second result. -/
def seqRight {α β : Type} (ia : Interpreter α) (ib : Unit → Interpreter β) : Interpreter β :=
  let ib' := ib ()
  { grammar := {
      usage := Usage.append ia.grammar.usage ib'.grammar.usage
    }
    , eval := fun stream =>
        match ia.eval stream with
        | .ok _ stream' => ib'.eval stream'
        | .error err => .error err }

/-- Strict helper for `seqRight`. -/
def seqRightApply {α β : Type} (ia : Interpreter α) (ib : Interpreter β) : Interpreter β :=
  seqRight ia (fun _ => ib)

/-- Consume a positional argument using the classified token stream. -/
def positional (doc : PositionalDoc) : Interpreter String :=
  { grammar := Grammar.positional doc
    , eval := fun cursor =>
        match TokenCursor.takePositional? cursor with
        | Option.some (tok, rest) => .ok tok rest
        | Option.none =>
            .error {
              code := .missing
              , subject? := some doc.metavar
            } }

/-- Boolean flag that reports presence of the given token. -/
def flag {α : Type} [TokenSpec α] [TokenCursor.ToParsedName α]
    (doc : OptionDoc) (name : α) : Interpreter Bool :=
  { grammar := Grammar.flag doc
    , eval := fun cursor =>
        match TokenCursor.consumeFlag name cursor with
        | .ok (present, stream') => .ok present stream'
        | .error err => .error err }

/-- Option parser returning the associated value when present. -/
def option {α : Type} [TokenSpec α] [TokenCursor.ToParsedName α]
    (doc : OptionDoc) (name : α) : Interpreter (Option String) :=
  { grammar := Grammar.option doc
    , eval := fun cursor =>
        match TokenCursor.consumeValue name cursor with
        | .ok (value?, stream') => .ok value? stream'
        | .error err => .error err }

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

/-- Parser that always fails with a structural `missing` error. -/
def failure {α : Type} (message? : Option String := none) : Interpreter α :=
  fail {
    code := .missing
    , detail? := message?
  }

/-- Try `p`, falling back to a lazily constructed alternative when `p` is missing. -/
def orElseCore {α : Type} (p : Interpreter α) (q : Unit → Interpreter α) : Interpreter α :=
  let q' := q ()
  {
    grammar := {
      usage := Usage.optional (Usage.append p.grammar.usage q'.grammar.usage)
    }
    , eval := fun stream =>
        match p.eval stream with
        | .ok value stream' => .ok value stream'
        | .error err =>
            if err.code = .missing then
              q'.eval stream
            else
              .error err
  }

/-- Non-lazy alternative between two interpreters. -/
def orElse {α : Type} (p q : Interpreter α) : Interpreter α :=
  orElseCore p (fun _ => q)

/-- Run a list of interpreters until one succeeds. -/
def choice {α : Type} : List (Interpreter α) → Interpreter α
  | [] => failure (message? := Option.some "empty choice")
  | p :: ps => ps.foldl (fun acc next => orElse acc next) p

/-- Produce an optional result, returning `none` on missing errors. -/
def optional {α : Type} (p : Interpreter α) : Interpreter (Option α) :=
  {
    grammar := {
      usage := Usage.optional p.grammar.usage
    }
    , eval := fun stream =>
        match p.eval stream with
        | .ok value stream' => .ok (Option.some value) stream'
        | .error err =>
            if err.code = .missing then
              .ok Option.none stream
            else
              .error err
  }

/-- Supply a default when a parser reports a missing error. -/
def optionalOrElse {α : Type} (p : Interpreter α) (backup : Unit → Interpreter α) : Interpreter α :=
  orElseCore p backup

/-- Supply a constant default when a parser reports a missing error. -/
def withDefault {α : Type} (p : Interpreter α) (value : α) : Interpreter α :=
  map (optional p) (fun opt : Option α => opt.getD value)

/-- Zero-or-more repetition combinator. -/
def many {α : Type} (p : Interpreter α) : Interpreter (List α) :=
  {
    grammar := {
      usage := Usage.optional p.grammar.usage
    },
    eval := fun cursor =>
      let fuel := remainingSize cursor
      let rec loop : Nat → List α → TokenCursor → Result (List α)
        | 0, acc, cursor => .ok acc.reverse cursor
        | Nat.succ fuel, acc, cursor =>
            match p.eval cursor with
            | .ok value cursor' =>
                if remainingSize cursor' < remainingSize cursor then
                  loop fuel (value :: acc) cursor'
                else
                  .error progressError
            | .error err =>
                if err.code = .missing then
                  .ok acc.reverse cursor
                else
                  .error err
      loop fuel [] cursor
  }

/-- One-or-more repetition combinator. -/
def some {α : Type} (p : Interpreter α) : Interpreter (List α) :=
  {
    grammar := {
      usage := Usage.append p.grammar.usage (Usage.optional p.grammar.usage)
    },
    eval := fun stream =>
      match p.eval stream with
      | .ok head stream' =>
          match (many p).eval stream' with
          | .ok tail stream'' => .ok (head :: tail) stream''
          | .error err => .error err
      | .error err => .error err
  }

instance : Functor Interpreter where
  map f p := map p f

instance : Pure Interpreter := ⟨pure⟩

instance : Seq Interpreter where
  seq pf pa := seq pf pa

instance : SeqLeft Interpreter where
  seqLeft := seqLeft

instance : SeqRight Interpreter where
  seqRight := seqRight

instance : Applicative Interpreter where
  pure := pure
  map f p := map p f
  seq := Seq.seq
  seqLeft := SeqLeft.seqLeft
  seqRight := SeqRight.seqRight

instance : Alternative Interpreter where
  failure := failure
  orElse := fun p q => orElseCore p q

end Interpreter

end Native
end Argparse

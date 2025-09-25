import Std
import Argparse.Basic.Error
import Argparse.Basic.Docs
import Argparse.Basic.ParseState

namespace Argparse

open Std

/-- Core parser type with usage metadata. -/
structure Parser (α : Type u) where
  run : ParseState → Except ParseError (α × ParseState)
  usage : Usage

namespace Parser

open Usage

private def map {α β} (f : α → β) (p : Parser α) : Parser β := {
  run := fun s => do
    let (a, s') ← p.run s
    return (f a, s'),
  usage := p.usage
}

private def pure {α} (x : α) : Parser α := {
  run := fun s => .ok (x, s),
  usage := Usage.empty
}

private def seqCore {α β} (pf : Parser (α → β)) (pa : Unit → Parser α) : Parser β :=
  let pa' := pa ()
  {
    run := fun s => do
      let (f, s') ← pf.run s
      let (a, s'') ← pa'.run s'
      return (f a, s''),
    usage := Usage.append pf.usage pa'.usage
  }

private def seqLeftCore {α β} (pa : Parser α) (pb : Unit → Parser β) : Parser α :=
  let pb' := pb ()
  {
    run := fun s => do
      let (a, s') ← pa.run s
      let (_, s'') ← pb'.run s'
      return (a, s''),
    usage := Usage.append pa.usage pb'.usage
  }

private def seqRightCore {α β} (pa : Parser α) (pb : Unit → Parser β) : Parser β :=
  let pb' := pb ()
  {
    run := fun s => do
      let (_, s') ← pa.run s
      let (b, s'') ← pb'.run s'
      return (b, s''),
    usage := Usage.append pa.usage pb'.usage
  }

instance : Functor Parser where
  map := fun f p => map f p

instance : Pure Parser := ⟨pure⟩

instance : Seq Parser where
  seq := seqCore

instance : SeqLeft Parser where
  seqLeft := seqLeftCore

instance : SeqRight Parser where
  seqRight := seqRightCore

private def failure {α} (msg : String := "empty parser") : Parser α :=
  {
    run := fun _ => .error {
      kind := .missing,
      message := msg
    },
    usage := Usage.empty
  }

private def orElseCore {α} (p : Parser α) (q : Unit → Parser α) : Parser α :=
  let q' := q ()
  {
    run := fun s =>
      match p.run s with
      | .ok result => .ok result
      | .error err =>
        if err.kind = .missing then
          q'.run s
        else
          .error err,
    usage := Usage.optional (Usage.append p.usage q'.usage)
  }

private def orElse {α} (p q : Parser α) : Parser α :=
  orElseCore p (fun _ => q)

instance : Applicative Parser where
  pure := pure
  map := fun f p => map f p
  seq := Seq.seq
  seqLeft := SeqLeft.seqLeft
  seqRight := SeqRight.seqRight

instance : Alternative Parser where
  failure := failure
  orElse := fun p q => orElseCore p q

def many {α} (p : Parser α) : Parser (List α) :=
  {
    run := fun s =>
      let rec loop : Nat → ParseState → List α → Except ParseError (List α × ParseState)
        | 0, state, acc => .ok (acc.reverse, state)
        | Nat.succ fuel, state, acc =>
          match p.run state with
          | .ok (value, state') => loop fuel state' (value :: acc)
          | .error err =>
            if err.kind = .missing then
              .ok (acc.reverse, state)
            else
              .error err
      loop (ParseState.size s) s []
    , usage := Usage.optional p.usage
  }

def some {α} (p : Parser α) : Parser (List α) := {
  run := fun s => do
    let (head, s') ← p.run s
    let (tail, s'') ← (many p).run s'
    return (head :: tail, s'')
  , usage := Usage.append p.usage (many p).usage
}

def many1 {α} (p : Parser α) : Parser (List α) := some p

def some1 {α} (p : Parser α) : Parser (List α) := some p

def optionalOrElse {α} (p : Parser α) (backup : Unit → Parser α) : Parser α :=
  orElseCore p backup

def choice {α} : List (Parser α) → Parser α
  | [] => failure "empty choice"
  | p :: ps => ps.foldl (fun acc next => orElse acc next) p

def optional {α} (p : Parser α) : Parser (Option α) := {
  run := fun s =>
    match p.run s with
    | .ok (a, s') => .ok (Option.some a, s')
    | .error err =>
      if err.kind = .missing then
        .ok (none, s)
      else
        .error err,
  usage := Usage.optional p.usage
}

def withDefault {α} (p : Parser α) (value : α) : Parser α :=
  map (fun opt => opt.getD value) (optional p)

end Parser

end Argparse

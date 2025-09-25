import Std
import LeanArgparse.Basic.Error

namespace LeanArgparse

open Std

/-- Parser state separates pre and post `--` segments. -/
structure ParseState where
  front : List String
  tail : List String
  deriving Repr

namespace ParseState

def ofList (args : List String) : ParseState :=
  match args.span (· ≠ "--") with
  | (front, []) => { front := front, tail := [] }
  | (front, _ :: tail) => { front := front, tail := tail }

def withFront (s : ParseState) (front : List String) : ParseState := { s with front }

def withTail (s : ParseState) (tail : List String) : ParseState := { s with tail }

def remaining (s : ParseState) : List String :=
  match s.tail with
  | [] => s.front
  | tail => s.front ++ "--" :: tail

def size (s : ParseState) : Nat :=
  s.front.length + s.tail.length

private def splitOnFirst (sep : Char) (xs : List Char) : List Char × Option (List Char) :=
  let rec loop (acc : List Char) : List Char → List Char × Option (List Char)
    | [] => (acc.reverse, none)
    | c :: cs =>
      if c = sep then
        (acc.reverse, some cs)
      else
        loop (c :: acc) cs
  loop [] xs

private def parseLongToken (tok : String) : Option (String × Option String) :=
  match tok.data with
  | '-' :: '-' :: rest =>
    let (nameChars, valueChars?) := splitOnFirst '=' rest
    let name := String.mk nameChars
    if name.isEmpty then none else
    some (name, valueChars?.map String.mk)
  | _ => none

private def parseShortToken (tok : String) : Option (Char × Option String) :=
  match tok.data with
  | '-' :: [] => none
  | '-' :: '-' :: _ => none
  | '-' :: c :: rest =>
    match rest with
    | [] => some (c, none)
    | '=' :: rest' => some (c, some (String.mk rest'))
    | _ => some (c, some (String.mk rest))
  | _ => none

private def isOptionLike (tok : String) : Bool :=
  match tok.data with
  | '-' :: '-' :: _ => true
  | '-' :: [] => false
  | '-' :: c :: _ => c.isAlpha
  | _ => false

def consumeLongFlag (s : ParseState) (name : String) : Except ParseError (Bool × ParseState) :=
  let rec loop (processed : List String) : List String → Except ParseError (Bool × List String)
    | [] => pure (false, processed.reverse)
    | tok :: rest =>
      match parseLongToken tok with
      | some (found, value?) =>
        if found = name then
          match value? with
          | some _ =>
            throw <| ParseError.mk .invalid s!"Flag --{name} does not accept a value" (some s!"--{name}") []
          | none =>
            let newFront := processed.reverse ++ rest
            pure (true, newFront)
        else
          loop (tok :: processed) rest
      | none =>
        loop (tok :: processed) rest
  match loop [] s.front with
  | .ok (found, front) => pure (found, s.withFront front)
  | .error err => .error err

def consumeShortFlag (s : ParseState) (name : Char) : Except ParseError (Bool × ParseState) :=
  let rec loop (processed : List String) : List String → Except ParseError (Bool × List String)
    | [] => pure (false, processed.reverse)
    | tok :: rest =>
      match parseShortToken tok with
      | some (found, value?) =>
        if found = name then
          match value? with
          | some _ =>
            throw <| ParseError.mk .invalid s!"Flag -{String.mk [name]} does not accept a value" (some s!"-{String.mk [name]}") []
          | none =>
            let newFront := processed.reverse ++ rest
            pure (true, newFront)
        else
          loop (tok :: processed) rest
      | none =>
        loop (tok :: processed) rest
  match loop [] s.front with
  | .ok (found, front) => pure (found, s.withFront front)
  | .error err => .error err

def consumeLongValue (s : ParseState) (name : String) : Except ParseError (Option String × ParseState) :=
  let rec loop (processed : List String) : List String → Except ParseError (Option (String × List String))
    | [] => pure none
    | tok :: rest =>
      match parseLongToken tok with
      | some (found, value?) =>
        if found = name then
          match value? with
          | some v =>
            let newFront := processed.reverse ++ rest
            pure (some (v, newFront))
          | none =>
            match rest with
            | valueTok :: restTail =>
              let newFront := processed.reverse ++ restTail
              pure (some (valueTok, newFront))
            | [] =>
              throw <| ParseError.mk .missing s!"Option --{name} expects a value" (some s!"--{name}") []
        else
          loop (tok :: processed) rest
      | none => loop (tok :: processed) rest
  match loop [] s.front with
  | .ok none => pure (none, s)
  | .ok (some (value, front)) => pure (some value, s.withFront front)
  | .error err => .error err

def consumeShortValue (s : ParseState) (name : Char) : Except ParseError (Option String × ParseState) :=
  let rec loop (processed : List String) : List String → Except ParseError (Option (String × List String))
    | [] => pure none
    | tok :: rest =>
      match parseShortToken tok with
      | some (found, value?) =>
        if found = name then
          match value? with
          | some v =>
            let newFront := processed.reverse ++ rest
            pure (some (v, newFront))
          | none =>
            match rest with
            | valueTok :: restTail =>
              let newFront := processed.reverse ++ restTail
              pure (some (valueTok, newFront))
            | [] =>
              throw <| ParseError.mk .missing s!"Option -{String.mk [name]} expects a value" (some s!"-{String.mk [name]}") []
        else
          loop (tok :: processed) rest
      | none => loop (tok :: processed) rest
  match loop [] s.front with
  | .ok none => pure (none, s)
  | .ok (some (value, front)) => pure (some value, s.withFront front)
  | .error err => .error err

def takePositional? (s : ParseState) : Option (String × ParseState) :=
  let rec loop (before : List String) : List String → Option (String × List String)
    | [] => none
    | tok :: rest =>
      if isOptionLike tok then
        loop (tok :: before) rest
      else
        let newFront := before.reverse ++ rest
        some (tok, newFront)
  match loop [] s.front with
  | some (tok, front) => some (tok, s.withFront front)
  | none =>
    match s.tail with
    | [] => none
    | tok :: tail => some (tok, s.withTail tail)

end ParseState

end LeanArgparse

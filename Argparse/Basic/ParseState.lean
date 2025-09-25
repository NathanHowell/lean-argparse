import Std
import Argparse.Basic.Error

namespace Argparse

open Std

/-- Parser state separates pre and post `--` segments. -/
structure ParseState where
  /-- Arguments that precede a `--` separator. -/
  front : List String
  /-- Arguments that follow a `--` separator. -/
  tail : List String
  deriving Repr

namespace ParseState

/-- Construct parser state from raw argument list. -/
def ofList (args : List String) : ParseState :=
  match args.span (· ≠ "--") with
  | (front, []) => { front := front, tail := [] }
  | (front, _ :: tail) => { front := front, tail := tail }

/-- Replace the pre-separator portion of the state. -/
def withFront (s : ParseState) (front : List String) : ParseState := { s with front }

/-- Replace the post-separator portion of the state. -/
def withTail (s : ParseState) (tail : List String) : ParseState := { s with tail }

/-- Reconstruct the remaining command-line arguments. -/
def remaining (s : ParseState) : List String :=
  match s.tail with
  | [] => s.front
  | tail => s.front ++ "--" :: tail

/-- Total number of remaining tokens. -/
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

/-- Adaptor describing how to interpret a token as a flag/option identifier. -/
class TokenSpec (α : Type) where
  /-- Attempt to parse a token into the logical identifier and optional value. -/
  parse : String → Option (α × Option String)
  /-- Pretty-print the identifier for diagnostic output. -/
  describe : α → String

instance : TokenSpec String where
  parse := parseLongToken
  describe := fun name => s!"--{name}"

instance : TokenSpec Char where
  parse := parseShortToken
  describe := fun name => s!"-{String.mk [name]}"

private def consumeFromFront [DecidableEq α] [TokenSpec α]
    (front : List String)
    (name : α)
    (handle : Option String → List String → Except ParseError (β × List String))
    : Except ParseError (Option (β × List String)) :=
  let rec loop (processed : List String) : List String → Except ParseError (Option (β × List String))
    | [] => pure none
    | tok :: rest =>
      match TokenSpec.parse tok with
      | some (found, value?) =>
        if found = name then
          match handle value? rest with
          | .ok (result, remaining) =>
            let newFront := processed.reverse ++ remaining
            pure (some (result, newFront))
          | .error err => .error err
        else
          loop (tok :: processed) rest
      | none => loop (tok :: processed) rest
  loop [] front

private def consumeFlag [DecidableEq α] [TokenSpec α]
    (s : ParseState) (name : α)
    : Except ParseError (Bool × ParseState) := do
  let flagName := TokenSpec.describe name
  let handle : Option String → List String → Except ParseError (Unit × List String)
    | some _, _ =>
      throw <| ParseError.mk .invalid s!"Flag {flagName} does not accept a value" (some flagName) []
    | none, rest => pure ((), rest)
  match ← consumeFromFront s.front name handle with
  | some ((), front) => pure (true, s.withFront front)
  | none => pure (false, s)

private def consumeValue [DecidableEq α] [TokenSpec α]
    (s : ParseState) (name : α)
    : Except ParseError (Option String × ParseState) := do
  let optionName := TokenSpec.describe name
  let handle : Option String → List String → Except ParseError (String × List String)
    | some v, rest => pure (v, rest)
    | none, rest =>
      match rest with
      | valueTok :: restTail => pure (valueTok, restTail)
      | [] =>
        throw <| ParseError.mk .missing s!"Option {optionName} expects a value" (some optionName) []
  match ← consumeFromFront s.front name handle with
  | some (value, front) => pure (some value, s.withFront front)
  | none => pure (none, s)

/-- Attempt to consume a long flag and report whether it was present. -/
def consumeLongFlag (s : ParseState) (name : String) : Except ParseError (Bool × ParseState) :=
  consumeFlag s name

/-- Attempt to consume a short flag and report whether it was present. -/
def consumeShortFlag (s : ParseState) (name : Char) : Except ParseError (Bool × ParseState) :=
  consumeFlag s name

/-- Attempt to consume the value associated with a long option. -/
def consumeLongValue (s : ParseState) (name : String) : Except ParseError (Option String × ParseState) :=
  consumeValue s name

/-- Attempt to consume the value associated with a short option. -/
def consumeShortValue (s : ParseState) (name : Char) : Except ParseError (Option String × ParseState) :=
  consumeValue s name

/-- Remove the next positional argument, if any. -/
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

end Argparse

import ArgParse.Core.Types

/-!
# ArgParse.Core.Value

`FromArg` typeclass and a handful of baseline instances.
-/

namespace ArgParse

universe u

/-- How far a value reaches when it is concatenated onto its short form, as
much of the decoder as can be said without running it.

`-n5v` is `5` then a `-v` flag for a `Nat` and the whole tail for a `String`,
and only the decoder knows which. The bundle pre-pass runs before any decoder is
in reach -- the items it reads are payload-free -- so each decoder states the
shape of its own values here instead, in data the pre-pass can act on.

`anything` is the safe answer and the default: a type that says it takes any
string is never split up front, which leaves the token for the option's own
scan, exactly as before this existed. -/
inductive ConcatFit where
  /-- The decoder accepts any string, so the whole tail is the value. -/
  | anything
  /-- The decoder accepts exactly a non-empty run of ASCII digits. -/
  | digits
deriving Repr, DecidableEq, Inhabited

/-- Convert a single token to a typed value for option/positional parsing. -/
class FromArg (α : Type u) where
  /-- Attempt to parse the token; failures are reported via a diagnostic string. -/
  parse : String → Except String α
  /-- Suggested metavariable name for help text. -/
  metavar : String := "VALUE"
  /-- Optional finite set of admissible values (for completions/docs). -/
  choices : Option (List String) := none
  /-- How far a value reaches when concatenated onto a short form. Overriding
  this is only sound when the decoder accepts exactly the stated shape. -/
  concatFit : ConcatFit := .anything

namespace FromArg

/-- Run the parser for a particular token. -/
@[inline] def run (s : String) [FromArg α] : Except String α :=
  FromArg.parse s

/-- Helper to lift an `Option` into the `Except` error channel. -/
def ofOption (failMsg : String) : Option α → Except String α
  | some a => .ok a
  | none => .error failMsg

/-- Normalise strings for case-insensitive comparisons. -/
private def lowercase (s : String) : String := s.toLower

/-- Build a `FromArg` instance for enumerations described by `(name, value)` pairs.

Names are matched case-insensitively while documentation retains the original
spelling from `xs`. Duplicate keys prefer the first occurrence. -/
@[reducible] def enumFrom (xs : List (String × α)) : FromArg α where
  parse input :=
    let needle := lowercase input
    let table := xs.map (fun pair => (lowercase pair.fst, pair.snd))
    match table.find? (fun entry : String × α => entry.fst = needle) with
    | some entry => .ok entry.snd
    | none =>
        let expectation := String.intercalate ", " (xs.map Prod.fst)
        .error s!"expected one of {expectation}, found '{input}'"
  metavar := "VALUE"
  choices := some (xs.map Prod.fst)

end FromArg

open FromArg

instance instFromArgString : FromArg String where
  parse s := .ok s
  metavar := "STRING"

instance instFromArgSubstring : FromArg Substring.Raw where
  parse s := .ok s.toRawSubstring
  metavar := "STRING"

private def natError (input : String) : String := s!"expected a natural number, found '{input}'"

instance instFromArgNat : FromArg Nat where
  parse s := FromArg.ofOption (natError s) s.toNat?
  metavar := "NAT"
  -- `String.toNat?` accepts exactly a non-empty digit run, so the longest
  -- decodable prefix of a tail is exactly its leading digits.
  concatFit := .digits

private def intError (input : String) : String := s!"expected an integer, found '{input}'"

instance instFromArgInt : FromArg Int where
  parse s := FromArg.ofOption (intError s) s.toInt?
  metavar := "INT"

private def boolError (input : String) : String := s!"expected one of true/false/1/0, found '{input}'"

private def parseBool (s : String) : Except String Bool :=
  match lowercase s with
  | "true" => .ok true
  | "false" => .ok false
  | "1" => .ok true
  | "0" => .ok false
  | _ => .error (boolError s)

instance instFromArgBool : FromArg Bool where
  parse := parseBool
  metavar := "BOOL"
  choices := some ["true", "false"]

end ArgParse

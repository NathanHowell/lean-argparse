import Argparse.Core.Types

/-!
# ArgParse.Core.Value

`FromArg` typeclass and a handful of baseline instances.
-/

namespace ArgParse

universe u

/-- Convert a single token to a typed value for option/positional parsing. -/
class FromArg (α : Type u) where
  /-- Attempt to parse the token; failures are reported via a diagnostic string. -/
  parse : String → Except String α
  /-- Suggested metavariable name for help text. -/
  metavar : String := "VALUE"
  /-- Optional finite set of admissible values (for completions/docs). -/
  choices : Option (List String) := none

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

end FromArg

open FromArg

instance instFromArgString : FromArg String where
  parse s := .ok s
  metavar := "STRING"

instance instFromArgSubstring : FromArg Substring where
  parse s := .ok s.toSubstring
  metavar := "STRING"

private def natError (input : String) : String := s!"expected a natural number, found '{input}'"

instance instFromArgNat : FromArg Nat where
  parse s := FromArg.ofOption (natError s) s.toNat?
  metavar := "NAT"

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

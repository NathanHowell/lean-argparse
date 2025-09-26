import Std

namespace Argparse
namespace Native
namespace Token

open Std

/-- Return the prefix and optional suffix after the first occurrence of `sep`. -/
private def splitOnFirst (sep : Char) (xs : List Char) : List Char × Option (List Char) :=
  let rec loop (acc : List Char) : List Char → List Char × Option (List Char)
    | [] => (acc.reverse, none)
    | c :: cs =>
      if c = sep then
        (acc.reverse, some cs)
      else
        loop (c :: acc) cs
  loop [] xs

/-- Classifies whether a token uses the long-option syntax (`--name` or `--name=value`). -/
def parseLong? (tok : String) : Option (String × Option String) :=
  match tok.data with
  | '-' :: '-' :: rest =>
      let (nameChars, valueChars?) := splitOnFirst '=' rest
      let name := String.mk nameChars
      if name.isEmpty then
        none
      else
        some (name, valueChars?.map String.mk)
  | _ => none

/-- Classifies whether a token uses the short-option syntax (`-n` or `-n=value`). -/
def parseShort? (tok : String) : Option (Char × Option String) :=
  match tok.data with
  | '-' :: [] => none
  | '-' :: '-' :: _ => none
  | '-' :: c :: rest =>
      match rest with
      | [] => some (c, none)
      | '=' :: tail => some (c, some (String.mk tail))
      | _ => some (c, some (String.mk rest))
  | _ => none

/-- Heuristic predicate used to decide whether a token looks like an option. -/
def isOptionLike (tok : String) : Bool :=
  match tok.data with
  | '-' :: '-' :: _ => true
  | '-' :: [] => false
  | '-' :: c :: _ => c.isAlpha
  | _ => false

/-- Adapter describing how to interpret tokens into logical identifiers. -/
class TokenSpec (α : Type) where
  parse : String → Option (α × Option String)
  describe : α → String

instance : TokenSpec String where
  parse := parseLong?
  describe := fun name => s!"--{name}"

instance : TokenSpec Char where
  parse := parseShort?
  describe := fun name => s!"-{String.mk [name]}"

end Token
end Native
end Argparse

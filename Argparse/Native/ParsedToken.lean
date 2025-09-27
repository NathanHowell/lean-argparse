import Argparse.Native.Token

namespace Argparse
namespace Native

/-- Canonical name for option-like tokens. -/
inductive ParsedName where
  | long (name : String)
  | short (name : Char)
  deriving DecidableEq, Repr

namespace ParsedName

@[simp] def toString : ParsedName → String
  | .long name => s!"--{name}"
  | .short name => s!"-{String.mk [name]}"

end ParsedName

/-- Representation of an option-like token together with any inline value. -/
structure ParsedOption where
  /-- Canonicalised name without the leading hyphen markers. -/
  name : ParsedName
  /-- Original token spelling as provided on the command line. -/
  original : String
  /-- Inline value supplied via `--opt=value` or `-oVALUE`. -/
  inlineValue? : Option String := none
  deriving DecidableEq, Repr

namespace ParsedOption

@[simp] def canonical (opt : ParsedOption) : String :=
  opt.name.toString

end ParsedOption

/-- Classified command-line tokens produced by `classify`. -/
inductive ParsedToken where
  | option (data : ParsedOption)
  | positional (value : String)
  deriving DecidableEq, Repr

namespace ParsedToken

@[simp] def original : ParsedToken → String
  | .option data => data.original
  | .positional value => value

end ParsedToken

/-- Buckets of option and positional tokens after normalisation. -/
structure ClassifiedTokens where
  options : List ParsedOption
  positionals : List String
  deriving Inhabited, Repr

namespace Internal

open Token

private def optionFromLong (tok : String) : Option ParsedOption := do
  let (name, inline?) ← Token.parseLong? tok
  some { name := .long name, original := tok, inlineValue? := inline? }

private def optionFromShort (tok : String) : Option ParsedOption := do
  let (name, inline?) ← Token.parseShort? tok
  some { name := .short name, original := tok, inlineValue? := inline? }

def classifyAux
    (positionalOnly : Bool)
    (optionsRev : List ParsedOption)
    (positionalsRev : List String)
    : List String → ClassifiedTokens
  | [] =>
      { options := optionsRev.reverse
        , positionals := positionalsRev.reverse }
  | tok :: rest =>
      if positionalOnly then
        classifyAux true optionsRev (tok :: positionalsRev) rest
      else if tok = "--" then
        classifyAux true optionsRev positionalsRev rest
      else
        match optionFromLong tok with
        | some opt => classifyAux positionalOnly (opt :: optionsRev) positionalsRev rest
        | none =>
            match optionFromShort tok with
            | some opt => classifyAux positionalOnly (opt :: optionsRev) positionalsRev rest
            | none => classifyAux true optionsRev (tok :: positionalsRev) rest

end Internal

/-- Traverse a raw CLI token list and classify each argument. -/
def classify (tokens : List String) : ClassifiedTokens :=
  Internal.classifyAux false [] [] tokens

end Native
end Argparse

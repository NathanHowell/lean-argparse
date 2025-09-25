import Std

namespace Argparse

open Std

/-- Classifies parser errors for richer downstream handling. -/
inductive ParseErrorKind
  | missing
  | invalid
  | unexpected
  deriving DecidableEq, Repr

/-- Structured error used throughout the parser pipeline. -/
structure ParseError where
  kind : ParseErrorKind
  message : String
  context? : Option String := none
  notes : List String := []
  deriving Repr

end Argparse

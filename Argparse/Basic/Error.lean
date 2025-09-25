import Std

namespace Argparse

open Std

/-- Classifies parser errors for richer downstream handling. -/
inductive ParseErrorKind
  /-- A required option, flag, or argument was not provided. -/
  | missing
  /-- A value was present but failed validation or parsing. -/
  | invalid
  /-- Extra input remained after parsing completed. -/
  | unexpected
  deriving DecidableEq, Repr

/-- Structured error used throughout the parser pipeline. -/
structure ParseError where
  /-- High-level classification of the parse failure. -/
  kind : ParseErrorKind
  /-- Human-readable explanation of what went wrong. -/
  message : String
  /-- Optional label describing the option/argument being parsed. -/
  context? : Option String := none
  /-- Additional lines of context that should accompany the error. -/
  notes : List String := []
  deriving Repr

end Argparse

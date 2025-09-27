/-!
# ArgParse.Core.Types

Core runtime types as prescribed by `SPEC.md`.
-/

namespace ArgParse

/-- Raw argv tokens before normalization. -/
abbrev Tokens := List String

/-- Parser state splits tokens around the `--` sentinel and tracks a flat cursor. -/
structure State where
  /-- Tokens that appear before the `--` sentinel (if any). -/
  pre    : List String
  /-- Tokens that appear after the `--` sentinel (pure positionals). -/
  post   : List String
  /-- Cursor over the flattened stream `pre ++ sentinel? ++ post`. -/
  cursor : Nat
deriving Repr, DecidableEq

/-- Error categories surfaced by the parser. -/
inductive ErrorKind where
  | unknownShort
  | unknownLong
  | missingValue
  | leftover
  | conflict
  | custom
deriving Repr, DecidableEq

/-- Expectations carried alongside errors for diagnostics. -/
inductive Expect where
  | flag (short? : Option Char) (long? : Option String)
  | optionVal (name : String)
  | positional (name : String)
  | subcommand (name : String)
  | endOfInput
deriving Repr, DecidableEq

/-- Structured, non-exceptional parse error. -/
structure Error where
  /-- What went wrong (unknown flag, missing value, …). -/
  kind    : ErrorKind
  /-- Nearby tokens to improve diagnostics. -/
  context : List String
  /-- Items the parser expected at the point of failure. -/
  expect  : List Expect
deriving Repr, DecidableEq

/-- Parser result: either a value with remaining state or a structured error. -/
inductive Result (α : Type) where
  | ok  : α → State → Result α
  | err : Error → Result α
deriving Repr, DecidableEq

end ArgParse

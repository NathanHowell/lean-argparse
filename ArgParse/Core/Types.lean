/-!
# ArgParse.Core.Types

Core runtime types: the parser state, the structured error vocabulary, and the
result type. Nothing above Layer 1 changes these.
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
  /-- A short flag was not recognised. -/
  | unknownShort
  /-- A long flag/option name was not recognised. -/
  | unknownLong
  /-- An option or positional was missing a required value. -/
  | missingValue
  /-- Tokens remained after parsing completed. -/
  | leftover
  /-- Two mutually exclusive flags/options were provided together. -/
  | conflict
  /-- Catch-all for domain-specific error reporting. -/
  | custom
deriving Repr, DecidableEq

/-- Expectations carried alongside errors for diagnostics. -/
inductive Expect where
  /-- Expect a boolean flag matching one of the provided names. -/
  | flag (short? : Option Char) (long? : Option String)
  /-- Expect an option value identified by name. -/
  | optionVal (name : String)
  /-- Expect a positional argument identified by name. -/
  | positional (name : String)
  /-- Expect a particular subcommand token. -/
  | subcommand (name : String)
  /-- Expect the end of input stream. -/
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
  /-- Successful parse carrying a value and remaining state. -/
  | ok  : α → State → Result α
  /-- Failed parse carrying a structured error. -/
  | err : Error → Result α
deriving Repr, DecidableEq

end ArgParse

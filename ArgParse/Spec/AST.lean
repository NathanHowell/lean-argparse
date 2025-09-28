import ArgParse.Core.Value

/-!
# ArgParse.Spec.AST

Single-source-of-truth specification AST following `SPEC.md`.
-/

universe u

namespace ArgParse.Spec

open ArgParse

/-- A short flag is any ASCII character other than `-` (supports `-0`). -/
structure Short where
  /-- Character used after the leading dash. -/
  c  : Char
  /-- Proof that the character is not `-` and fits within ASCII. -/
  ok : c ≠ '-' ∧ c.toNat < 128
deriving DecidableEq, Repr

/-- Common metadata shared by flags, options, and positionals. -/
structure Meta where
  /-- Canonical label used in documentation and diagnostics. -/
  name      : String
  /-- Short help text shown in synopsis tables. -/
  help?     : Option String := none
  /-- Long-form description used in detailed help/man pages. -/
  longHelp? : Option String := none
  /-- Placeholder text displayed for values in usage strings. -/
  metavar?  : Option String := none
  /-- Environment variable name consulted as an alternate default. -/
  env?      : Option String := none
  /-- Default value surfaced in documentation when present. -/
  default?  : Option String := none
deriving Inhabited, Repr, DecidableEq

/-- Number of values an item expects. -/
inductive Arity
  /-- No values are consumed (`--flag`). -/
  | zero
  /-- At most one value is consumed (`--opt value`). -/
  | one
  /-- Any number of values may be collected (`--repeat ...`). -/
  | many
  /-- One-or-more values must be collected (`--repeat ...` requiring at least one). -/
  | some
deriving Repr, DecidableEq

/-- Declarative description of a boolean flag. -/
structure FlagSpec where
  /-- Short-form identifier (`-x`) accepted for the flag, when present. -/
  short?     : Option Short := none
  /-- Long-form identifier (`--example`) accepted for the flag, when present. -/
  long?      : Option String := none
  /-- User-visible metadata (name/help text/environment variable hints). -/
  «meta»     : Meta
  /-- Whether this flag conflicts with other mutually exclusive entries. -/
  exclusive? : Bool := false
  /-- Whether the flag should be hidden from generated documentation. -/
  hidden?    : Bool := false
deriving Repr, DecidableEq

/-- Declarative description of an option that consumes values. -/
structure OptSpec (α : Type u) [ArgParse.FromArg α] where
  /-- Short-form identifier accepted for the option (`-x`). -/
  short?     : Option Short := none
  /-- Long-form identifier accepted for the option (`--example`). -/
  long?      : Option String := none
  /-- User-visible metadata shared with flags and positionals. -/
  «meta»     : Meta
  /-- Expected number of values gathered for the option. -/
  arity      : Arity := .one
  /-- Allow concatenated forms like `-ovalue` to satisfy the option. -/
  concatVal? : Bool := true
  /-- Allow `--opt=value` style syntax to satisfy the option. -/
  eqVal?     : Bool := true
  /-- Permit the option to be repeated (collecting many values). -/
  repeatable : Bool := (arity ≠ .one)
deriving Repr

/-- Declarative description of positional arguments. -/
structure PosSpec (α : Type u) [ArgParse.FromArg α] where
  /-- Metadata describing the positional argument in generated docs. -/
  «meta» : Meta
  /-- Expected number of values gathered for the positional. -/
  arity : Arity := .one
deriving Repr

/-- Items that may appear inside a command specification. -/
inductive ItemSpec : Type (u + 1) where
  /-- A boolean flag entry. -/
  | flag (spec : FlagSpec)
  /-- An option entry that parses one or more values. -/
  | opt {α : Type u} [ArgParse.FromArg α] (spec : OptSpec α)
  /-- A positional entry that consumes values from the argument list. -/
  | pos {α : Type u} [ArgParse.FromArg α] (spec : PosSpec α)

/-- Command tree: a node consists of local items and potential subcommands. -/
structure CmdSpec where
  /-- Command name used in documentation and subcommand dispatch. -/
  name : String
  /-- Metadata surfaced in help/man output (about text, etc.). -/
  «meta» : Meta
  /-- Items (flags/options/positionals) supported by the command. -/
  args : List ItemSpec := []
  /-- Nested subcommands available beneath this command. -/
  subs : List CmdSpec := []

/-- Application-level descriptor built from the command tree. -/
structure AppSpec where
  /-- Application name rendered in docs and errors. -/
  name     : String
  /-- Optional version string surfaced in `--version` style output. -/
  version? : Option String := none
  /-- Optional summary/description for the application. -/
  about?   : Option String := none
  /-- Optional epilog text appended to generated help/man pages. -/
  epilog?  : Option String := none
  /-- Root command specification describing flags/options/subcommands. -/
  root     : CmdSpec

end ArgParse.Spec

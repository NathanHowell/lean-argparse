/-!
# ArgParse.Spec.AST

Single-source-of-truth specification AST following `SPEC.md`.
-/

import Argparse.Core.Value

namespace ArgParse.Spec

open ArgParse

/-- A short flag is any ASCII character other than `-` (supports `-0`). -/
structure Short where
  c  : Char
  ok : c ≠ '-' ∧ c.toNat < 128
deriving DecidableEq

/-- Common metadata shared by flags, options, and positionals. -/
structure Meta where
  name      : String
  help?     : Option String := none
  longHelp? : Option String := none
  metavar?  : Option String := none
  env?      : Option String := none
  default?  : Option String := none
deriving Inhabited, Repr, DecidableEq

/-- Number of values an item expects. -/
inductive Arity
  | zero
  | one
  | many
  | some
deriving Repr, DecidableEq

/-- Declarative description of a boolean flag. -/
structure FlagSpec where
  short?     : Option Short := none
  long?      : Option String := none
  meta       : Meta
  exclusive? : Bool := false
  hidden?    : Bool := false
deriving Repr, DecidableEq

/-- Declarative description of an option that consumes values. -/
structure OptSpec (α : Type) [FromArg α] where
  short?     : Option Short := none
  long?      : Option String := none
  meta       : Meta
  arity      : Arity := .one
  concatVal? : Bool := true
  eqVal?     : Bool := true
  repeatable : Bool := (arity ≠ .one)
deriving Repr

/-- Declarative description of positional arguments. -/
structure PosSpec (α : Type) [FromArg α] where
  meta  : Meta
  arity : Arity := .one
deriving Repr

/-- Items that may appear inside a command specification. -/
inductive ItemSpec : Type where
  | flag (spec : FlagSpec)
  | opt {α : Type} [FromArg α] (spec : OptSpec α)
  | pos {α : Type} [FromArg α] (spec : PosSpec α)

/-- Command tree: a node consists of local items and potential subcommands. -/
structure CmdSpec where
  name : String
  meta : Meta
  args : List ItemSpec := []
  subs : List CmdSpec := []
deriving Repr

/-- Application-level descriptor built from the command tree. -/
structure AppSpec where
  name     : String
  version? : Option String := none
  about?   : Option String := none
  epilog?  : Option String := none
  root     : CmdSpec
deriving Repr

end ArgParse.Spec

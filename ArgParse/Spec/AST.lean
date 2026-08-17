import ArgParse.Core.Value

/-!
# ArgParse.Spec.AST

Two families of declarative types live here.

**Runtime specs** (`Short`, `Meta`, `Arity`, `FlagSpec`, `OptSpec`, `PosSpec`)
are the inputs Layer 1's combinators consume. They are typed: `OptSpec α`
carries the `FromArg α` instance used to decode a token.

**The render model** (`ItemSpec`, `CmdSpec`, `AppSpec`) is what the help, man,
and completion renderers read. It is payload-free — the value type is erased
down to a metavar string and an optional choice list. That erasure is load
bearing: it keeps `Doc` (and therefore `P α`) in `Type` with no universe bump,
and it means one item type serves both the renderers and the correspondence
theorems.

Nothing in this file is written by hand at the application level. Layer 3
builds a runtime spec and an `ItemSpec` from the same arguments; Layer 4's
`Cmd.toCmdSpec` produces the `CmdSpec`.
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
  /-- Text shown as the metavariable placeholder in usage strings. -/
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
deriving Repr, DecidableEq, Inhabited

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
  /-- Whether the option should be hidden from generated documentation. -/
  hidden?    : Bool := false
deriving Repr

/-- Declarative description of positional arguments. -/
structure PosSpec (α : Type u) [ArgParse.FromArg α] where
  /-- Metadata describing the positional argument in generated docs. -/
  «meta» : Meta
  /-- Expected number of values gathered for the positional. -/
  arity : Arity := .one
  /-- Whether the positional should be hidden from generated documentation. -/
  hidden? : Bool := false
deriving Repr

/-! ### The render model -/

/-- Which surface syntax an item presents. -/
inductive ItemKind
  /-- A boolean flag (`--verbose`). -/
  | flag
  /-- An option consuming values (`--count N`). -/
  | option
  /-- A positional argument (`NAME`). -/
  | positional
deriving Repr, DecidableEq, Inhabited

/-- One flag, option, or positional with its value type erased.

This is the leaf of both `Doc` and `CmdSpec`. Everything a renderer or a
completion script needs about an item is here as plain data; nothing about how
its value is decoded is. -/
structure ItemSpec where
  /-- Surface syntax presented by the item. -/
  kind      : ItemKind
  /-- Canonical label, matching the runtime spec's `Meta.name`. -/
  name      : String
  /-- Short-form character, when the item accepts one. -/
  short?    : Option Char := none
  /-- Long-form name, when the item accepts one. -/
  long?     : Option String := none
  /-- Placeholder shown where the item's value goes. -/
  metavar?  : Option String := none
  /-- One-line description. -/
  help?     : Option String := none
  /-- How many values the item consumes. -/
  arity     : Arity := .one
  /-- Enumerated admissible values, when the value type supplies them. -/
  choices?  : Option (List String) := none
  /-- Default rendered in help when the item is absent. -/
  default?  : Option String := none
  /-- Whether omitting the item is an error.

  Optionality is structural in `Doc` -- it is `alt [item, none]` -- but
  `Doc.items` flattens that structure away, and usage synopses need to know
  whether to bracket the item. The builders set this field in the same body
  that chooses the `Doc` shape, so the two agree by construction. -/
  required  : Bool := true
  /-- Whether the item is omitted from generated documentation. -/
  hidden    : Bool := false
deriving Repr, DecidableEq, Inhabited

namespace ItemSpec

/-- Surface lexemes the item answers to. Order is irrelevant: this list is for
matching, not display. -/
def lexemes (item : ItemSpec) : List String :=
  match item.kind with
  | .positional => [item.name]
  | _ =>
      item.long?.toList.map (fun name => "--" ++ name) ++
        item.short?.toList.map (fun c => "-" ++ String.singleton c)

/-- Lexemes in the order documentation shows them: short form first, the
convention every other CLI follows. -/
def displayLexemes (item : ItemSpec) : List String :=
  match item.kind with
  | .positional => [item.name]
  | _ =>
      item.short?.toList.map (fun c => "-" ++ String.singleton c) ++
        item.long?.toList.map (fun name => "--" ++ name)

/-- Placeholder text for the item's value, falling back to the upper-cased name. -/
def metavar (item : ItemSpec) : String :=
  item.metavar?.getD item.name.toUpper

/-- The single lexeme a usage synopsis shows, preferring the long form because
it reads. -/
def synopsisLexeme (item : ItemSpec) : String :=
  match item.kind with
  | .positional => item.metavar
  | _ => (item.long?.map (fun n => "--" ++ n)).getD
      ((item.short?.map (fun c => "-" ++ String.singleton c)).getD item.name)

end ItemSpec

/-- Command tree used by the renderers: a name, metadata, local items, and
subcommands. Produced by `Cmd.toCmdSpec`, never written by hand.

This is an `inductive` rather than a `structure` on purpose. A structure whose
field recurses through `List CmdSpec` admits no structural measure, so every
renderer over it would have to be `partial` — and Layer 6 asks for rendering
totality on every constructible tree. Matching on the constructor gives Lean the
nested recursion it needs; the projections below restore field syntax. -/
inductive CmdSpec where
  /-- A command with its name, metadata, local items, and subcommands. -/
  | mk (name : String) («meta» : Meta) (args : List ItemSpec) (subs : List CmdSpec)

namespace CmdSpec

/-- Command name used in documentation and subcommand dispatch. -/
@[inline] def name : CmdSpec → String
  | .mk n _ _ _ => n

/-- Metadata surfaced in help/man output (about text, etc.). -/
@[inline] def «meta» : CmdSpec → Meta
  | .mk _ m _ _ => m

/-- Items (flags/options/positionals) supported by the command. -/
@[inline] def args : CmdSpec → List ItemSpec
  | .mk _ _ a _ => a

/-- Nested subcommands available beneath this command. -/
@[inline] def subs : CmdSpec → List CmdSpec
  | .mk _ _ _ s => s

end CmdSpec

/-- Application-level descriptor wrapping the root command with the facts the
runner needs but a single command does not carry. -/
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

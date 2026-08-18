import ArgParse.Core.Value

/-!
# ArgParse.Spec.Item

Two families of declarative types live here, both below the description tree.

**Runtime specs** (`Short`, `Meta`, `Arity`, `FlagSpec`, `OptSpec`, `PosSpec`)
are the inputs Layer 1's combinators consume. They are typed: `OptSpec α`
carries the `FromArg α` instance used to decode a token.

**`ItemSpec`** is the leaf of the render model: one flag, option, or positional,
payload-free — the value type erased down to a metavar string, an optional
choice list, and the shape its concatenated values take. That erasure is load bearing: it keeps `Doc` (and therefore `P α`)
in `Type` with no universe bump, and it means one item type serves both the
renderers and the correspondence theorems.

`Doc` builds on this file and `ArgParse.Spec.AST` builds on `Doc`, which is why
the render model is split across the two: `CmdSpec` carries a description tree,
and the tree is made of these items.

Nothing here is written by hand at the application level. Layer 3 builds a
runtime spec and an `ItemSpec` from the same arguments.
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

/-- Number of values an item expects.

Repeating an item is never an error. `.one` takes the last value supplied, so
`--name a --name b` is `b`; `.many` and `.some` accumulate every value in the
order the tokens appeared. -/
inductive Arity
  /-- No values are consumed (`--flag`). -/
  | zero
  /-- One value is carried (`--opt value`), the last one when several are given. -/
  | one
  /-- Any number of values may be collected (`--repeat ...`), in order. -/
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
  /-- How far this item's value reaches when concatenated onto its short form.

  The one thing about decoding that has to survive erasure: the bundle pre-pass
  runs before any decoder is in reach, and `-n5v` cannot be split without
  knowing whether `5v` is a value or a value and a flag. It is a statement of
  shape rather than a decoder, so `ItemSpec` stays plain data. -/
  concatFit : ConcatFit := .anything
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

/-- Whether this item consumes the token that follows it.

A flag consumes nothing, and the packed `--name=value` and `-nvalue` forms are
a single token, so only a value-taking option in its detached spelling does. -/
def takesValue (item : ItemSpec) : Bool :=
  item.kind == .option && item.arity != .zero

/-- Lexemes in the order documentation shows them: short form first, the
convention every other CLI follows. -/
def displayLexemes (item : ItemSpec) : List String :=
  match item.kind with
  | .positional => [item.name]
  | _ =>
      item.short?.toList.map (fun c => "-" ++ String.singleton c) ++
        item.long?.toList.map (fun name => "--" ++ name)

/-- The default as documentation shows it.

An empty default is quoted, because `default: ` reads as "no default" when it
means "the empty string" -- and the two are different, since `default?` is an
`Option`. -/
def defaultText (item : ItemSpec) : Option String :=
  item.default?.map (fun d => if d.isEmpty then "\"\"" else d)

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

/-- Lexemes of the items that consume the token after them.

Used by every walk that crosses a command's own arguments looking for the next
verb: an option's *value* is not in verb position, however it happens to be
spelled. -/
def valueLexemes (items : List ItemSpec) : List String :=
  (items.filter (·.takesValue)).flatMap (·.lexemes)

end ArgParse.Spec

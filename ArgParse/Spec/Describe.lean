import ArgParse.Spec.AST

/-!
# ArgParse.Spec.Describe

Scaffold for deriving documentation artefacts from the specification AST.
-/

namespace ArgParse.Spec

/-- Simplified documentation entry produced from parts of the spec. -/
inductive EntryKind
  /-- Entry describing a command or subcommand. -/
  | command
  /-- Entry describing a boolean flag. -/
  | flag
  /-- Entry describing an option that accepts values. -/
  | option
  /-- Entry describing a positional argument. -/
  | positional
deriving Repr, DecidableEq, Inhabited

/-- Documentation payload emitted for one spec element. -/
structure DocEntry where
  /-- Heading displayed for the entry (name or command title). -/
  heading : String
  /-- Description lines associated with the entry. -/
  lines   : List String := []
  /-- The classification of the entry (command/flag/option/positional). -/
  kind    : EntryKind := .command
deriving Repr, Inhabited

/-- Convert metadata help text into a documentation entry. -/
@[inline] def entryOfMeta (info : Meta) (kind : EntryKind := .command) : DocEntry :=
  { heading := info.name, lines := info.help?.toList, kind := kind }

/-- Describe a single item in terms of documentation entries. -/
@[inline] def describeItem : ItemSpec → List DocEntry
  | .flag spec => [entryOfMeta spec.«meta» .flag]
  | @ItemSpec.opt _ _ spec => [entryOfMeta spec.«meta» .option]
  | @ItemSpec.pos _ _ spec => [entryOfMeta spec.«meta» .positional]

/-- Describe an entire command, including child subcommands. -/
partial def describeCommand (cmd : CmdSpec) : List DocEntry :=
  let selfEntry := entryOfMeta cmd.«meta» .command
  let itemEntries :=
    cmd.args.foldr (fun item acc => describeItem item ++ acc) []
  let subEntries :=
    cmd.subs.foldr (fun sub acc => describeCommand sub ++ acc) []
  selfEntry :: itemEntries ++ subEntries

/-- Produce documentation entries for the whole application specification. -/
partial def describeApp (spec : AppSpec) : List DocEntry :=
  let header : DocEntry := { heading := spec.name, lines := spec.about?.toList }
  header :: describeCommand spec.root

end ArgParse.Spec

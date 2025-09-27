import Argparse.Spec.AST

/-!
# ArgParse.Spec.Describe

Scaffold for deriving documentation artefacts from the specification AST.
-/

namespace ArgParse.Spec

/-- Simplified documentation entry produced from parts of the spec. -/
inductive EntryKind
  | command
  | flag
  | option
  | positional
deriving Repr, DecidableEq

structure DocEntry where
  heading : String
  lines   : List String := []
  kind    : EntryKind := .command
deriving Repr, Inhabited

/-- Convert metadata help text into a documentation entry. -/
def entryOfMeta (meta : Meta) (kind : EntryKind := .command) : DocEntry :=
  { heading := meta.name, lines := meta.help?.toList, kind := kind }

/-- Describe a single item in terms of documentation entries. -/
def describeItem : ItemSpec → List DocEntry
  | .flag spec => [entryOfMeta spec.meta .flag]
  | .opt spec => [entryOfMeta spec.meta .option]
  | .pos spec => [entryOfMeta spec.meta .positional]

/-- Describe an entire command, including child subcommands. -/
def describeCommand (cmd : CmdSpec) : List DocEntry :=
  let selfEntry := entryOfMeta cmd.meta .command
  let itemEntries := cmd.args.bind describeItem
  let subEntries := cmd.subs.bind describeCommand
  selfEntry :: itemEntries ++ subEntries

/-- Produce documentation entries for the whole application specification. -/
def describeApp (spec : AppSpec) : List DocEntry :=
  let header := DocEntry.mk spec.name (spec.about?.toList)
  header :: describeCommand spec.root

end ArgParse.Spec

import ArgParse.Spec.AST

/-!
# ArgParse.Spec.Describe

Flattens the render model into documentation entries. Every function here is a
total, pure function of `CmdSpec`/`AppSpec`: no parsed values reach a renderer,
so help has exactly one data path and the correspondence theorems cover it.
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
deriving Repr, Inhabited, DecidableEq

/-- Entry classification matching an item's surface syntax. -/
@[inline] def entryKindOfItem : ItemKind → EntryKind
  | .flag => .flag
  | .option => .option
  | .positional => .positional

/-- Convert metadata help text into a documentation entry. -/
@[inline] def entryOfMeta (info : Meta) (kind : EntryKind := .command) : DocEntry :=
  { heading := info.name, lines := info.help?.toList, kind := kind }

/-- Heading for an item: its surface lexemes plus a metavar where one applies. -/
def itemHeading (item : ItemSpec) : String :=
  let names := String.intercalate ", " item.lexemes
  match item.kind with
  | .flag => names
  | _ => s!"{names} {item.metavar}"

/-- Supplementary lines describing defaults and enumerated choices. -/
def itemDetails (item : ItemSpec) : List String :=
  item.help?.toList
    ++ (item.default?.toList.map fun d => s!"default: {d}")
    ++ (item.choices?.toList.map fun cs => s!"choices: {String.intercalate ", " cs}")

/-- Describe a single item as one documentation entry. -/
@[inline] def describeItem (item : ItemSpec) : DocEntry :=
  { heading := itemHeading item
  , lines := itemDetails item
  , kind := entryKindOfItem item.kind }

/-- Items a renderer should show, dropping the hidden ones. -/
@[inline] def visibleItems (items : List ItemSpec) : List ItemSpec :=
  items.filter (fun item => !item.hidden)

mutual

/-- Describe an entire command, including child subcommands. -/
def describeCommand : CmdSpec → List DocEntry
  | .mk _ info args subs =>
      entryOfMeta info .command
        :: ((visibleItems args).map describeItem ++ describeCommands subs)

/-- Describe each command in a list, in order. -/
def describeCommands : List CmdSpec → List DocEntry
  | [] => []
  | cmd :: rest => describeCommand cmd ++ describeCommands rest

end

/-- Produce documentation entries for the whole application specification. -/
def describeApp (spec : AppSpec) : List DocEntry :=
  let header : DocEntry := { heading := spec.name, lines := spec.about?.toList }
  header :: describeCommand spec.root

end ArgParse.Spec

import ArgParse.Spec.Describe

/-!
# ArgParse.Doc.Usage

Usage synopses, and the shared item-label formatting the help and man renderers
both use. Pure functions of the render model.
-/

namespace ArgParse.Doc

open ArgParse.Spec

/-- Pad `s` on the right to at least `width` characters. -/
def padTo (width : Nat) (s : String) : String :=
  if s.length < width then s ++ String.ofList (List.replicate (width - s.length) ' ') else s

/-- Bracket `s` when the item it describes may be omitted. -/
@[inline] def bracketIf (optional : Bool) (s : String) : String :=
  if optional then "[" ++ s ++ "]" else s

/-- Ellipsis marker for items that may repeat. -/
@[inline] def repeatMark (arity : Arity) : String :=
  match arity with
  | .many | .some => "..."
  | _ => ""

/-- How an item appears inside a usage synopsis. -/
def itemSynopsis (item : ItemSpec) : String :=
  let core :=
    match item.kind with
    | .flag => item.synopsisLexeme
    | .option => s!"{item.synopsisLexeme} {item.metavar}"
    | .positional => item.metavar
  bracketIf (!item.required) (core ++ repeatMark item.arity)

/-- The label shown in the left column of a help or man entry: every accepted
lexeme, with the metavar where the item takes a value. -/
def itemLabel (item : ItemSpec) : String :=
  let names := String.intercalate ", " item.displayLexemes
  match item.kind with
  | .flag => names
  | .option => s!"{names} {item.metavar}"
  | .positional => item.metavar

/-- Trailing notes for an item: its default and its admissible values. -/
def itemNotes (item : ItemSpec) : String :=
  let parts :=
    (item.default?.toList.map fun d => s!"[default: {d}]") ++
      (item.choices?.toList.map fun cs => s!"[choices: {String.intercalate "|" cs}]")
  match parts with
  | [] => ""
  | _ => " " ++ String.intercalate " " parts

/-- Usage synopsis for one command, reached by `path`.

Options are summarised as `[OPTIONS]` once there are more than three of them,
which is the usual convention and keeps the line readable. -/
def usageLine (path : List String) (cmd : CmdSpec) : String :=
  let visible := visibleItems cmd.args
  let positionals := visible.filter (fun i => i.kind == .positional)
  let switches := visible.filter (fun i => i.kind != .positional)
  let switchPart :=
    if switches.length > 3 then ["[OPTIONS]"]
    else switches.map itemSynopsis
  let verbPart := if cmd.subs.isEmpty then [] else ["<COMMAND>"]
  let words := path ++ switchPart ++ positionals.map itemSynopsis ++ verbPart
  "  " ++ String.intercalate " " words

end ArgParse.Doc

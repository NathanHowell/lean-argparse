import ArgParse.Doc.Usage

/-!
# ArgParse.Doc.Help

The `--help` page, rendered from the command the user actually named.

Help for a subcommand is help for *that* command: its own items, its own
children, its own usage line under the full invocation path. Nothing here reads
parsed values, so there is exactly one data path into help.
-/

namespace ArgParse.Doc

open ArgParse.Spec

/-- Column at which descriptions start. -/
private def descrColumn : Nat := 26

/-- One `label  description` row.

A label that reaches the description column gets two spaces instead of running
into the text. Padding alone is not enough: `padTo` is a minimum, so a long
enough label -- `--completion-script SHELL` is one -- would otherwise abut its
description with no separator at all. -/
def entryRow (label : String) (descr : String) : String :=
  let cell := "  " ++ label
  let padded := if cell.length + 2 ≤ descrColumn then padTo descrColumn cell else cell ++ "  "
  if descr.isEmpty then padded.trimAsciiEnd.toString else padded ++ descr

/-- Render a titled block, or nothing when it has no rows. -/
def block (title : String) (rows : List String) : List String :=
  match rows with
  | [] => []
  | _ => [title ++ ":\n" ++ String.intercalate "\n" rows]

/-- Rows describing a command's items of one kind. -/
def itemRows (items : List ItemSpec) : List String :=
  items.map fun item =>
    entryRow (itemLabel item) ((item.help?.getD "") ++ itemNotes item)

/-- Rows describing a command's children. -/
def subRows (subs : List CmdSpec) : List String :=
  subs.map fun sub => entryRow sub.name (sub.«meta».help?.getD "")

/-- Render the help page for `cmd`, invoked as `path`.

`extra` holds the runner's own items (`--help`, `--version`, …), which belong in
the options table but are not part of any application's parser.

`globals` holds items declared on ancestors of this command. They are legal here
-- that is what a node's globals mean -- so a help page that omitted them would
be documenting less than the parser accepts. They get their own section rather
than being mixed in, because where they may appear differs: before the verb. -/
def renderCommandHelp (path : List String) (cmd : CmdSpec)
    (extra : List ItemSpec := []) (about? : Option String := none)
    (epilog? : Option String := none) (globals : List ItemSpec := []) : String :=
  let visible := visibleItems cmd.args
  let positionals := visible.filter (fun i => i.kind == .positional)
  let switches := visible.filter (fun i => i.kind != .positional)
  let title :=
    let descr := (about?.orElse (fun _ => cmd.«meta».help?)).getD ""
    if descr.isEmpty then String.intercalate " " path
    else s!"{String.intercalate " " path} - {descr}"
  let blocks :=
    [title]
      ++ block "Usage" [usageLine path cmd]
      ++ block "Arguments" (itemRows positionals)
      ++ block "Options" (itemRows (switches ++ extra))
      ++ block "Global options" (itemRows (visibleItems globals))
      ++ block "Commands" (subRows cmd.subs)
      ++ epilog?.toList
  String.intercalate "\n\n" blocks

/-- Render help for a whole application descriptor. -/
def renderHelp (spec : AppSpec) (extra : List ItemSpec := []) : String :=
  renderCommandHelp [spec.name] spec.root extra spec.about? spec.epilog?

end ArgParse.Doc

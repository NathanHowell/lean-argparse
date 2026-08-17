import ArgParse.Doc.Usage

/-!
# ArgParse.Doc.Man

mdoc-style man page over the render model. Same data as `--help`, different
shape, no second source of truth.
-/

namespace ArgParse.Doc

open ArgParse.Spec

/-- One `.It` entry for an item. -/
def manItem (item : ItemSpec) : String :=
  let descr := (item.help?.getD "") ++ itemNotes item
  s!".It {itemLabel item}\n{descr}"

mutual

/-- A `.Sh` section for a command and, recursively, its children.

Matches on the constructor rather than projecting `.subs`, which is what lets
Lean see the recursion through `List CmdSpec` as structural. -/
def manCommand (path : List String) : CmdSpec → List String
  | .mk name info args subs =>
      let cmd := CmdSpec.mk name info args subs
      let visible := visibleItems args
      let heading := s!".Sh {String.intercalate " " (path.map String.toUpper)}"
      let synopsis := s!".Nm {String.intercalate " " path}\n{usageLine path cmd}"
      let descr := info.help?.toList.map (fun line => s!".Pp {line}")
      let items :=
        match visible with
        | [] => []
        | _ => [".Bl -tag -width Ds\n" ++ String.intercalate "\n" (visible.map manItem) ++ "\n.El"]
      (heading :: synopsis :: descr ++ items) ++ manCommands path subs

/-- `manCommand` over a list of children, extending the path. -/
def manCommands (path : List String) : List CmdSpec → List String
  | [] => []
  | cmd :: rest => manCommand (path ++ [cmd.name]) cmd ++ manCommands path rest

end

/-- Render a man page for the application. -/
def renderMan (spec : AppSpec) : String :=
  let header := s!".Dd Generated\n.Dt {spec.name.toUpper} 1\n.Os"
  let about := spec.about?.toList.map (fun line => s!".Pp {line}")
  String.intercalate "\n"
    ((header :: about) ++ manCommand [spec.name] spec.root ++ spec.epilog?.toList.map (fun e => s!".Pp {e}"))

end ArgParse.Doc

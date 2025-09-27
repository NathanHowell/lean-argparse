import Argparse.Spec.Describe
import Argparse.Spec.Elab

/-!
# ArgParse.Doc.Help

Placeholder help renderer that consumes describer output.
-/

namespace ArgParse.Doc

open ArgParse.Spec

namespace

open Spec.EntryKind

def runtimeLinesFor (partial? : Option Spec.Partial) (entry : DocEntry) : List String :=
  match partial? with
  | none => []
  | some partial =>
      match entry.kind with
      | .flag =>
          match partial.flagValue? entry.heading with
          | some true => ["current: enabled"]
          | some false => ["current: disabled"]
          | none => []
      | .option =>
          let values := partial.optionValues entry.heading
          if values.isEmpty then []
          else [s!"current: {String.intercalate ", " values}"]
      | .positional =>
          let values := partial.positionalValues entry.heading
          if values.isEmpty then []
          else [s!"current: {String.intercalate ", " values}"]
      | .command => []

end

/-- Render a single documentation entry into a human-readable block. -/
def renderEntryWith (entry : DocEntry) (partial? : Option Spec.Partial := none) : String :=
  let body := entry.lines.map (fun line => s!"  {line}")
  let runtime := runtimeLinesFor partial? entry |>.map (fun line => s!"  {line}")
  String.intercalate "\n" (entry.heading :: body ++ runtime)

/-- Render help text for the entire application specification. -/
def renderHelpWith (spec : AppSpec) (partial? : Option Spec.Partial := none) : String :=
  let entries := describeApp spec
  String.intercalate "\n\n" (entries.map (fun entry => renderEntryWith entry partial?))

/-- Render help text without runtime annotations. -/
def renderHelp (spec : AppSpec) : String :=
  renderHelpWith spec none

end ArgParse.Doc

import ArgParse.Spec.Describe
import ArgParse.Spec.Elab

/-!
# ArgParse.Doc.Help

Placeholder help renderer that consumes describer output.
-/

namespace ArgParse.Doc

open ArgParse.Spec

open ArgParse.Spec.EntryKind

/-- Runtime annotations describing the current values associated with the entry. -/
def runtimeLinesForSummary (summary? : Option Spec.Partial.Summary) (entry : DocEntry) :
    List String :=
  match summary? with
  | none => []
  | some summary =>
      match entry.kind with
      | .flag =>
          match summary.flagValue? entry.heading with
          | some true => ["current: enabled"]
          | some false => ["current: disabled"]
          | none => []
      | .option =>
          let values := summary.optionValues entry.heading
          if values.isEmpty then []
          else [s!"current: {String.intercalate ", " values}"]
      | .positional =>
          let values := summary.positionalValues entry.heading
          if values.isEmpty then []
          else [s!"current: {String.intercalate ", " values}"]
      | .command => []

/-- Render a single documentation entry into a human-readable block. -/
def renderEntryWithSummary (entry : DocEntry)
    (summary? : Option Spec.Partial.Summary := none) : String :=
  let body := entry.lines.map (fun line => s!"  {line}")
  let runtime := runtimeLinesForSummary summary? entry |>.map (fun line => s!"  {line}")
  String.intercalate "\n" (entry.heading :: body ++ runtime)

/-- Render a single documentation entry given a raw `Partial`. -/
def renderEntryWith (entry : DocEntry) (partial? : Option Spec.Partial := none) : String :=
  renderEntryWithSummary entry (partial?.map Partial.toSummary)

/-- Render help text for the entire application specification. -/
def renderHelpWithSummary (spec : AppSpec)
    (summary? : Option Spec.Partial.Summary := none) : String :=
  let entries := describeApp spec
  String.intercalate "\n\n" (entries.map (fun entry => renderEntryWithSummary entry summary?))

/-- Render help text for the entire application specification. -/
def renderHelpWith (spec : AppSpec) (partial? : Option Spec.Partial := none) : String :=
  renderHelpWithSummary spec (partial?.map Partial.toSummary)

/-- Render help text without runtime annotations. -/
def renderHelp (spec : AppSpec) : String :=
  renderHelpWith spec none

end ArgParse.Doc

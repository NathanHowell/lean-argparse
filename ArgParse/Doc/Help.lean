import ArgParse.Spec.Describe
import ArgParse.Spec.Elab
import ArgParse.Doc.Runtime

/-!
# ArgParse.Doc.Help

Basic help renderer that consumes describer output and optionally threads
summary information captured at runtime.
-/

namespace ArgParse.Doc

open ArgParse.Spec

open ArgParse.Spec.EntryKind

/-- Runtime annotations describing the current values associated with the entry. -/
def runtimeLinesForSummary (summary? : Option Spec.Partial.Summary) (entry : DocEntry) :
    List String :=
  runtimeAnnotations id summary? entry

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

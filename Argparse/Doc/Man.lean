import Argparse.Spec.Describe
import Argparse.Spec.Elab

/-!
# ArgParse.Doc.Man

Placeholder manpage emitter consuming describer output.
-/

namespace ArgParse.Doc

open ArgParse.Spec

open ArgParse.Spec.EntryKind

/-- Runtime annotations inserted into the rendered manpage for the entry. -/
def runtimeParagraphs (summary? : Option Spec.Partial.Summary) (entry : DocEntry) :
    List String :=
  match summary? with
  | none => []
  | some summary =>
      match entry.kind with
      | .flag =>
          match summary.flagValue? entry.heading with
          | some true => [".Pp current: enabled"]
          | some false => [".Pp current: disabled"]
          | none => []
      | .option =>
          let values := summary.optionValues entry.heading
          if values.isEmpty then []
          else [s!".Pp current: {String.intercalate ", " values}"]
      | .positional =>
          let values := summary.positionalValues entry.heading
          if values.isEmpty then []
          else [s!".Pp current: {String.intercalate ", " values}"]
      | .command => []

/-- Render a minimal mdoc-style section for a documentation entry. -/
def renderSectionWithSummary (entry : DocEntry)
    (summary? : Option Spec.Partial.Summary := none) : String :=
  let heading := s!".Sh {entry.heading}"
  let lines := entry.lines.map (fun line => s!".Pp {line}")
  let runtime := runtimeParagraphs summary? entry
  String.intercalate "\n" (heading :: lines ++ runtime)

/-- Render a minimal section using raw partial data. -/
def renderSectionWith (entry : DocEntry) (partial? : Option Spec.Partial := none) : String :=
  renderSectionWithSummary entry (partial?.map Partial.toSummary)

/-- Render a basic mdoc document for the application spec. -/
def renderManWithSummary (spec : AppSpec)
    (summary? : Option Spec.Partial.Summary := none) : String :=
  let header := s!".Dd Generated\n.Dt {spec.name}\n.Os"
  let sections := describeApp spec |>.map (fun entry => renderSectionWithSummary entry summary?)
  String.intercalate "\n" (header :: sections)

/-- Render a manpage with optional raw partial annotations. -/
def renderManWith (spec : AppSpec) (partial? : Option Spec.Partial := none) : String :=
  renderManWithSummary spec (partial?.map Partial.toSummary)

/-- Render a manpage without runtime annotations. -/
def renderMan (spec : AppSpec) : String :=
  renderManWith spec none

end ArgParse.Doc

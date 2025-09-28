import ArgParse.Spec.Describe
import ArgParse.Spec.Elab

/-!
# ArgParse.Doc.Completion

Placeholder completion emitters built from describer output.
-/

namespace ArgParse.Doc

open ArgParse.Spec

/-- Naive completion suggestions derived from the spec headings and runtime state. -/
def suggestionsWithSummary (spec : AppSpec)
    (summary? : Option Spec.Partial.Summary := none) : List String :=
  let base := describeApp spec |>.map (·.heading)
  let extras :=
    match summary? with
    | none => []
    | some summary =>
        let flagNames := summary.flags.map (·.fst)
        let optionTerms := summary.options.foldr
          (fun entry acc =>
            match entry with
            | (name, values) =>
                values.foldr (fun value acc' => s!"{name}={value}" :: acc') acc)
          []
        let positionalTerms := summary.positionals.foldr
          (fun entry acc =>
            match entry with
            | (name, values) =>
                values.foldr (fun value acc' => s!"{name}:{value}" :: acc') acc)
          []
        flagNames ++ optionTerms ++ positionalTerms
  (base ++ extras).eraseDups

/-- Naive completion suggestions derived from a raw `Partial`. -/
def suggestionsWith (spec : AppSpec) (partial? : Option Spec.Partial := none) : List String :=
  suggestionsWithSummary spec (partial?.map Partial.toSummary)

/-- Render a simple newline-separated completion list. -/
def renderCompletionWith (spec : AppSpec) (partial? : Option Spec.Partial := none) : String :=
  String.intercalate "\n" (suggestionsWith spec partial?)

/-- Render completions using a payload summary. -/
def renderCompletionWithSummary (spec : AppSpec)
    (summary? : Option Spec.Partial.Summary := none) : String :=
  String.intercalate "\n" (suggestionsWithSummary spec summary?)

/-- Render completions without runtime annotations. -/
def renderCompletion (spec : AppSpec) : String :=
  renderCompletionWith spec none

end ArgParse.Doc

import Argparse.Spec.Describe
import Argparse.Spec.Elab

/-!
# ArgParse.Doc.Completion

Placeholder completion emitters built from describer output.
-/

namespace ArgParse.Doc

open ArgParse.Spec

/-- Naive completion suggestions derived from the spec headings and runtime state. -/
def suggestionsWith (spec : AppSpec) (partial? : Option Spec.Partial := none) : List String :=
  let base := describeApp spec |>.map (·.heading)
  let extras :=
    match partial? with
    | none => []
    | some partial =>
        let flagNames := partial.flags.map fun (name, _) => name
        let optionTerms := partial.options.map fun (name, value) => s!"{name}={value}"
        let positionalTerms := partial.positionals.map fun (name, value) => s!"{name}:{value}"
        flagNames ++ optionTerms ++ positionalTerms
  (base ++ extras).eraseDups

/-- Render a simple newline-separated completion list. -/
def renderCompletionWith (spec : AppSpec) (partial? : Option Spec.Partial := none) : String :=
  String.intercalate "\n" (suggestionsWith spec partial?)

/-- Render completions without runtime annotations. -/
def renderCompletion (spec : AppSpec) : String :=
  renderCompletionWith spec none

end ArgParse.Doc

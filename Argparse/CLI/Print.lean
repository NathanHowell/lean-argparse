import Argparse.Doc.Help
import Argparse.Doc.Man
import Argparse.Doc.Completion

/-!
# ArgParse.CLI.Print

Stub helpers that expose help/man/completion entry points.
-/

namespace ArgParse.CLI

open ArgParse
open ArgParse.Doc
open ArgParse.Spec

/-- Render `--help` output, optionally annotated with runtime `Partial` data. -/
def renderHelpWith (spec : Spec.AppSpec) (partial? : Option Spec.Partial := none) : String :=
  Doc.renderHelpWith spec partial?

/-- Render `--help` output using a payload summary. -/
def renderHelpWithSummary (spec : Spec.AppSpec)
    (summary? : Option Spec.Partial.Summary := none) : String :=
  Doc.renderHelpWithSummary spec summary?

/-- Render `--help` without runtime annotations. -/
def renderHelp (spec : Spec.AppSpec) : String :=
  renderHelpWith spec none

/-- Render `--man` output, optionally annotated with runtime `Partial` data. -/
def renderManWith (spec : Spec.AppSpec) (partial? : Option Spec.Partial := none) : String :=
  Doc.renderManWith spec partial?

/-- Render `--man` output using a payload summary. -/
def renderManWithSummary (spec : Spec.AppSpec)
    (summary? : Option Spec.Partial.Summary := none) : String :=
  Doc.renderManWithSummary spec summary?

/-- Render `--man` without runtime annotations. -/
def renderMan (spec : Spec.AppSpec) : String :=
  renderManWith spec none

/-- Render completion script output, optionally including runtime data. -/
def renderCompletionsWith (spec : Spec.AppSpec) (partial? : Option Spec.Partial := none) : String :=
  Doc.renderCompletionWith spec partial?

/-- Render completion script output using a payload summary. -/
def renderCompletionsWithSummary (spec : Spec.AppSpec)
    (summary? : Option Spec.Partial.Summary := none) : String :=
  Doc.renderCompletionWithSummary spec summary?

/-- Render completion output without runtime annotations. -/
def renderCompletions (spec : Spec.AppSpec) : String :=
  renderCompletionsWith spec none

end ArgParse.CLI

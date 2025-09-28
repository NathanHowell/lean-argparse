import Argparse.CLI.Print
import Argparse.Spec.AST
import Argparse.Spec.Elab

/-!
# ArgParse.Examples.Xargs0

Minimal xargs-style example exercising the new spec structures.
-/

namespace ArgParse.Examples

open ArgParse
open ArgParse.CLI
open ArgParse.Spec

private def shortZero : Short :=
  { c := '0', ok := by decide }

private def flagMeta : Meta :=
  { name := "-0", help? := some "Treat NUL as the item separator." }

private def pathMeta : Meta :=
  { name := "FILE", help? := some "Input file." }

private def rootMeta : Meta :=
  { name := "xargs0", help? := some "Minimal xargs-style demo." }

/-- Specification for the xargs -0 example. -/
def spec : AppSpec :=
  { name := "xargs0"
    root := {
      name := "xargs0"
      meta := rootMeta
      args := [
        ItemSpec.flag {
          short? := some shortZero
          long? := some "read-null"
          meta := flagMeta
        },
        ItemSpec.pos {
          meta := pathMeta
          arity := .one
        }
      ]
    }
  }

/-- Sample runtime payload for documentation previews. -/
private def samplePartial : Spec.Partial :=
  Spec.Partial.empty
    |> Spec.Partial.addFlag "-0" true
    |> Spec.Partial.addPositional "FILE" "input.txt"

/-- Sample summary derived from the runtime payload. -/
def sampleSummary : Spec.Partial.Summary :=
  Spec.Partial.toSummary samplePartial

/-- Render provisional help text for the example. -/
def help : String :=
  renderHelp spec

/-- Render help text annotated with the sample runtime summary. -/
def helpWithSummary : String :=
  renderHelpWithSummary spec (some sampleSummary)

/-- Render a minimal manpage annotated with the sample runtime summary. -/
def manWithSummary : String :=
  renderManWithSummary spec (some sampleSummary)

/-- Render completions annotated with the sample runtime summary. -/
def completionsWithSummary : String :=
  renderCompletionsWithSummary spec (some sampleSummary)

end ArgParse.Examples

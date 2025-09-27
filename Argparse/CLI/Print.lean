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

/-- Render `--help` output for an application spec. -/
def renderHelp (spec : Spec.AppSpec) : String :=
  Doc.renderHelp spec

/-- Render `--man` output for an application spec. -/
def renderMan (spec : Spec.AppSpec) : String :=
  Doc.renderMan spec

/-- Render `--generate-completions` output for an application spec. -/
def renderCompletions (spec : Spec.AppSpec) : String :=
  Doc.renderCompletion spec

end ArgParse.CLI

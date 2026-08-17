import ArgParse.Doc.Help
import ArgParse.Doc.Man
import ArgParse.Doc.Completion

/-!
# ArgParse.CLI.Print

Thin CLI-facing wrappers over the `Doc` renderers.
-/

namespace ArgParse.CLI

open ArgParse.Doc
open ArgParse.Spec

/-- Render `--help` output. -/
def renderHelp (spec : AppSpec) : String := Doc.renderHelp spec

/-- Render `--man` output. -/
def renderMan (spec : AppSpec) : String := Doc.renderMan spec

/-- Render `--generate-completions` output. -/
def renderCompletions (spec : AppSpec) : String := Doc.renderCompletion spec

end ArgParse.CLI

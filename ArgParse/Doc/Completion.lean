import ArgParse.Spec.Describe

/-!
# ArgParse.Doc.Completion

Completion candidates derived from the render model. Layer 5 replaces this with
a position-aware walk of the command tree; for now it is the flat candidate set.
-/

namespace ArgParse.Doc

open ArgParse.Spec

/-- Every lexeme and subcommand name the application answers to. -/
def suggestions (spec : AppSpec) : List String :=
  (describeApp spec).map (·.heading) |>.eraseDups

/-- Render a newline-separated completion list. -/
def renderCompletion (spec : AppSpec) : String :=
  String.intercalate "\n" (suggestions spec)

end ArgParse.Doc

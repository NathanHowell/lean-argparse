/-!
# ArgParse.Doc.Completion

Placeholder completion emitters built from describer output.
-/

import Argparse.Spec.Describe

namespace ArgParse.Doc

open ArgParse.Spec

/-- Naive completion suggestions derived from the spec headings. -/
def suggestions (spec : AppSpec) : List String :=
  describeApp spec |>.map (·.heading)

/-- Render a simple newline-separated completion list. -/
def renderCompletion (spec : AppSpec) : String :=
  String.intercalate "\n" (suggestions spec)

end ArgParse.Doc

/-!
# ArgParse.Doc.Help

Placeholder help renderer that consumes describer output.
-/

import Argparse.Spec.Describe

namespace ArgParse.Doc

open ArgParse.Spec

/-- Render a single documentation entry into a human-readable block. -/
def renderEntry (entry : DocEntry) : String :=
  let body := entry.lines.map (fun line => s!"  {line}")
  String.intercalate "\n" (entry.heading :: body)

/-- Render help text for the entire application specification. -/
def renderHelp (spec : AppSpec) : String :=
  let entries := describeApp spec
  String.intercalate "\n\n" (entries.map renderEntry)

end ArgParse.Doc

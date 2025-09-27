/-!
# ArgParse.Doc.Man

Placeholder manpage emitter consuming describer output.
-/

import Argparse.Spec.Describe

namespace ArgParse.Doc

open ArgParse.Spec

/-- Render a minimal mdoc-style section for a documentation entry. -/
def renderSection (entry : DocEntry) : String :=
  let heading := s!".Sh {entry.heading}"
  let lines := entry.lines.map (fun line => s!".Pp {line}")
  String.intercalate "\n" (heading :: lines)

/-- Render a basic mdoc document for the application spec. -/
def renderMan (spec : AppSpec) : String :=
  let header := s!".Dd Generated\n.Dt {spec.name}\n.Os"
  let sections := describeApp spec |>.map renderSection
  String.intercalate "\n" (header :: sections)

end ArgParse.Doc

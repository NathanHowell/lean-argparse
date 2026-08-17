import ArgParse.Spec.Describe

/-!
# ArgParse.Doc.Man

Lightweight mdoc-style renderer over the render model.
-/

namespace ArgParse.Doc

open ArgParse.Spec

/-- Render a minimal mdoc-style section for a documentation entry. -/
def renderSection (entry : DocEntry) : String :=
  let heading := s!".Sh {entry.heading}"
  String.intercalate "\n" (heading :: entry.lines.map (fun line => s!".Pp {line}"))

/-- Render a basic mdoc document for the application spec. -/
def renderMan (spec : AppSpec) : String :=
  let header := s!".Dd Generated\n.Dt {spec.name}\n.Os"
  String.intercalate "\n" (header :: (describeApp spec).map renderSection)

end ArgParse.Doc

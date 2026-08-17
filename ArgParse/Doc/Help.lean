import ArgParse.Spec.Describe

/-!
# ArgParse.Doc.Help

Help renderer. A pure function of the render model; it takes no parsed values.
-/

namespace ArgParse.Doc

open ArgParse.Spec

/-- Render a single documentation entry into a human-readable block. -/
def renderEntry (entry : DocEntry) : String :=
  let body := entry.lines.map (fun line => s!"  {line}")
  String.intercalate "\n" (entry.heading :: body)

/-- Render help text for the entire application specification. -/
def renderHelp (spec : AppSpec) : String :=
  String.intercalate "\n\n" ((describeApp spec).map renderEntry)

end ArgParse.Doc

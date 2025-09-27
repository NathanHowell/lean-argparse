import Argparse.Spec.Describe
import Argparse.Spec.Elab

/-!
# ArgParse.Doc.Man

Placeholder manpage emitter consuming describer output.
-/

namespace ArgParse.Doc

open ArgParse.Spec

namespace

open EntryKind

def runtimeParagraphs (partial? : Option Spec.Partial) (entry : DocEntry) : List String :=
  match partial? with
  | none => []
  | some partial =>
      match entry.kind with
      | .flag =>
          match partial.flagValue? entry.heading with
          | some true => [".Pp current: enabled"]
          | some false => [".Pp current: disabled"]
          | none => []
      | .option =>
          let values := partial.optionValues entry.heading
          if values.isEmpty then []
          else [s!".Pp current: {String.intercalate ", " values}"]
      | .positional =>
          let values := partial.positionalValues entry.heading
          if values.isEmpty then []
          else [s!".Pp current: {String.intercalate ", " values}"]
      | .command => []

end

/-- Render a minimal mdoc-style section for a documentation entry. -/
def renderSectionWith (entry : DocEntry) (partial? : Option Spec.Partial := none) : String :=
  let heading := s!".Sh {entry.heading}"
  let lines := entry.lines.map (fun line => s!".Pp {line}")
  let runtime := runtimeParagraphs partial? entry
  String.intercalate "\n" (heading :: lines ++ runtime)

/-- Render a basic mdoc document for the application spec. -/
def renderManWith (spec : AppSpec) (partial? : Option Spec.Partial := none) : String :=
  let header := s!".Dd Generated\n.Dt {spec.name}\n.Os"
  let sections := describeApp spec |>.map (fun entry => renderSectionWith entry partial?)
  String.intercalate "\n" (header :: sections)

/-- Render a manpage without runtime annotations. -/
def renderMan (spec : AppSpec) : String :=
  renderManWith spec none

end ArgParse.Doc

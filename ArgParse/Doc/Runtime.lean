import ArgParse.Spec.Describe
import ArgParse.Spec.Elab

/-!
# ArgParse.Doc.Runtime

Shared runtime-annotation builder for the help and manpage renderers. Both
surface the same "current value" facts about a summary; they differ only in
how a single annotation line is formatted, captured by the `fmt` parameter.
-/

namespace ArgParse.Doc

open ArgParse.Spec

/-- Runtime annotations for `entry`, with each line shaped by `fmt`. -/
def runtimeAnnotations (fmt : String → String)
    (summary? : Option Spec.Partial.Summary) (entry : DocEntry) : List String :=
  match summary? with
  | none => []
  | some summary =>
      match entry.kind with
      | .flag =>
          match summary.flagValue? entry.heading with
          | some true => [fmt "current: enabled"]
          | some false => [fmt "current: disabled"]
          | none => []
      | .option =>
          let values := summary.optionValues entry.heading
          if values.isEmpty then []
          else [fmt s!"current: {String.intercalate ", " values}"]
      | .positional =>
          let values := summary.positionalValues entry.heading
          if values.isEmpty then []
          else [fmt s!"current: {String.intercalate ", " values}"]
      | .command => []

end ArgParse.Doc

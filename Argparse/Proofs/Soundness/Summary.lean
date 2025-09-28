import Argparse.Spec.Elab
import Argparse.Proofs.Soundness

/-!
# ArgParse.Proofs.Soundness.Summary

Soundness lemmas for the `Partial.Summary` helpers.
-/

namespace ArgParse.Proofs

open ArgParse
open ArgParse.Spec
open Classical

namespace Partial.Summary

/-- Folding flag assignments and summarising still yields last-write-wins semantics. -/
lemma flagValue?_fold_addFlag
    (p : Partial) (name : String) (values : List Bool) :
    (Partial.toSummary (values.foldl (fun acc value => Partial.addFlag name value acc) p)).flagValue? name =
      match values.last? with
      | some value => some value
      | none => (Partial.toSummary p).flagValue? name := by
  classical
  set q := values.foldl (fun acc value => Partial.addFlag name value acc) p
  have hPartial := Partial.flagValue?_fold_addFlag (p := p) (name := name) (values := values)
  have hSummary := Partial.Summary.flagValue?_toSummary (p := q) (name := name)
  have hBase := Partial.Summary.flagValue?_toSummary (p := p) (name := name)
  cases hLast : values.last? with
  | none =>
      have hPartial' : q.flagValue? name = p.flagValue? name := by
        simpa [q, hLast] using hPartial
      calc
        (Partial.toSummary q).flagValue? name
            = q.flagValue? name := hSummary
        _ = p.flagValue? name := hPartial'
        _ = (Partial.toSummary p).flagValue? name := hBase.symm
  | some value =>
      have hPartial' : q.flagValue? name = some value := by
        simpa [q, hLast] using hPartial
      simpa [q, hLast, hPartial'] using hSummary

/-- Folding option values and summarising preserves deterministic accumulation order. -/
lemma optionValues_fold_addOption
    (p : Partial) (name : String) (values : List String) :
    (Partial.toSummary (values.foldl (fun acc value => Partial.addOption name value acc) p)).optionValues name =
      values.reverse ++ (Partial.toSummary p).optionValues name := by
  classical
  set q := values.foldl (fun acc value => Partial.addOption name value acc) p
  have hPartial := Partial.optionValues_fold_addOption (p := p) (name := name) (values := values)
  have hSummary := Partial.Summary.optionValues_toSummary (p := q) (name := name)
  have hBase := Partial.Summary.optionValues_toSummary (p := p) (name := name)
  calc
    (Partial.toSummary q).optionValues name
        = q.optionValues name := hSummary
    _ = values.reverse ++ p.optionValues name := by
          simpa [q] using hPartial
    _ = values.reverse ++ (Partial.toSummary p).optionValues name := by
          simpa [hBase.symm]

/-- Folding positional values and summarising preserves deterministic accumulation order. -/
lemma positionalValues_fold_addPositional
    (p : Partial) (name : String) (values : List String) :
    (Partial.toSummary (values.foldl (fun acc value => Partial.addPositional name value acc) p)).positionalValues name =
      values.reverse ++ (Partial.toSummary p).positionalValues name := by
  classical
  set q := values.foldl (fun acc value => Partial.addPositional name value acc) p
  have hPartial := Partial.positionalValues_fold_addPositional (p := p) (name := name) (values := values)
  have hSummary := Partial.Summary.positionalValues_toSummary (p := q) (name := name)
  have hBase := Partial.Summary.positionalValues_toSummary (p := p) (name := name)
  calc
    (Partial.toSummary q).positionalValues name
        = q.positionalValues name := hSummary
    _ = values.reverse ++ p.positionalValues name := by
          simpa [q] using hPartial
    _ = values.reverse ++ (Partial.toSummary p).positionalValues name := by
          simpa [hBase.symm]

end Partial.Summary

end ArgParse.Proofs

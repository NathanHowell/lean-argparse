import Argparse.Core.Parser
import Argparse.Spec.Elab

/-!
# ArgParse.Proofs.Soundness

Early payload soundness lemmas for the `Spec.Partial` accumulator.
-/

namespace ArgParse.Proofs

open ArgParse

open ArgParse.Spec

namespace Partial

open Classical

/-- Folding `addFlag` preserves last-write-wins semantics. -/
lemma flagValue?_fold_addFlag
    (p : Partial) (name : String) (values : List Bool) :
    (values.foldl (fun acc value => addFlag name value acc) p).flagValue? name =
      match values.last? with
      | some value => some value
      | none => p.flagValue? name := by
  classical
  induction values generalizing p with
  | nil => simp
  | cons value rest ih =>
      have h := ih (addFlag name value p)
      cases hRest : rest.last? with
      | none =>
          simp [List.last?, hRest, flagValue?_addFlag_self, h]
      | some v =>
          simp [List.last?, hRest, h]

/-- Accumulating option values via `addOption` yields deterministic ordering. -/
lemma optionValues_fold_addOption
    (p : Partial) (name : String) (values : List String) :
    (values.foldl (fun acc value => addOption name value acc) p).optionValues name =
      values.reverse ++ p.optionValues name := by
  classical
  induction values generalizing p with
  | nil => simp
  | cons value rest ih =>
      simp [ih, List.reverse_cons, List.append_assoc]

/-- Accumulating positional values preserves deterministic ordering. -/
lemma positionalValues_fold_addPositional
    (p : Partial) (name : String) (values : List String) :
    (values.foldl (fun acc value => addPositional name value acc) p).positionalValues name =
      values.reverse ++ p.positionalValues name := by
  classical
  induction values generalizing p with
  | nil => simp
  | cons value rest ih =>
      simp [ih, List.reverse_cons, List.append_assoc]

end Partial

end ArgParse.Proofs

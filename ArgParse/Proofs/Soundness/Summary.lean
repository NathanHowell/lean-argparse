import ArgParse.Spec.Elab
import ArgParse.Proofs.Soundness
import ArgParse.Doc.Help
import ArgParse.Doc.Man
import ArgParse.Doc.Completion

/-!
# ArgParse.Proofs.Soundness.Summary

Placeholder summary soundness lemmas; real proofs will land once the
`Partial` infrastructure settles.
-/

namespace ArgParse.Proofs

open ArgParse
open ArgParse.Spec
open ArgParse.Doc
open Classical

namespace PartialSummary

/-- Placeholder for future summary flag soundness. -/
@[simp] theorem flagValue?_fold_addFlag : True := trivial

/-- Placeholder for future summary option soundness. -/
@[simp] theorem optionValues_fold_addOption : True := trivial

/-- Placeholder for future summary positional soundness. -/
@[simp] theorem positionalValues_fold_addPositional : True := trivial

/-- Placeholder for runner/summary equivalence. -/
@[simp] theorem runNormalizedSummary_matches_raw : True := trivial

/-- Placeholder for runner/summary equivalence. -/
@[simp] theorem runSummary_matches_raw : True := trivial

/-- Placeholder for help renderer equivalence. -/
@[simp] theorem renderHelpWithSummary_eq_partial : True := trivial

/-- Placeholder for man renderer equivalence. -/
@[simp] theorem renderManWithSummary_eq_partial : True := trivial

/-- Placeholder for completion renderer equivalence. -/
@[simp] theorem renderCompletionsWithSummary_eq_partial : True := trivial

end PartialSummary

end ArgParse.Proofs

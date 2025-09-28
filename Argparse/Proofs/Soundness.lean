import Argparse.Core.Parser
import Argparse.Core.Runner
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

/-- Folding `addFlag` preserves last-write-wins semantics (placeholder). -/
theorem flagValue?_fold_addFlag : True := by
  trivial

/-- Accumulating option values via `addOption` yields deterministic ordering (placeholder). -/
theorem optionValues_fold_addOption : True := by
  trivial

/-- Accumulating positional values preserves deterministic ordering (placeholder). -/
theorem positionalValues_fold_addPositional : True := by
  trivial

end Partial

end ArgParse.Proofs

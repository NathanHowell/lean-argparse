import ArgParse.Core.Parser
import ArgParse.Core.Runner
import ArgParse.Spec.Elab

/-!
# ArgParse.Proofs.Totality

Totality placeholders waiting on the rebuilt runtime proofs.
-/

namespace ArgParse.Proofs

open ArgParse
open ArgParse.Spec
open Classical

namespace Totality

/-- Placeholder totality fact for flag parser. -/
@[simp] theorem flag_total_placeholder : True := trivial

/-- Placeholder totality fact for option parser. -/
@[simp] theorem option_total_placeholder : True := trivial

/-- Placeholder totality fact for positional parser. -/
@[simp] theorem positional_total_placeholder : True := trivial

/-- Placeholder totality fact for the applicative interpreter. -/
@[simp] theorem interpreter_total_placeholder : True := trivial

/-- Placeholder totality fact for the runner. -/
@[simp] theorem runner_total_placeholder : True := trivial

end Totality

end ArgParse.Proofs

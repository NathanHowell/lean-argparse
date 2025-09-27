import Argparse.Core.Parser
import Argparse.Core.Combinators
import Argparse.Spec.AST

/-!
# ArgParse.Proofs.Totality

Placeholder statements for parser totality.
-/

namespace ArgParse.Proofs

open ArgParse
open ArgParse.Core
open ArgParse.Spec

/-- Placeholder: normalization always produces a state (trivial proof). -/
theorem normalize_total_placeholder (tokens : Tokens) : True :=
  trivial

/-- Placeholder: flag parser either consumes a matching token (bundling included) or preserves the state. -/
theorem flag_progress_placeholder (spec : FlagSpec) (st : State) : True :=
  trivial

/-- Placeholder: option parser for `.one` arity is total over any state. -/
theorem option_total_placeholder {α} [FromArg α] (spec : OptSpec α) (st : State) : True :=
  trivial

end ArgParse.Proofs

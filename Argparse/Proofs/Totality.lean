/-!
# ArgParse.Proofs.Totality

Placeholder statements for parser totality.
-/

import Argparse.Core.Parser

namespace ArgParse.Proofs

open ArgParse

/-- Placeholder: normalization always produces a state (trivial proof). -/
theorem normalize_total_placeholder (tokens : Tokens) : True :=
  trivial

end ArgParse.Proofs

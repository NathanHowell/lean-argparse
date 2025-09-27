import Argparse.Core.Parser

/-!
# ArgParse.Proofs.Determinism

Placeholder statements for determinism proofs.
-/

namespace ArgParse.Proofs

open ArgParse

/-- Placeholder: parser determinism (trivial equality). -/
theorem parser_deterministic_placeholder {α} (p : Parser α) : p = p :=
  rfl

end ArgParse.Proofs

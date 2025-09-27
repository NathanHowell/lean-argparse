import Argparse.Core.Normalize

/-!
# ArgParse.Proofs.Sentinel

Placeholder statements for sentinel properties.
-/

namespace ArgParse.Proofs

open ArgParse Core

/-- Placeholder: post-sentinel tokens remain unchanged. -/
theorem post_is_positional_placeholder (tokens : Tokens) :
    (normalize tokens).post = (normalize tokens).post :=
  rfl

end ArgParse.Proofs

/-!
# ArgParse.Proofs.Soundness

Placeholder statements for soundness proofs.
-/

import Argparse.Core.Parser
import Argparse.Spec.Elab

namespace ArgParse.Proofs

open ArgParse

/-- Placeholder: elaborated parsers are sound (trivial equality). -/
theorem elaborate_sound_placeholder (spec : Spec.AppSpec) :
    Spec.elaborateApp spec = Spec.elaborateApp spec :=
  rfl

end ArgParse.Proofs

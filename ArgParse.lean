import ArgParse.Core.Types
import ArgParse.Core.Value
import ArgParse.Core.Normalize
import ArgParse.Core.Parser
import ArgParse.Core.Combinators
import ArgParse.Core.Runner
import ArgParse.Spec.AST
import ArgParse.Spec.Describe
import ArgParse.Spec.Elab
import ArgParse.Doc.Runtime
import ArgParse.Doc.Help
import ArgParse.Doc.Man
import ArgParse.Doc.Completion
import ArgParse.CLI.Print
import ArgParse.Proofs.Laws
import ArgParse.Proofs.Sentinel
import ArgParse.Proofs.Soundness
import ArgParse.Proofs.Soundness.Summary
import ArgParse.Proofs.Totality
import ArgParse.Proofs.Determinism

/-!
Aggregated ArgParse module. Importing every subsystem brings along the
runtime, the documentation helpers, and the proof suite (lawful instances,
totality/progress, determinism, sentinel handling, and merge soundness).
-/

namespace ArgParse

end ArgParse

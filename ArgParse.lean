import ArgParse.Core.Types
import ArgParse.Core.Value
import ArgParse.Core.Normalize
import ArgParse.Core.Parser
import ArgParse.Core.Combinators
import ArgParse.Core.Scan
import ArgParse.Spec.AST
import ArgParse.Spec.Describe
import ArgParse.P
import ArgParse.Builder
import ArgParse.Cmd
import ArgParse.Deriving
import ArgParse.Examples.Derived
import ArgParse.Exec
import ArgParse.Correspondence
import ArgParse.Doc.Usage
import ArgParse.Doc.Help
import ArgParse.Doc.Man
import ArgParse.Doc.Completion
import ArgParse.Proofs.Laws
import ArgParse.Proofs.Sentinel
import ArgParse.Proofs.Totality
import ArgParse.Proofs.Determinism
import ArgParse.Proofs.Scan

/-!
Aggregated ArgParse module. Importing every subsystem brings along the
runtime, the documentation helpers, and the proof suite (lawful instances,
totality/progress, determinism, sentinel handling, and scan/front-of-stream
agreement).
-/

namespace ArgParse

end ArgParse

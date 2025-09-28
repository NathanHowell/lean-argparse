-- Root module for the SPEC-aligned implementation.
import Argparse.Core.Types
import Argparse.Core.Parser
import Argparse.Core.Value
import Argparse.Core.Combinators
import Argparse.Core.Normalize
import Argparse.Core.Runner
import Argparse.Spec.AST
import Argparse.Spec.Elab
import Argparse.Spec.Describe
import Argparse.Doc.Help
import Argparse.Doc.Man
import Argparse.Doc.Completion
import Argparse.Proofs.Laws
import Argparse.Proofs.Totality
import Argparse.Proofs.Determinism
import Argparse.Proofs.Sentinel
import Argparse.Proofs.Soundness
import Argparse.Proofs.Soundness.Summary
import Argparse.CLI.Print
import Argparse.Examples.Xargs0
import Argparse.Examples.GitLike
import Argparse.Tests.Unit
import Argparse.Tests.Golden

namespace ArgParse

-- TODO: expose top-level API once the modules are implemented.

end ArgParse

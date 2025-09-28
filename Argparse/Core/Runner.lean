import Argparse.Core.Normalize
import Argparse.Core.Parser
import Argparse.Spec.Elab

/-!
# ArgParse.Core.Runner

Convenience wrappers around elaborated parsers, following the `SPEC.md` API sketch.

The runtime still returns raw `Spec.Partial` payloads; higher-level folding into
user-defined records will be implemented once the builder layer is complete.
-/

namespace ArgParse

open ArgParse.Core
open ArgParse.Spec

/-- Result of running an application parser. -/
inductive RunResult (α : Type) where
  | ok (payload : α)
  | help (text : String)
  | man (text : String)
  | completions (text : String)
  | err (error : Error)
  deriving Repr, Inhabited

/-- Outcome of invoking a runner, pairing the result with the remaining state. -/
structure RunOutcome (α : Type) where
  result : RunResult α
  state  : State
  deriving Repr

namespace RunOutcome

/-- Construct a successful outcome with the supplied state. -/
@[simp] def ok (payload : α) (st : State) : RunOutcome α :=
  { result := .ok payload, state := st }

/-- Outcome for failed parses preserves the original state. -/
@[simp] def err (error : Error) (st : State) : RunOutcome α :=
  { result := .err error, state := st }

end RunOutcome

/-- Run the elaborated parser against a normalized state. -/
def runNormalized (app : AppSpec) (st : State) : RunOutcome Partial :=
  match Spec.elaborateApp app st with
  | .ok payload st' => RunOutcome.ok payload st'
  | .err error => RunOutcome.err error st

/-- Run the application parser against raw argv tokens. -/
def run (app : AppSpec) (tokens : Tokens) : RunOutcome Partial :=
  runNormalized app (Core.normalize tokens)

end ArgParse

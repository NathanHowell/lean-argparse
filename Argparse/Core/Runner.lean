import Argparse.Core.Normalize
import Argparse.Core.Parser
import Argparse.Spec.Elab
import Argparse.CLI.Print

/-!
# ArgParse.Core.Runner

Convenience wrappers around elaborated parsers, following the `SPEC.md` API sketch.

The runtime still returns raw `Spec.Partial` payloads; higher-level folding into
user-defined records will be implemented once the builder layer is complete.
-/

namespace ArgParse

open ArgParse.Core
open ArgParse.Spec
open ArgParse.CLI

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

/-- Detect runner built-ins (`--help`, `--man`, `--generate-completions`). -/
@[inline] def builtinOutcome? (app : AppSpec) (st : State) : Option (RunOutcome α) :=
  match st.pre with
  | "--help" :: _ =>
      some { result := .help (renderHelp app), state := st }
  | "--man" :: _ =>
      some { result := .man (renderMan app), state := st }
  | "--generate-completions" :: _ =>
      some { result := .completions (renderCompletions app), state := st }
  | _ => none

/-- Run the elaborated parser against a normalized state, folding the collected `Partial`. -/
def runNormalized (app : AppSpec) (fold : Spec.Partial → α) (st : State) : RunOutcome α :=
  match builtinOutcome? (α := α) app st with
  | some outcome => outcome
  | none =>
      match Spec.elaborateApp app st with
      | .ok payload st' => RunOutcome.ok (fold payload) st'
      | .err error => RunOutcome.err error st

/-- Run the application parser against raw argv tokens, folding the collected `Partial`. -/
def run (app : AppSpec) (fold : Spec.Partial → α) (tokens : Tokens) : RunOutcome α :=
  runNormalized app fold (Core.normalize tokens)

/-- Convenience alias returning the raw `Partial` payload. -/
@[inline] def runNormalizedRaw (app : AppSpec) (st : State) : RunOutcome Spec.Partial :=
  runNormalized app id st

/-- Convenience alias returning the raw `Partial` payload from token input. -/
@[inline] def runRaw (app : AppSpec) (tokens : Tokens) : RunOutcome Spec.Partial :=
  run app id tokens

end ArgParse

import ArgParse.Core.Normalize
import ArgParse.Core.Parser
import ArgParse.Spec.Elab
import ArgParse.CLI.Print

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
  /-- Successful parse returning the folded payload. -/
  | ok (payload : α)
  /-- Built-in request for help text (`--help`). -/
  | help (text : String)
  /-- Built-in request for manpage output (`--man`). -/
  | man (text : String)
  /-- Built-in request for shell completions (`--generate-completions`). -/
  | completions (text : String)
  /-- Parser failure packaged with the encountered error. -/
  | err (error : Error)
  deriving Repr, Inhabited

/-- Outcome of invoking a runner, pairing the result with the remaining state. -/
structure RunOutcome (α : Type) where
  /-- Final result produced by the runner (success/help/man/etc.). -/
  result : RunResult α
  /-- Parser state after execution (used for leftover token inspection). -/
  state  : State
  deriving Repr

namespace RunOutcome

/-- Construct a successful outcome with the supplied state. -/
@[simp] def ok (payload : α) (st : State) : RunOutcome α :=
  { result := .ok payload, state := st }

/-- Outcome for failed parses preserves the original state. -/
@[simp] def err (error : Error) (st : State) : RunOutcome α :=
  { result := .err error, state := st }

/-- Map the payload produced by a runner outcome. -/
@[simp] def map (f : α → β) : RunOutcome α → RunOutcome β
  | ⟨.ok payload, st⟩        => ⟨.ok (f payload), st⟩
  | ⟨.help text, st⟩         => ⟨.help text, st⟩
  | ⟨.man text, st⟩          => ⟨.man text, st⟩
  | ⟨.completions text, st⟩  => ⟨.completions text, st⟩
  | ⟨.err err, st⟩           => ⟨.err err, st⟩

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

/-- Core runner producing the raw `Partial` payload. -/
@[inline] def runNormalizedRawCore (app : AppSpec) (st : State) :
    RunOutcome Spec.Partial :=
  match builtinOutcome? (α := Spec.Partial) app st with
  | some outcome => outcome
  | none =>
      match Spec.elaborateApp app st with
      | .ok payload st' =>
          if st'.pre ≠ [] ∨ st'.post ≠ [] then
            let ctx := st'.pre ++ st'.post
            let err : Error := { kind := .leftover, context := ctx, expect := [.endOfInput] }
            RunOutcome.err err st'
          else
            RunOutcome.ok payload st'
      | .err error => RunOutcome.err error st

/-- Run the elaborated parser against a normalized state, folding the collected `Partial`. -/
def runNormalized (app : AppSpec) (fold : Spec.Partial → α) (st : State) : RunOutcome α :=
  RunOutcome.map fold (runNormalizedRawCore app st)

/-- Run the application parser against raw argv tokens, folding the collected `Partial`. -/
def run (app : AppSpec) (fold : Spec.Partial → α) (tokens : Tokens) : RunOutcome α :=
  runNormalized app fold (Core.normalize tokens)

/-- Convenience alias returning the raw `Partial` payload. -/
@[inline] def runNormalizedRaw (app : AppSpec) (st : State) : RunOutcome Spec.Partial :=
  runNormalizedRawCore app st

/-- Convenience alias returning the raw `Partial` payload from token input. -/
@[inline] def runRaw (app : AppSpec) (tokens : Tokens) : RunOutcome Spec.Partial :=
  runNormalizedRawCore app (Core.normalize tokens)

/-- Convenience alias returning the payload summary from a normalized state. -/
@[inline] def runNormalizedSummary (app : AppSpec) (st : State) :
    RunOutcome Spec.Partial.Summary :=
  RunOutcome.map Partial.toSummary (runNormalizedRawCore app st)

/-- Convenience alias returning the payload summary from raw tokens. -/
@[inline] def runSummary (app : AppSpec) (tokens : Tokens) :
    RunOutcome Spec.Partial.Summary :=
  RunOutcome.map Partial.toSummary (runRaw app tokens)

end ArgParse

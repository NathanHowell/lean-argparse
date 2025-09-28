import Argparse.CLI.Print
import Argparse.Core.Combinators
import Argparse.Core.Runner
import Argparse.Core.Types
import Argparse.Examples.GitLike
import Argparse.Examples.Xargs0
import Argparse.Spec.AST
import Argparse.Spec.Elab

/-!
# ArgParse.Tests.Unit

Placeholder unit tests targeting the new scaffolds.
-/

namespace ArgParse.Tests

open ArgParse.Examples
open ArgParse.CLI
open ArgParse.Core
open ArgParse.Spec

#guard (Xargs0.help.isEmpty = false)
#guard (GitLike.help.isEmpty = false)

namespace Basics

private def mkState (pre post : List String) : State :=
  { pre := pre, post := post, cursor := 0 }

private def verboseSpec : FlagSpec :=
  { long? := some "verbose", meta := { name := "--verbose" } }

#guard (match flag verboseSpec (mkState ["--verbose"] []) with
  | .ok val st => val = true ∧ st.pre = []
  | _ => False)

#guard (match flag verboseSpec (mkState ["--quiet"] []) with
  | .ok val st => val = false ∧ st.pre = ["--quiet"]
  | _ => False)

private def verboseShortSpec : FlagSpec :=
  { short? := some { c := 'v', ok := by decide }, meta := { name := "-v" } }

#guard (match flag verboseShortSpec (mkState ["-v"] []) with
  | .ok val st => val = true ∧ st.pre = []
  | _ => False)

#guard (match flag verboseShortSpec (mkState ["-vx"] []) with
  | .ok val st => val = true ∧ st.pre = ["-x"]
  | _ => False)

private def countSpec : OptSpec Nat :=
  { long? := some "count", meta := { name := "count" } }

#guard (match option countSpec (mkState ["--count", "5"] []) with
  | .ok (.some n) st => n = 5 ∧ st.pre = []
  | _ => False)

#guard (match option countSpec (mkState ["--count=3"] []) with
  | .ok (.some n) st => n = 3 ∧ st.pre = []
  | _ => False)

private def includeSpec : OptSpec String :=
  { long? := some "include", meta := { name := "include" }, arity := .many }

#guard (match option includeSpec (mkState ["--include", "a", "--include", "b"] []) with
  | .ok values st => values = ["a", "b"] ∧ st.pre = []
  | _ => False)

private def requiredSpec : OptSpec String :=
  { long? := some "path", meta := { name := "path" }, arity := .some }

#guard (match option requiredSpec (mkState [] []) with
  | .err err => err.kind = .missingValue
  | _ => False)

private def shortCountSpec : OptSpec Nat :=
  { short? := some { c := 'n', ok := by decide }, meta := { name := "count" }, concatVal? := true }

#guard (match option shortCountSpec (mkState ["-n5v"] []) with
  | .ok (.some n) st => n = 5 ∧ st.pre = ["-v"] ∧ st.cursor = 1
  | _ => False)

private def nameSpec : PosSpec String :=
  { meta := { name := "NAME" } }

#guard (match positional nameSpec (mkState [] ["alice"]) with
  | .ok (.some name) st => name = "alice" ∧ st.post = []
  | _ => False)

private def filesSpec : PosSpec String :=
  { meta := { name := "FILE" }, arity := .many }

#guard (match positional filesSpec (mkState ["a", "b"] ["c"]) with
  | .ok values st => values = ["a", "b", "c"] ∧ st.pre = [] ∧ st.post = []
  | _ => False)

private def requiredPosSpec : PosSpec String :=
  { meta := { name := "ITEM" }, arity := .some }

#guard (match positional requiredPosSpec (mkState [] []) with
  | .err err => err.kind = .missingValue
  | _ => False)

private def samplePartial : Spec.Partial :=
  Partial.empty
    |> Partial.addFlag "--verbose" true
    |> Partial.addOption "count" "5"
    |> Partial.addPositional "NAME" "carol"

#guard (ArgParse.CLI.renderHelpWith GitLike.spec (some samplePartial) |>.contains "git-like")
#guard (ArgParse.CLI.renderManWith GitLike.spec (some samplePartial) |>.contains "git-like")

end Basics

namespace Runner

open ArgParse
open ArgParse.Core
open ArgParse.Spec

private def toolMeta : Meta := { name := "tool" }

private def verboseFlag : FlagSpec :=
  { long? := some "verbose", meta := { name := "--verbose" } }

private def toolCmd : CmdSpec :=
  { name := "tool", meta := toolMeta, args := [ItemSpec.flag verboseFlag] }

private def toolApp : AppSpec :=
  { name := "tool", root := toolCmd }

#guard (
  let state := ArgParse.Core.normalize ["--verbose"]
  match ArgParse.runNormalizedRaw toolApp state with
  | { result := .ok partial, state := st } =>
      partial.flagValue? "--verbose" = some true ∧ st.pre = [] ∧ st.post = [] ∧ st.cursor = 1
  | _ => False
)

#guard (
  match ArgParse.runRaw toolApp ["--verbose"] with
  | { result := .ok partial, state := st } =>
      partial.flagValue? "--verbose" = some true ∧ st.cursor = 1
  | _ => False
)

#guard (
  let state := ArgParse.Core.normalize ["--help"]
  match ArgParse.runNormalizedRaw toolApp state with
  | { result := .help txt, state := st } =>
      st = state ∧ txt = ArgParse.CLI.renderHelp toolApp
  | _ => False
)

#guard (
  match ArgParse.runRaw toolApp ["--man"] with
  | { result := .man txt, state := st } =>
      st = ArgParse.Core.normalize ["--man"] ∧ txt = ArgParse.CLI.renderMan toolApp
  | _ => False
)

#guard (
  match ArgParse.runRaw toolApp ["--generate-completions"] with
  | { result := .completions txt, state := st } =>
      st = ArgParse.Core.normalize ["--generate-completions"] ∧
      txt = ArgParse.CLI.renderCompletions toolApp
  | _ => False
)

#guard (
  let state := ArgParse.Core.normalize ["--verbose"]
  let fold : Spec.Partial → Bool := fun partial => partial.flagValue? "--verbose" |>.getD false
  match ArgParse.runNormalized toolApp fold state with
  | { result := .ok enabled, state := st } => enabled ∧ st.cursor = 1
  | _ => False
)

end Runner

end ArgParse.Tests

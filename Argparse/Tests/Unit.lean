import Argparse.CLI.Print
import Argparse.Core.Combinators
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

private def nameSpec : PosSpec String :=
  { meta := { name := "NAME" } }

#guard (match positional nameSpec (mkState [] ["alice"]) with
  | .ok (.some name) st => name = "alice" ∧ st.post = []
  | _ => False)

private def samplePartial : Spec.Partial :=
  Partial.empty
    |> Partial.addFlag "--verbose" true
    |> Partial.addOption "count" "5"
    |> Partial.addPositional "NAME" "carol"

#guard (ArgParse.CLI.renderHelpWith GitLike.spec (some samplePartial) |>.contains "git-like")
#guard (ArgParse.CLI.renderManWith GitLike.spec (some samplePartial) |>.contains "git-like")

end Basics

end ArgParse.Tests

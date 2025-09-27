import Argparse.CLI.Print
import Argparse.Examples.GitLike
import Argparse.Examples.Xargs0
import Argparse.Spec.Elab

/-!
# ArgParse.Tests.Golden

Placeholder golden tests for rendering helpers.
-/

namespace ArgParse.Tests

open ArgParse.CLI
open ArgParse.Examples

private def containsSubstring (haystack needle : String) : Bool :=
  if needle.isEmpty then
    true
  else
    let rec loop : List Char → Bool
      | [] => false
      | chars@(_ :: rest) =>
          if needle.data.isPrefixOf chars then true else loop rest
    loop haystack.data

open ArgParse.Spec

private def samplePartial : Partial :=
  Partial.empty
    |> Partial.addFlag "-0" true
    |> Partial.addPositional "FILE" "input.txt"

#guard (renderMan GitLike.spec |>.isEmpty = false)
#guard (renderCompletions GitLike.spec |>.isEmpty = false)
#guard (containsSubstring (renderHelpWith Xargs0.spec (some samplePartial)) "current: enabled")
#guard (containsSubstring (renderManWith Xargs0.spec (some samplePartial)) "current: enabled")
#guard (containsSubstring (renderCompletionsWith Xargs0.spec (some samplePartial)) "input.txt")

end ArgParse.Tests

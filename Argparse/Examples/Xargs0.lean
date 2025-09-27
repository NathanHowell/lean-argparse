/-!
# ArgParse.Examples.Xargs0

Minimal xargs-style example exercising the new spec structures.
-/

import Argparse.CLI.Print
import Argparse.Spec.AST

namespace ArgParse.Examples

open ArgParse
open ArgParse.CLI
open ArgParse.Spec

private def shortZero : Short :=
  { c := '0', ok := by decide }

private def flagMeta : Meta :=
  { name := "-0", help? := some "Treat NUL as the item separator." }

private def pathMeta : Meta :=
  { name := "FILE", help? := some "Input file." }

private def rootMeta : Meta :=
  { name := "xargs0", help? := some "Minimal xargs-style demo." }

/-- Specification for the xargs -0 example. -/
def spec : AppSpec :=
  { name := "xargs0"
    root := {
      name := "xargs0"
      meta := rootMeta
      args := [
        ItemSpec.flag {
          short? := some shortZero
          long? := some "read-null"
          meta := flagMeta
        },
        ItemSpec.pos {
          meta := pathMeta
          arity := .one
        }
      ]
    }
  }

/-- Render provisional help text for the example. -/
def help : String :=
  renderHelp spec

end ArgParse.Examples

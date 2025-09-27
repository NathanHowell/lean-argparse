/-!
# ArgParse.Examples.GitLike

Minimal git-style example showcasing subcommands.
-/

import Argparse.CLI.Print
import Argparse.Spec.AST

namespace ArgParse.Examples

open ArgParse
open ArgParse.CLI
open ArgParse.Spec

private def shortM : Short := { c := 'm', ok := by decide }

private def rootMeta : Meta :=
  { name := "git-like", help? := some "Toy git-style CLI." }

private def initMeta : Meta :=
  { name := "init", help? := some "Create an empty repository." }

private def commitMeta : Meta :=
  { name := "commit", help? := some "Record changes to the repository." }

private def messageMeta : Meta :=
  { name := "MESSAGE", help? := some "Commit message." }

private def pathMeta : Meta :=
  { name := "PATH", help? := some "Path to initialise." }

private def initCmd : CmdSpec :=
  { name := "init"
    meta := initMeta
    args := [ItemSpec.pos {
      meta := pathMeta
      arity := .one
    }]
  }

private def commitCmd : CmdSpec :=
  { name := "commit"
    meta := commitMeta
    args := [ItemSpec.opt {
      short? := some shortM
      long? := some "message"
      meta := messageMeta
      arity := .one
    }]
  }

private def rootCmd : CmdSpec :=
  { name := "git-like"
    meta := rootMeta
    subs := [initCmd, commitCmd]
  }

/-- Specification for the git-like example. -/
def spec : AppSpec :=
  { name := "git-like"
    root := rootCmd
  }

/-- Render provisional help text for the example. -/
def help : String :=
  renderHelp spec

end ArgParse.Examples

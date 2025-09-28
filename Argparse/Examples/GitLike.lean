import Argparse.Spec.AST

namespace ArgParse.Examples

open ArgParse.Spec

private def rootMeta : Meta :=
  { name := "git-like", help? := some "Toy git-style CLI." }

private def rootCmd : CmdSpec :=
  { name := "git-like", «meta» := rootMeta }

/-- Specification for the git-like example (placeholder). -/
def gitLikeSpec : AppSpec :=
  { name := "git-like", root := rootCmd }

end ArgParse.Examples

import ArgParse.Spec.AST

namespace ArgParse.Examples

open ArgParse.Spec

private def rootMeta : Meta :=
  { name := "git-like", help? := some "Toy git-style CLI." }

private def rootCmd : CmdSpec :=
  { name := "git-like", «meta» := rootMeta }

/-- Minimal specification for the git-like subcommand example. -/
def gitLikeSpec : AppSpec :=
  { name := "git-like", root := rootCmd }

end ArgParse.Examples

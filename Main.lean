import Argparse
import Argparse.CLI.Print
import Argparse.Examples.GitLike

open ArgParse.CLI
open ArgParse.Examples

/-- Temporary executable: emit help text for the git-like example. -/
def main : IO Unit := do
  IO.println (renderHelp GitLike.spec)

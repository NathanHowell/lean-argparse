import ArgParse.Spec.AST

namespace ArgParse.Examples

open ArgParse.Spec

private def rootMeta : Meta :=
  { name := "xargs0", help? := some "Minimal xargs-style demo." }

private def rootCmd : CmdSpec :=
  { name := "xargs0", «meta» := rootMeta }

/-- Specification for the xargs example (placeholder). -/
def xargsSpec : AppSpec :=
  { name := "xargs0", root := rootCmd }

end ArgParse.Examples

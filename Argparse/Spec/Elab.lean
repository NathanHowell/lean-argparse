/-!
# ArgParse.Spec.Elab

Scaffold for elaborating the specification AST into runtime parsers.
-/

import Argparse.Core.Parser
import Argparse.Spec.AST

namespace ArgParse.Spec

open ArgParse

/-- Placeholder payload type for elaborated commands (to be refined). -/
abbrev Payload := Unit

/-- Interpret a flag spec; currently a stub returning `false`. -/
def interpretFlag (_ : FlagSpec) : Parser Bool :=
  Parser.pure false

/-- Interpret an option spec; currently returns `Unit` while the real parser is pending. -/
def interpretOption (_ : ItemSpec) : Parser Payload :=
  Parser.pure ()

/-- Interpret a positional spec; stubbed until the parser pipeline is implemented. -/
def interpretPositional (_ : ItemSpec) : Parser Payload :=
  Parser.pure ()

/-- Elaborate an item within a command. -/
def elaborateItem (item : ItemSpec) : Parser Payload :=
  match item with
  | .flag spec =>
      -- TODO: thread the boolean flag into the eventual payload.
      interpretFlag spec *> Parser.pure ()
  | .opt _ => interpretOption item
  | .pos _ => interpretPositional item

/-- Elaborate a command and all of its child items; currently returns `Unit`. -/
def elaborateCommand (cmd : CmdSpec) : Parser Payload :=
  cmd.args.foldl (init := Parser.pure ()) fun acc item =>
    Parser.seqLeft acc (elaborateItem item)

/-- Entry point: elaborate the root application spec. -/
def elaborateApp (app : AppSpec) : Parser Payload :=
  elaborateCommand app.root

end ArgParse.Spec

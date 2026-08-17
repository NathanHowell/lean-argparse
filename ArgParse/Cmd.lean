import ArgParse.Builder

/-!
# ArgParse.Cmd

First-token dispatch over named alternatives. The tree is data, walkable to
arbitrary depth, with opaque `P` leaves.

A verb is declared once. `Cmd.leaf "greet" meta (AppCommand.greet <$> greetP)`
carries the name, the description, and the typed payload in one expression, and
the two interpreters below read that single declaration: `toParser` builds the
runtime dispatch from Layer 1's `subcommand`, `toCmdSpec` erases the payload
type for the renderers. Because both walk the same `subs` list, the names help
prints and the names the parser dispatches on are the same names.

Typed verbs need no stringly recovery step: a `Cmd AppCommand` is an ordinary
`Functor` use over the application's own inductive.
-/

namespace ArgParse

open ArgParse.Spec

/-- A command tree over payload type `α`. -/
inductive Cmd (α : Type) where
  /-- A terminal command: a name, its metadata, and the parser for its payload. -/
  | leaf (name : String) («meta» : Meta) (p : P α)
  /-- An interior command owning global items that apply to any descendant.

  The globals parse to `α → α` and are applied to whichever leaf result the
  dispatch reaches, which is what makes `tool --verbose sub ...` work. A node
  with no globals passes `pure id`. -/
  | node (name : String) («meta» : Meta) (globals : P (α → α)) (subs : List (Cmd α))

namespace Cmd

/-- The name this command is dispatched on. -/
def name : Cmd α → String
  | .leaf n _ _ => n
  | .node n _ _ _ => n

/-- Documentation metadata for this command. -/
def «meta» : Cmd α → Meta
  | .leaf _ m _ => m
  | .node _ m _ _ => m

/-! ### Interpretation as a parser -/

mutual

/-- Build the runtime parser for a command.

A node parses its globals *scoped to the tokens before the first subcommand
name*, so a parent's flags never reach into a child's segment, then dispatches
on the next token and applies the globals to the child's result. Neither form
consumes its own name: the parent's `subcommand` already did that, and the root
is never named in argv. -/
def toParser : Cmd α → Parser α
  | .leaf _ _ p => p.run
  | .node _ _ globals subs =>
      let names := subs.map Cmd.name
      let dispatch := Core.subcommand (toSubcommands subs)
      fun st =>
        match Core.scopedPre names globals.run st with
        | .err e => .err e
        | .ok f st' =>
            match dispatch st' with
            | .ok a st'' => .ok (f a) st''
            | .err e => .err e

/-- Dispatch entries for a list of sibling commands. -/
def toSubcommands : List (Cmd α) → List (Core.Subcommand α)
  | [] => []
  | c :: rest => { name := c.name, parser := toParser c } :: toSubcommands rest

end

/-! ### Interpretation as a render model -/

mutual

/-- Erase the payload type, handing the renderers the recursive `CmdSpec` they
consume. The item list is read off the same `Doc` the parser was paired with. -/
def toCmdSpec : Cmd α → CmdSpec
  | .leaf n m p => .mk n m (Doc.items p.doc) []
  | .node n m globals subs => .mk n m (Doc.items globals.doc) (toCmdSpecs subs)

/-- `toCmdSpec` over a list of sibling commands. -/
def toCmdSpecs : List (Cmd α) → List CmdSpec
  | [] => []
  | c :: rest => toCmdSpec c :: toCmdSpecs rest

end

/-- Wrap a command tree as an application descriptor for the renderers. -/
def toAppSpec (c : Cmd α) (version? : Option String := none)
    (epilog? : Option String := none) : AppSpec :=
  { name := c.name
  , version? := version?
  , about? := c.«meta».help?
  , epilog? := epilog?
  , root := toCmdSpec c }

/-! ### Walking the tree

The runner needs to answer "which command do these leading tokens name?" so it
can render help for the command the user actually asked about rather than for
the root. -/

mutual

/-- Descend the tree along the leading non-option tokens, returning the deepest
command reached and the tokens that were not verb names. -/
def descend : Cmd α → List String → Cmd α × List String
  | c, [] => (c, [])
  | c, token :: rest =>
      match c with
      | .leaf _ _ _ => (c, token :: rest)
      | .node _ _ _ subs =>
          match findSub subs token with
          | Option.some child => descend child rest
          | Option.none => (c, token :: rest)

/-- Find the sibling named `token`, if any. -/
def findSub : List (Cmd α) → String → Option (Cmd α)
  | [], _ => Option.none
  | c :: rest, token => if c.name = token then Option.some c else findSub rest token

end

/-- Names this command dispatches on. Empty for a leaf. -/
def subNames : Cmd α → List String
  | .leaf _ _ _ => []
  | .node _ _ _ subs => subs.map Cmd.name

/-- Items declared directly on this command. -/
def items : Cmd α → List ItemSpec
  | .leaf _ _ p => Doc.items p.doc
  | .node _ _ globals _ => Doc.items globals.doc

end Cmd

end ArgParse

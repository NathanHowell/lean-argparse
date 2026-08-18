import ArgParse.Doc

/-!
# ArgParse.Spec.AST

The top of the render model: the command tree the help, man, and completion
renderers read.

`CmdSpec` carries a `Doc` rather than a flat item list. The list is still what
most renderers want, so `CmdSpec.args` computes it — but a usage synopsis wants
more than the list: `(-a | -b)` is a fact about how the items compose, and
composition is exactly what flattening throws away. Keeping the tree and
deriving the list means the two can never disagree.

`ArgParse.Spec.Item` holds the leaf types this builds on, and is re-exported by
importing this file. Nothing here is written by hand at the application level:
Layer 4's `Cmd.toCmdSpec` produces the `CmdSpec`.
-/

namespace ArgParse.Spec

open ArgParse

/-- Command tree used by the renderers: a name, metadata, local items, and
subcommands. Produced by `Cmd.toCmdSpec`, never written by hand.

This is an `inductive` rather than a `structure` on purpose. A structure whose
field recurses through `List CmdSpec` admits no structural measure, so every
renderer over it would have to be `partial` — and Layer 6 asks for rendering
totality on every constructible tree. Matching on the constructor gives Lean the
nested recursion it needs; the projections below restore field syntax. -/
inductive CmdSpec where
  /-- A command with its name, metadata, description tree, and subcommands. -/
  | mk (name : String) («meta» : Meta) (doc : Doc) (subs : List CmdSpec)

namespace CmdSpec

/-- Command name used in documentation and subcommand dispatch. -/
@[inline] def name : CmdSpec → String
  | .mk n _ _ _ => n

/-- Metadata surfaced in help/man output (about text, etc.). -/
@[inline] def «meta» : CmdSpec → Meta
  | .mk _ m _ _ => m

/-- How this command's items compose: the description the parser was paired
with, payload-free. Read by the usage synopsis, which needs the alternations. -/
@[inline] def doc : CmdSpec → Doc
  | .mk _ _ d _ => d

/-- Items (flags/options/positionals) supported by the command, in order.

Computed from `doc` rather than stored beside it. Every renderer but the
synopsis wants the flat list, and deriving it is what keeps the list and the
tree from ever disagreeing about which items exist. -/
@[inline] def args (c : CmdSpec) : List ItemSpec :=
  Doc.items c.doc

/-- Nested subcommands available beneath this command. -/
@[inline] def subs : CmdSpec → List CmdSpec
  | .mk _ _ _ s => s

end CmdSpec

/-- Application-level descriptor wrapping the root command with the facts the
runner needs but a single command does not carry. -/
structure AppSpec where
  /-- Application name rendered in docs and errors. -/
  name     : String
  /-- Optional version string surfaced in `--version` style output. -/
  version? : Option String := none
  /-- Optional summary/description for the application. -/
  about?   : Option String := none
  /-- Optional epilog text appended to generated help/man pages. -/
  epilog?  : Option String := none
  /-- Root command specification describing flags/options/subcommands. -/
  root     : CmdSpec

end ArgParse.Spec

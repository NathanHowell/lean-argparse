import ArgParse.Doc.Usage

/-!
# ArgParse.Doc.Completion

Completion candidates derived by walking the render model to the command the
user has typed so far, then offering that command's verbs and item lexemes.

The walk mirrors dispatch: a token that names a child descends, anything else is
skipped. Both read the same `CmdSpec`, so a verb that completes is a verb that
parses.
-/

namespace ArgParse.Doc

open ArgParse.Spec

mutual

/-- Descend the spec tree along whichever tokens name children. -/
def specAt : CmdSpec → List String → CmdSpec
  | cmd, [] => cmd
  | .mk name info args subs, token :: rest =>
      match findSpec subs token with
      | Option.some child => specAt child rest
      | Option.none => specAt (.mk name info args subs) rest

/-- Find the child named `token`. -/
def findSpec : List CmdSpec → String → Option CmdSpec
  | [], _ => Option.none
  | cmd :: rest, token => if cmd.name = token then Option.some cmd else findSpec rest token

end

/-- Items declared along the path these tokens name, innermost last.

An interior command's globals stay legal inside its children, so diagnosing an
unrecognised option needs every item on the path, not just the deepest one.
Recursion is on fuel for the same reason `Cmd.descendFuel` is: naming a child
shrinks the tree, skipping a token shrinks the input. -/
def pathItemsFuel : Nat → CmdSpec → List String → List ItemSpec
  | 0, cmd, _ => cmd.args
  | _, cmd, [] => cmd.args
  | fuel + 1, .mk name info args subs, token :: rest =>
      match findSpec subs token with
      | Option.some child => args ++ pathItemsFuel fuel child rest
      | Option.none => pathItemsFuel fuel (.mk name info args subs) rest

/-- Items legal at the command these tokens name, ancestors included. -/
@[inline] def pathItems (cmd : CmdSpec) (tokens : List String) : List ItemSpec :=
  pathItemsFuel tokens.length cmd tokens

/-- Every lexeme the command answers to: its children's names and its items'
long and short forms. Positionals contribute their choices, when they have any. -/
def candidatesFor (cmd : CmdSpec) : List String :=
  let visible := visibleItems cmd.args
  let verbs := cmd.subs.map (·.name)
  let lexemes := visible.filter (fun i => i.kind != .positional) |>.flatMap (·.lexemes)
  let choices := visible.filterMap (·.choices?) |>.flatten
  (verbs ++ lexemes ++ choices).eraseDups

/-- Candidates after the words typed so far.

When the previous word is an option that takes a value and enumerates its
admissible ones, those values are the only candidates. -/
def completeAt (cmd : CmdSpec) (words : List String) : List String :=
  let here := specAt cmd words
  let previous? := words.getLast?
  let enumerated :=
    match previous? with
    | Option.none => Option.none
    | Option.some word =>
        (visibleItems here.args).find? (fun i =>
          i.kind == .option && i.lexemes.contains word) |>.bind (·.choices?)
  match enumerated with
  | Option.some choices => choices
  | Option.none => candidatesFor here

/-- Newline-separated candidates, the shape a shell completion hook consumes. -/
def renderCompletion (spec : AppSpec) (words : List String := []) : String :=
  String.intercalate "\n" (completeAt spec.root words)

end ArgParse.Doc

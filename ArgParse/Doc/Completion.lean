import ArgParse.Doc.Usage

/-!
# ArgParse.Doc.Completion

Completion candidates derived by walking the render model to the command the
user has typed so far, then offering that command's verbs and item lexemes.

The walk mirrors dispatch: a token that names a child descends, a value-taking
option lexeme takes the token after it with it, and anything else is skipped.
Both read the same `CmdSpec`, so a verb that completes is a verb that parses --
and the same rule in `Cmd.descendFuel` keeps the two walks agreeing about which
command the user is inside.
-/

namespace ArgParse.Doc

open ArgParse.Spec

/-- Find the child named `token`. -/
def findSpec : List CmdSpec → String → Option CmdSpec
  | [], _ => Option.none
  | cmd :: rest, token => if cmd.name = token then Option.some cmd else findSpec rest token

/-- Descend the spec tree along whichever tokens name children, stepping over
the value of any option that takes one.

Fuel, for the same reason `Cmd.descendFuel` and `pathItemsFuel` use it: naming a
child shrinks the tree while skipping a token shrinks the input, and stepping
over a value shrinks it by two. No one structural measure covers all three. -/
def specAtFuel : Nat → CmdSpec → List String → CmdSpec
  | 0, cmd, _ => cmd
  | _, cmd, [] => cmd
  | fuel + 1, .mk name info args subs, token :: rest =>
      match findSpec subs token with
      | Option.some child => specAtFuel fuel child rest
      | Option.none =>
          if (valueLexemes args).contains token then
            specAtFuel fuel (.mk name info args subs) (rest.drop 1)
          else
            specAtFuel fuel (.mk name info args subs) rest

/-- The command these tokens name. -/
@[inline] def specAt (cmd : CmdSpec) (tokens : List String) : CmdSpec :=
  specAtFuel tokens.length cmd tokens

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
      | Option.none =>
          if (valueLexemes args).contains token then
            pathItemsFuel fuel (.mk name info args subs) (rest.drop 1)
          else
            pathItemsFuel fuel (.mk name info args subs) rest

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

/-! ### Installable completion scripts

`renderCompletion` answers one query. A shell needs a hook that asks the
question, which is what these emit.

Every script is the same three lines of shell: take the words typed before the
cursor, drop the program name, hand them back to the binary's own completion
flag, and offer the newline-separated result. Nothing about the command tree is
baked into the script, so it never goes stale — adding a subcommand changes what
the binary answers, not what the user has installed. -/

/-- Shells with a completion script generator. -/
inductive Shell where
  /-- GNU bash, via `complete -F`. -/
  | bash
  /-- Z shell, via `compdef`. -/
  | zsh
  /-- fish, via `complete -c`. -/
  | fish
deriving Repr, DecidableEq

namespace Shell

/-- Every shell that can be named on the command line. -/
def all : List Shell := [.bash, .zsh, .fish]

/-- The name the user types. -/
def name : Shell → String
  | .bash => "bash"
  | .zsh => "zsh"
  | .fish => "fish"

/-- Parse a shell name, case-sensitively. -/
def ofString? (s : String) : Option Shell :=
  all.find? (fun sh => sh.name == s)

end Shell

/-- Make an identifier a shell will accept, by replacing anything that is not a
letter, digit, or underscore. Program names routinely contain `-`. -/
def shellIdent (prog : String) : String :=
  String.ofList (prog.toList.map fun c =>
    if c.isAlphanum || c == '_' then c else '_')

/-- A completion script for `prog`, calling back into it via `flag`.

Each script is meant to be evaluated, not installed as a file — one line in a
shell config, printed at the top of the script itself. The autoload conventions
(`_prog` in `$fpath` for zsh, `completions/prog.fish` for fish) would work too,
but they need the file in the right place under the right name, and the eval
form is the same instruction for all three shells.

`prog` is both the command to complete and the command to invoke, so it must be
on `PATH` — the same assumption every generated completion script makes. -/
def renderCompletionScript (prog : String) (flag : String) : Shell → String
  | .bash =>
      let fn := "_" ++ shellIdent prog ++ "_complete"
      String.intercalate "\n"
        [ "# Add to ~/.bashrc:  eval \"$(" ++ prog ++ " --completion-script bash)\""
        , fn ++ "() {"
        , "  local cur prev_words candidates"
        , "  cur=\"${COMP_WORDS[COMP_CWORD]}\""
        , "  prev_words=(\"${COMP_WORDS[@]:1:COMP_CWORD-1}\")"
        , "  candidates=\"$(" ++ prog ++ " " ++ flag ++ " \"${prev_words[@]}\" 2>/dev/null)\""
        , "  local IFS=$'\\n'"
        , "  COMPREPLY=($(compgen -W \"${candidates}\" -- \"${cur}\"))"
        , "}"
        , "complete -F " ++ fn ++ " " ++ prog
        , "" ]
  | .zsh =>
      let fn := "_" ++ shellIdent prog
      String.intercalate "\n"
        [ "# Add to ~/.zshrc after compinit:"
            ++ "  eval \"$(" ++ prog ++ " --completion-script zsh)\""
        , fn ++ "() {"
        , "  local -a candidates"
        , "  candidates=(\"${(@f)$(" ++ prog ++ " " ++ flag
            ++ " ${words[2,CURRENT-1]} 2>/dev/null)}\")"
        , "  compadd -- ${candidates}"
        , "}"
        , "compdef " ++ fn ++ " " ++ prog
        , "" ]
  | .fish =>
      let fn := "__" ++ shellIdent prog ++ "_complete"
      String.intercalate "\n"
        [ "# Add to ~/.config/fish/config.fish:  "
            ++ prog ++ " --completion-script fish | source"
        , "function " ++ fn
        , "    set -l tokens (commandline -opc)"
        , "    " ++ prog ++ " " ++ flag ++ " $tokens[2..-1]"
        , "end"
        , "complete -c " ++ prog ++ " -f -a '(" ++ fn ++ ")'"
        , "" ]

end ArgParse.Doc

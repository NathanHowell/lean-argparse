import ArgParse.Exec

/-!
# ArgParse.Tests.Exec

Integration coverage over `Cmd`/`P`/`Exec`, restoring what the retired
`Partial`-runner tests used to check: nested dispatch, order-insensitive
scanning, globals on interior nodes, and the runner's own builtins.
-/

namespace ArgParse.Tests

open ArgParse ArgParse.Builder ArgParse.Spec

/-- Payload for the nested-dispatch fixture. -/
inductive Sample where
  /-- The `grand` leaf: a mode, a payload, and whether `--switch` was seen. -/
  | grand (rootMode : String) (childMode : List String) (leafMode : String)
      (payload : String) (switch : Bool)
  /-- The `child` leaf reached without descending further. -/
  | childOnly (switch : Bool)
  deriving Repr, DecidableEq

/-- Globals are threaded as `α → α`, so the fixture records them by rewriting
the payload the leaf produced. -/
private def withRootMode (mode : String) : Sample → Sample
  | .grand _ c l p s => .grand mode c l p s
  | .childOnly s => .childOnly s

private def withChildMode (modes : List String) : Sample → Sample
  | .grand r _ l p s => .grand r modes l p s
  | .childOnly s => .childOnly s

private def withSwitch (switch : Bool) : Sample → Sample
  | .grand r c l p _ => .grand r c l p switch
  | .childOnly _ => .childOnly switch

/-- `sample grand` requires a leaf mode and a positional payload. -/
private def grandP : P Sample :=
  (fun leafMode payload => Sample.grand "" [] leafMode payload false)
    <$> option String "leaf-mode"
    <*> positional "payload"

private def sampleApp : Cmd Sample :=
  .node "sample" { name := "sample" }
    (withRootMode <$> optionD "root-mode" (default := "") )
    [ .node "child" { name := "child" }
        ((fun switch modes => withSwitch switch ∘ withChildMode modes)
          <$> flag "switch"
          <*> options String "child-mode")
        [ .leaf "grand" { name := "grand" } grandP ] ]

private def expectTrue (cond : Bool) (msg : String) : Except String Unit :=
  if cond then .ok () else .error msg

private def expectSample (label : String) (argv : List String) (want : Sample) :
    Except String Unit :=
  match Exec.exec sampleApp argv with
  | .ok value => expectTrue (value == want) s!"{label}: got {repr value}, want {repr want}"
  | .output text => .error s!"{label}: unexpected runner output {text}"
  | .error text => .error s!"{label}: unexpected error {text}"

/-- Tokens in declaration order, sentinel-guarded payload. -/
private def nestedTokens : List String :=
  [ "--root-mode", "alpha"
  , "child"
  , "--switch"
  , "--child-mode", "beta"
  , "--child-mode", "gamma"
  , "grand"
  , "--leaf-mode", "delta"
  , "--", "payload" ]

/-- The same invocation with each command's items shuffled within its own
segment and the positional supplied mid-stream. -/
private def shuffledTokens : List String :=
  [ "--root-mode", "alpha"
  , "child"
  , "--child-mode", "beta"
  , "--switch"
  , "--child-mode", "gamma"
  , "grand"
  , "payload"
  , "--leaf-mode", "delta" ]

private def wanted : Sample :=
  .grand "alpha" ["beta", "gamma"] "delta" "payload" true

/-- Dispatch descends two levels, and each command's globals reach the leaf. -/
private def checkNested : Except String Unit :=
  expectSample "nested" nestedTokens wanted

/-- Order within a segment does not matter. -/
private def checkShuffled : Except String Unit :=
  expectSample "shuffled" shuffledTokens wanted

/-- A parent's items do not reach into a child's segment.

`--root-mode` belongs to the root node, whose globals are scoped to the tokens
before the first verb. Supplying it after `child` puts it where nothing accepts
it, so it is left over and reported -- rather than silently satisfying the
parent from inside the child's arguments. -/
private def checkScoping : Except String Unit := do
  match Exec.exec sampleApp
      ["child", "grand", "--leaf-mode", "delta", "--root-mode", "late", "payload"] with
  | .error _ => .ok ()
  | other => .error s!"expected --root-mode after the verb to be out of scope, got {repr other}"
  match Exec.exec sampleApp ["--root-mode", "early", "child", "grand",
      "--leaf-mode", "delta", "payload"] with
  | .ok (.grand rootMode _ _ _ _) =>
      expectTrue (rootMode == "early") s!"expected the global to reach the leaf, got {rootMode}"
  | other => .error s!"expected a grand payload, got {repr other}"

/-- An unknown verb is reported with a suggestion. -/
private def checkUnknownVerb : Except String Unit :=
  match Exec.exec sampleApp ["chidl"] with
  | .error text =>
      expectTrue ((text.splitOn "did you mean `child`?").length == 2)
        s!"expected a suggestion for `chidl`, got: {text}"
  | other => .error s!"expected an error, got {repr other}"

/-- An unknown long option is named, rather than a token it displaced. -/
private def checkUnknownOption : Except String Unit :=
  match Exec.exec sampleApp ["child", "grand", "--leaf-mdoe", "x", "payload"] with
  | .error text =>
      expectTrue ((text.splitOn "`--leaf-mdoe`").length == 2)
        s!"expected the unknown option to be named, got: {text}"
  | other => .error s!"expected an error, got {repr other}"

/-- A misspelled verb is reported even when a later token is also illegal.

Regression: options meant for the command the user *meant* are illegal at the
node dispatch stopped at, so the unknown-option check used to fire first and
name a token the user got right. -/
private def checkUnknownVerbBeatsOption : Except String Unit := do
  match Exec.exec sampleApp ["child", "grnd", "--leaf-mode", "delta"] with
  | .error text =>
      expectTrue ((text.splitOn "`grnd`").length == 2)
        s!"expected the misspelled verb to be named, got: {text}"
      expectTrue ((text.splitOn "did you mean `grand`?").length == 2)
        s!"expected a suggestion for the verb, got: {text}"
  | other => .error s!"expected an error, got {repr other}"

/-- An option standing where a verb belongs is still reported as the option.

The guard that makes the check above work keys on the token not looking like an
option, so this is the case it must not swallow. -/
private def checkOptionWhereVerbBelongs : Except String Unit :=
  match Exec.exec sampleApp ["child", "--leaf-mode", "delta"] with
  | .error text =>
      expectTrue ((text.splitOn "`--leaf-mode`").length == 2)
        s!"expected the option to be named, got: {text}"
  | other => .error s!"expected an error, got {repr other}"

/-- `--help` renders for the command named, not for the root. -/
private def checkNestedHelp : Except String Unit :=
  match Exec.exec sampleApp ["child", "grand", "--help"] with
  | .output text =>
      expectTrue (text.startsWith "sample child grand")
        s!"expected help for the grand command, got: {text}"
  | other => .error s!"expected help output, got {repr other}"

/-- Help is found wherever it appears, including after other tokens. -/
private def checkHelpAfterOptions : Except String Unit :=
  match Exec.exec sampleApp ["--root-mode", "alpha", "child", "-h"] with
  | .output text =>
      expectTrue (text.startsWith "sample child")
        s!"expected help for the child command, got: {text}"
  | other => .error s!"expected help output, got {repr other}"

/-- `--version` is absent unless the application supplies one. -/
private def checkVersion : Except String Unit := do
  match Exec.exec sampleApp ["--version"] (cfg := { version? := some "1.2.3" }) with
  | .output text =>
      expectTrue (text == "sample 1.2.3") s!"expected the version line, got: {text}"
  | other => .error s!"expected version output, got {repr other}"
  match Exec.exec sampleApp ["--version"] with
  | .error _ => .ok ()
  | other => .error s!"expected --version to be unrecognised without a version, got {repr other}"

/-- Completion candidates follow the command named so far. -/
private def checkCompletion : Except String Unit := do
  match Exec.exec sampleApp ["--generate-completions"] with
  | .output text =>
      expectTrue ((text.splitOn "child").length ≥ 2)
        s!"expected the root's verbs, got: {text}"
  | other => .error s!"expected completion output, got {repr other}"
  match Exec.exec sampleApp ["--generate-completions", "child"] with
  | .output text =>
      expectTrue ((text.splitOn "--child-mode").length == 2)
        s!"expected the child's items, got: {text}"
  | other => .error s!"expected completion output, got {repr other}"

/-- Help text mentions every item the parser accepts. This is the correspondence
property Layer 6 proves; checking it here keeps the renderers honest in the
meantime. -/
private def checkHelpMentionsItems : Except String Unit :=
  let spec := sampleApp.toCmdSpec
  let items := Doc.pathItems spec ["child", "grand"]
  match Exec.exec sampleApp ["child", "grand", "--help"] with
  | .output text =>
      items.foldl
        (fun acc item =>
          acc.bind fun _ =>
            item.lexemes.foldl
              (fun acc' lexeme =>
                acc'.bind fun _ =>
                  expectTrue ((text.splitOn lexeme).length ≥ 2)
                    s!"help omits {lexeme}")
              (Except.ok ()))
        (Except.ok ())
  | other => .error s!"expected help output, got {repr other}"

/-! ### Bundles packing an option

`-vn5` is a flag, then an option, then its value, in one token. Which of these
parse depends on where the split has to happen, so all four combinations are
pinned -- including the one that does not work, so it cannot change silently. -/

/-- Payload for the bundle fixtures. -/
private structure Bundled where
  count : Nat
  verbose : Bool
  deriving Repr, DecidableEq

/-- The option is sequenced before the flag. -/
private def optFirstApp : Cmd Bundled :=
  .leaf "run" { name := "run" }
    ((fun c v => Bundled.mk c v)
      <$> option Nat "count" (short := 'n') <*> flag "verbose" (short := 'v'))

/-- The flag is sequenced before the option. -/
private def flagFirstApp : Cmd Bundled :=
  .leaf "run" { name := "run" }
    ((fun v c => Bundled.mk c v)
      <$> flag "verbose" (short := 'v') <*> option Nat "count" (short := 'n'))

private def expectBundled (label : String) (app : Cmd Bundled) (argv : List String)
    (want : Bundled) : Except String Unit :=
  match Exec.exec app argv with
  | .ok value => expectTrue (value == want) s!"{label}: got {repr value}, want {repr want}"
  | other => .error s!"{label}: expected a parse, got {repr other}"

/-- A bundle that *leads* with flags splits before anything scans, so it parses
whichever order the parser was written in. This is what the expansion pass in
`Cmd.toParser` buys. -/
private def checkBundleFlagsThenOption : Except String Unit := do
  expectBundled "option first" optFirstApp ["-vn5"] { count := 5, verbose := true }
  expectBundled "flag first" flagFirstApp ["-vn5"] { count := 5, verbose := true }

/-- A bundle that *leads* with the option splits during the option's own scan,
which pushes the residue back onto the stream. Only parsers that reach the flag
afterwards can see it. -/
private def checkBundleOptionThenFlag : Except String Unit := do
  expectBundled "option first" optFirstApp ["-n5v"] { count := 5, verbose := true }

/-- The unfixed half, pinned. With the flag sequenced first, the residue `-v`
appears after that flag has already run, and is reported as leftover.

This is a consequence of scanning each item once in applicative order, not an
oversight: splitting `-n5v` up front would need the value's decoder, and the
item list the expansion pass reads is type-erased. `-n5 -v` and `--count=5 -v`
work in either order. -/
private def checkBundleOptionThenFlagLimitation : Except String Unit := do
  match Exec.exec flagFirstApp ["-n5v"] with
  | .error text =>
      expectTrue ((text.splitOn "-v").length ≥ 2)
        s!"expected the residue reported as leftover, got: {text}"
  | other => .error s!"expected the documented failure, got {repr other}"
  expectBundled "detached still works" flagFirstApp ["-n5", "-v"]
    { count := 5, verbose := true }

/-- A short the command does not accept leaves its token untouched, so the
expansion pass cannot invent tokens the user never typed. -/
private def checkBundleUnknownShortUntouched : Except String Unit := do
  match Exec.exec flagFirstApp ["-xn5"] with
  | .error _ => pure ()
  | other => .error s!"expected an error for an unknown short, got {repr other}"

/-! ### Positionals and other items' tokens

A positional takes the front of the stream; scanning takes tokens from anywhere.
So a positional sequenced first used to eat whatever was in front of it. These
pin the two halves of the fix -- hoisting claimed tokens behind the rest, and
declining an option-looking token outright -- and the escape hatches that must
survive both. -/

/-- Payload for the positional fixtures. -/
private structure Cleaned where
  dir : String
  purge : Bool
  deriving Repr, DecidableEq

/-- The positional is sequenced *before* the flag, which used to be a way to
write a parser that could not work. -/
private def posFirstApp : Cmd Cleaned :=
  .leaf "clean" { name := "clean" }
    ((fun d p => Cleaned.mk (d.getD "") p)
      <$> argOpt String "dir" <*> flag "purge" (short := 'p'))

private def cleaned (label : String) (argv : List String) (dir : String) (purge : Bool) :
    Except String Unit :=
  match Exec.exec posFirstApp argv with
  | .ok value =>
      expectTrue (value == { dir := dir, purge := purge })
        s!"{label}: got {repr value}"
  | other => .error s!"{label}: expected a parse, got {repr other}"

/-- A flag ahead of the positional's value no longer displaces it. -/
private def checkPositionalSkipsFlag : Except String Unit := do
  cleaned "-p out" ["-p", "out"] "out" true
  cleaned "out -p" ["out", "-p"] "out" true

/-- With no positional value at all, the flag is still a flag -- the positional
does not fall back to consuming it. -/
private def checkPositionalDeclinesLoneFlag : Except String Unit :=
  cleaned "-p alone" ["-p"] "" true

/-- A negative number is a value, not a lexeme, so positionals still take it. -/
private def checkNegativePositional : Except String Unit := do
  let app : Cmd (Int × Int) :=
    .leaf "span" { name := "span" }
      ((fun a b => (a, b)) <$> arg Int "from" <*> arg Int "to")
  match Exec.exec app ["-5", "-3"] with
  | .ok value => expectTrue (value == (-5, -3)) s!"got {repr value}"
  | other => .error s!"expected a parse, got {repr other}"

/-- `--` remains the way to pass a positional value that starts with a dash. -/
private def checkSentinelEscapesDash : Except String Unit :=
  cleaned "-- -weird" ["--", "-weird"] "-weird" false

/-- An option's value is not in verb position, even when it spells a verb.

`--root-mode child` sets the root's mode to the string "child"; the help walk
used to read that value as a descent and document the child instead. -/
private def checkOptionValueSpellingAVerb : Except String Unit := do
  match Exec.exec sampleApp ["--root-mode", "child", "--help"] with
  | .output text =>
      expectTrue ((text.splitOn "\n").headD "" == "sample")
        s!"expected the root's help, got: {text}"
  | other => .error s!"expected help output, got {repr other}"
  match Exec.exec sampleApp ["child", "--help"] with
  | .output text =>
      expectTrue (text.startsWith "sample child")
        s!"a real verb should still route, got: {text}"
  | other => .error s!"expected help output, got {repr other}"

/-- Completion walks the same way, so it agrees about which command it is in. -/
private def checkCompletionSkipsOptionValue : Except String Unit := do
  match Exec.exec sampleApp ["--generate-completions", "--root-mode", "child"] with
  | .output text =>
      expectTrue ((text.splitOn "child").length ≥ 2)
        s!"expected the root's verbs, got: {text}"
  | other => .error s!"expected completion output, got {repr other}"

/-- A builtin bundled with a command's own flag is still a builtin.

Builtins match whole tokens, and `-vh` is not one; the runner expands bundles
against the path's items plus its own before looking. -/
private def checkBundledBuiltin : Except String Unit := do
  match Exec.exec flagFirstApp ["-vh"] with
  | .output text =>
      expectTrue ((text.splitOn "Usage").length ≥ 2)
        s!"expected help for the bundled -h, got: {text}"
  | other => .error s!"expected help output, got {repr other}"

/-- An unrecognised short names itself, rather than letting a positional swallow
it and blaming the next token. -/
private def checkUnknownShortNamed : Except String Unit := do
  match Exec.exec flagFirstApp ["-q", "5"] with
  | .error text =>
      expectTrue ((text.splitOn "-q").length ≥ 2)
        s!"expected the unknown short named, got: {text}"
  | other => .error s!"expected an error, got {repr other}"

/-- A negative number is a value, not a lexeme, so it is never diagnosed as an
unrecognised short. -/
private def checkNegativeNumberNotDiagnosed : Except String Unit := do
  expectTrue (Exec.unknownShort? ['n', 'v'] ["-5"] == none)
    "a negative number was reported as an unknown short"
  expectTrue (Exec.unknownShort? ['n', 'v'] ["-q"] == some "-q")
    "an unknown short was not reported"
  expectTrue (Exec.unknownShort? ['n', 'v'] ["-v", "-n"] == none)
    "a known short was reported as unknown"

/-- The completion script names the binary and calls back into the query flag.

The point of generating a script rather than shipping one is that it stays
correct as the command tree changes, so what it must contain is the callback,
not any verb. -/
private def checkCompletionScript : Except String Unit := do
  for (shell, hook) in
      [(Doc.Shell.bash, "complete -F"), (Doc.Shell.zsh, "compdef"),
       (Doc.Shell.fish, "complete -c")] do
    match Exec.exec sampleApp ["--completion-script", shell.name] with
    | .output text =>
        expectTrue ((text.splitOn "--generate-completions").length ≥ 2)
          s!"{shell.name} script never calls back: {text}"
        expectTrue ((text.splitOn hook).length ≥ 2)
          s!"{shell.name} script never registers itself: {text}"
        expectTrue ((text.splitOn sampleApp.name).length ≥ 2)
          s!"{shell.name} script never names the binary: {text}"
    | other => .error s!"expected a script for {shell.name}, got {repr other}"

/-- An unusable shell name is refused, and the message lists the usable ones. -/
private def checkCompletionScriptUnknownShell : Except String Unit := do
  match Exec.exec sampleApp ["--completion-script", "tcsh"] with
  | .error text =>
      expectTrue ((text.splitOn "tcsh").length ≥ 2) s!"error omits the shell: {text}"
      expectTrue ((text.splitOn "bash").length ≥ 2) s!"error omits the choices: {text}"
  | other => .error s!"expected an error, got {repr other}"

/-- Asking for a script without naming a shell is refused too. -/
private def checkCompletionScriptNoShell : Except String Unit := do
  match Exec.exec sampleApp ["--completion-script"] with
  | .error text =>
      expectTrue ((text.splitOn "shell name").length ≥ 2)
        s!"error does not say what is missing: {text}"
  | other => .error s!"expected an error, got {repr other}"

/-- A label wide enough to reach the description column still gets a separator.

`--completion-script SHELL` is the first builtin long enough to hit this, and
without the guard in `entryRow` its description abutted the label. -/
private def checkWideLabelSeparated : Except String Unit := do
  match Exec.exec sampleApp ["--help"] with
  | .output text =>
      expectTrue ((text.splitOn "SHELLPrint").length == 1)
        s!"a wide label ran into its description: {text}"
      expectTrue ((text.splitOn "SHELL  Print").length ≥ 2)
        s!"expected two spaces after the wide label: {text}"
  | other => .error s!"expected help output, got {repr other}"

/-- `P.many` keeps going through a bundle.

`-vvv` is three occurrences in one token. The repetition bound used to count
tokens, so it stopped after two and left `-v` on the stream; `State.budget`
charges per character as well, which is what makes the third reachable. -/
private def checkManyThroughBundle : Except String Unit := do
  let verbose := Builder.flag "verbose" (short := 'v')
  match (P.many verbose).run (Core.normalize ["-vvv"]) with
  | .ok values st =>
      expectTrue (values.length == 3)
        s!"expected three occurrences from -vvv, got {repr values}"
      expectTrue (st.pre.isEmpty) s!"expected the bundle consumed, got {repr st.pre}"
  | other => .error s!"expected ok result, got {repr other}"

/-! ### Usage synopses over alternations

`(-a | -b)` is the one thing a flat item list cannot say, which is why the
synopsis reads `CmdSpec.doc`. These pin both directions: an alternation becomes
a choice, and the optionality spelling -- `alt [d, none]`, one real branch --
does not. -/

/-- The synopsis of a one-command spec built from `p`. -/
private def synopsisOf {α : Type} (p : P α) : String :=
  Doc.usageLine ["tool"] ((Cmd.leaf "tool" { name := "tool" } p).toCmdSpec)

/-- Two flags joined by `<|>` render as a choice, not as two independent
optional flags. -/
private def checkChoiceSynopsis : Except String Unit := do
  let line := synopsisOf (Builder.flag "fast" (short := 'f') <|> Builder.flag "slow" (short := 's'))
  expectTrue (line == "  tool (--fast | --slow)")
    s!"expected a parenthesised choice, got {repr line}"

/-- An optional choice is bracketed rather than parenthesised: `optional`
contributes the `none` branch that makes the whole group omissible. -/
private def checkOptionalChoiceSynopsis : Except String Unit := do
  let line := synopsisOf (P.optional (Builder.flag "fast" <|> Builder.flag "slow"))
  expectTrue (line == "  tool [--fast | --slow]")
    s!"expected a bracketed choice, got {repr line}"

/-- Each branch may hold more than one item, and they stay in order within it. -/
private def checkChoiceBranchItems : Except String Unit := do
  let line := synopsisOf
    (((·, ·) <$> Builder.flag "a" <*> Builder.flag "b")
      <|> ((·, ·) <$> Builder.flag "c" <*> Builder.flag "d"))
  expectTrue (line == "  tool (--a --b | --c --d)")
    s!"expected branches to keep their items, got {repr line}"

/-- A repeated choice keeps the ellipsis the group -- not any single item --
earns, and `P.many` says the group may be skipped entirely. -/
private def checkRepeatedChoiceSynopsis : Except String Unit := do
  let line := synopsisOf (P.many (Builder.flag "fast" <|> Builder.flag "slow"))
  expectTrue (line == "  tool [(--fast | --slow)...]")
    s!"expected a zero-or-more choice, got {repr line}"

/-- `P.some` is the same group without the bracket: it must appear at least
once, which is the whole reason `Doc.many` records which one it came from. -/
private def checkSomeChoiceSynopsis : Except String Unit := do
  let line := synopsisOf (P.some (Builder.flag "fast" <|> Builder.flag "slow"))
  expectTrue (line == "  tool (--fast | --slow)...")
    s!"expected a one-or-more choice, got {repr line}"

/-- A `many` above a single item overrides that item's own arity, which is what
used to go missing: `P.many` over a required option used to render as though
exactly one were mandatory. -/
private def checkManyOverridesItemArity : Except String Unit := do
  let line := synopsisOf (P.many (Builder.option String "inc"))
  expectTrue (line == "  tool [--inc STRING...]")
    s!"expected a repeated optional item, got {repr line}"

/-- And `P.some` over the same option keeps it mandatory. -/
private def checkSomeOverridesItemArity : Except String Unit := do
  let line := synopsisOf (P.some (Builder.option String "inc"))
  expectTrue (line == "  tool --inc STRING...")
    s!"expected a repeated mandatory item, got {repr line}"

/-- The repeating builders are unaffected: their repetition already travelled on
`ItemSpec.arity`, and a `many` node above it does not double the ellipsis. -/
private def checkRepeatingBuildersUnchanged : Except String Unit := do
  let optionsLine := synopsisOf (Builder.options String "inc")
  expectTrue (optionsLine == "  tool [--inc STRING...]")
    s!"expected the repeatable option unchanged, got {repr optionsLine}"
  let argsLine := synopsisOf (Builder.args String "file")
  expectTrue (argsLine == "  tool [file...]")
    s!"expected the repeatable positional unchanged, got {repr argsLine}"

/-- An optional option is *not* a choice. Its document is `alt [item, none]`,
one real branch, and it keeps the bracketing it always had. -/
private def checkOptionalIsNotAChoice : Except String Unit := do
  let line := synopsisOf (Builder.optionOpt Nat "count" (short := 'n'))
  expectTrue (line == "  tool [--count NAT]")
    s!"expected the optionality spelling to render as before, got {repr line}"

/-- A choice whose branches are all hidden renders as nothing, the same way a
hidden item does. -/
private def checkHiddenChoiceOmitted : Except String Unit := do
  let line := synopsisOf
    (Builder.flag "a" (hidden := true) <|> Builder.flag "b" (hidden := true))
  expectTrue (line == "  tool")
    s!"expected a hidden choice to disappear, got {repr line}"

/-- Items outside the choice keep their old treatment, choice and all: loose
switches first, then choices, then positionals. -/
private def checkChoiceAmongLooseItems : Except String Unit := do
  let line := synopsisOf
    ((·, ·, ·) <$> (Builder.flag "x" <|> Builder.flag "y")
      <*> Builder.option String "who" <*> Builder.arg String "name")
  expectTrue (line == "  tool --who STRING (--x | --y) name")
    s!"expected loose items around the choice, got {repr line}"

/-- Integration checks executed by `lake test`. -/
def execChecks : List (String × Except String Unit) :=
  [ ("nested dispatch", checkNested)
  , ("order insensitivity", checkShuffled)
  , ("parent items stay out of child segments", checkScoping)
  , ("unknown verb suggestion", checkUnknownVerb)
  , ("unknown option named", checkUnknownOption)
  , ("unknown verb beats unknown option", checkUnknownVerbBeatsOption)
  , ("option where a verb belongs", checkOptionWhereVerbBelongs)
  , ("help for the named command", checkNestedHelp)
  , ("help found after other tokens", checkHelpAfterOptions)
  , ("version builtin", checkVersion)
  , ("completion follows the path", checkCompletion)
  , ("help mentions every item", checkHelpMentionsItems)
  , ("many keeps going through a bundle", checkManyThroughBundle)
  , ("completion scripts call back", checkCompletionScript)
  , ("unknown shell refused", checkCompletionScriptUnknownShell)
  , ("missing shell refused", checkCompletionScriptNoShell)
  , ("wide labels keep their separator", checkWideLabelSeparated)
  , ("bundle leading with flags", checkBundleFlagsThenOption)
  , ("bundle leading with an option", checkBundleOptionThenFlag)
  , ("bundle residue needs a later flag", checkBundleOptionThenFlagLimitation)
  , ("unknown short leaves its token alone", checkBundleUnknownShortUntouched)
  , ("alternation renders as a choice", checkChoiceSynopsis)
  , ("optional choice is bracketed", checkOptionalChoiceSynopsis)
  , ("choice branches keep their items", checkChoiceBranchItems)
  , ("repeated choice keeps its ellipsis", checkRepeatedChoiceSynopsis)
  , ("one-or-more choice is not bracketed", checkSomeChoiceSynopsis)
  , ("many overrides an item's arity", checkManyOverridesItemArity)
  , ("some overrides an item's arity", checkSomeOverridesItemArity)
  , ("repeating builders unchanged", checkRepeatingBuildersUnchanged)
  , ("optionality is not a choice", checkOptionalIsNotAChoice)
  , ("hidden choice disappears", checkHiddenChoiceOmitted)
  , ("choice among loose items", checkChoiceAmongLooseItems)
  , ("bundled builtin is found", checkBundledBuiltin)
  , ("unknown short names itself", checkUnknownShortNamed)
  , ("negative numbers are values", checkNegativeNumberNotDiagnosed)
  , ("option value spelling a verb", checkOptionValueSpellingAVerb)
  , ("completion skips option values", checkCompletionSkipsOptionValue)
  , ("positional skips a flag", checkPositionalSkipsFlag)
  , ("positional declines a lone flag", checkPositionalDeclinesLoneFlag)
  , ("negative numbers stay positional", checkNegativePositional)
  , ("sentinel escapes a dash value", checkSentinelEscapesDash)
  ]

end ArgParse.Tests

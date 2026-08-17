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
  ]

end ArgParse.Tests

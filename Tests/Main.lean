import ArgParse.Core.Runner
import ArgParse.Tests.Unit

open ArgParse
open ArgParse.Spec

private def rootModeSpec : ItemSpec :=
  ItemSpec.opt
    (α := String)
    { long? := some "root-mode"
      , «meta» := { name := "root-mode" }
      , arity := .one }

private def childSwitchSpec : ItemSpec :=
  ItemSpec.flag
    { long? := some "switch"
      , «meta» := { name := "child-switch" } }

private def childModeSpec : ItemSpec :=
  ItemSpec.opt
    (α := String)
    { long? := some "child-mode"
      , «meta» := { name := "child-mode" }
      , arity := .many }

private def grandModeSpec : ItemSpec :=
  ItemSpec.opt
    (α := String)
    { long? := some "leaf-mode"
      , «meta» := { name := "leaf-mode" }
      , arity := .one }

private def grandPayloadSpec : ItemSpec :=
  ItemSpec.pos
    (α := String)
    { «meta» := { name := "payload" }
      , arity := .one }

private def grandCmd : CmdSpec :=
  { name := "grand"
    , «meta» := { name := "grand" }
    , args := [grandModeSpec, grandPayloadSpec] }

private def childCmd : CmdSpec :=
  { name := "child"
    , «meta» := { name := "child" }
    , args := [childSwitchSpec, childModeSpec]
    , subs := [grandCmd] }

private def rootCmd : CmdSpec :=
  { name := "sample"
    , «meta» := { name := "sample" }
    , args := [rootModeSpec]
    , subs := [childCmd] }

private def sampleApp : AppSpec :=
  { name := "sample"
    , root := rootCmd }

private def tokens : List String :=
  [ "--root-mode", "alpha"
  , "child"
  , "--switch"
  , "--child-mode", "beta"
  , "--child-mode", "gamma"
  , "grand"
  , "--leaf-mode", "delta"
  , "--", "payload" ]

/-- Same command tree, but flags/options shuffled within each command's segment
and the leaf positional supplied mid-stream: order-insensitive scanning must
produce the same summary as `tokens`. -/
private def shuffledTokens : List String :=
  [ "--root-mode", "alpha"
  , "child"
  , "--child-mode", "beta"
  , "--switch"
  , "--child-mode", "gamma"
  , "grand"
  , "payload"
  , "--leaf-mode", "delta" ]

private def verify (cond : Bool) (msg : String) : IO Bool :=
  if cond then
    pure true
  else
    IO.eprintln msg *> pure false

private def testNestedSubcommand (label : String) (argv : List String) : IO Bool := do
  let outcome := ArgParse.runSummary sampleApp argv
  match outcome.result with
  | .ok summary =>
      let rootVals := Partial.Summary.optionValues summary "root-mode"
      let childVals := Partial.Summary.optionValues summary "child-mode"
      let leafVals := Partial.Summary.optionValues summary "leaf-mode"
      let payloadVals := Partial.Summary.positionalValues summary "payload"
      let switchSeen := Partial.Summary.flagValue? summary "child-switch"
      let checks ←
        [ verify (rootVals = ["alpha"]) s!"{label}: expected root-mode=alpha"
        , verify (childVals = ["beta", "gamma"]) s!"{label}: expected child-mode beta then gamma"
        , verify (childVals.getLast? = some "gamma") s!"{label}: expected last child-mode to win"
        , verify (leafVals = ["delta"]) s!"{label}: expected leaf-mode=delta"
        , verify (payloadVals = ["payload"]) s!"{label}: expected positional payload"
        , verify (switchSeen = some true) s!"{label}: expected child-switch flag to be set" ]
          |>.mapM id
      return checks.all id
  | .err err =>
      IO.eprintln s!"{label}: unexpected parse error: {repr err}" *> pure false
  | other =>
      IO.eprintln s!"{label}: unexpected runner result: {repr other}" *> pure false

private def runCheck (label : String) (check : Except String Unit) : IO Bool :=
  match check with
  | .ok _ => pure true
  | .error msg =>
      IO.eprintln s!"[FAIL] {label}: {msg}" *> pure false

/-- Run every unit and golden check, reporting failures on stderr. -/
def main : IO UInt32 := do
  let unitChecks ← ArgParse.Tests.runtimeChecks.mapM (fun (label, chk) => runCheck label chk)
  let nestedOk ← testNestedSubcommand "nested" tokens
  let shuffledOk ← testNestedSubcommand "shuffled" shuffledTokens
  let allOk := nestedOk && shuffledOk && unitChecks.all id
  pure <| if allOk then 0 else 1

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

private def verify (cond : Bool) (msg : String) : IO Bool :=
  if cond then
    pure true
  else
    IO.eprintln msg *> pure false

private def testNestedSubcommand : IO Bool := do
  let outcome := ArgParse.runSummary sampleApp tokens
  match outcome.result with
  | .ok summary =>
      let rootVals := Partial.Summary.optionValues summary "root-mode"
      let childVals := Partial.Summary.optionValues summary "child-mode"
      let leafVals := Partial.Summary.optionValues summary "leaf-mode"
      let payloadVals := Partial.Summary.positionalValues summary "payload"
      let switchSeen := Partial.Summary.flagValue? summary "child-switch"
      let checks ←
        [ verify (rootVals = ["alpha"]) "expected root-mode=alpha"
        , verify (childVals = ["beta", "gamma"]) "expected child-mode beta then gamma"
        , verify (childVals.getLast? = some "gamma") "expected last child-mode to win"
        , verify (leafVals = ["delta"]) "expected leaf-mode=delta"
        , verify (payloadVals = ["payload"]) "expected positional payload"
        , verify (switchSeen = some true) "expected child-switch flag to be set" ]
          |>.mapM id
      return checks.all id
  | .err err =>
      IO.eprintln s!"unexpected parse error: {repr err}" *> pure false
  | other =>
      IO.eprintln s!"unexpected runner result: {repr other}" *> pure false

private def runCheck (label : String) (check : Except String Unit) : IO Bool :=
  match check with
  | .ok _ => pure true
  | .error msg =>
      IO.eprintln s!"[FAIL] {label}: {msg}" *> pure false

def main : IO UInt32 := do
  let unitChecks ← ArgParse.Tests.runtimeChecks.mapM (fun (label, chk) => runCheck label chk)
  let nestedOk ← testNestedSubcommand
  let allOk := nestedOk && unitChecks.all id
  pure <| if allOk then 0 else 1

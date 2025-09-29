import ArgParse.Core.Normalize
import ArgParse.Core.Value
import ArgParse.Spec.AST
import ArgParse.Spec.Elab
import ArgParse.Core.Runner
import ArgParse.Proofs.Sentinel

namespace ArgParse.Tests

open ArgParse Core ArgParse.Proofs

open ArgParse.Spec

#guard ((ArgParse.FromArg.run (α := Nat) "42").toOption = some 42)
#guard ((ArgParse.FromArg.run (α := Int) "-5").toOption = some (-5))
#guard ((ArgParse.FromArg.run (α := Bool) "TRUE").toOption = some true)

/-- Simple enum for exercising `FromArg.enumFrom`. -/
inductive Toggle
  | /-- Enabled state. -/ on
  | /-- Disabled state. -/ off
deriving DecidableEq

instance : ArgParse.FromArg Toggle :=
  ArgParse.FromArg.enumFrom [("on", Toggle.on), ("off", Toggle.off)]

#guard ((ArgParse.FromArg.run (α := Toggle) "OFF").toOption = some Toggle.off)
#guard ((ArgParse.FromArg.run (α := Toggle) "invalid").toOption = none)

#guard (normalize ["-n", "5", "--", "file"]).pre = ["-n", "5"]
#guard (normalize ["-n", "5", "--", "file"]).post = ["file"]
#guard (normalize ["alpha", "beta"]).post = []
#guard
  (let tokens := ["-v", "--", "tail"]
   let st := normalize tokens
   tokens = st.pre ++ "--" :: st.post)

-- Spec/Elab sanity: elaborate a simple flag + option and parse.
open ArgParse.Spec
/-– Helper to construct minimal metadata for test specs. -/
private def mkMeta (n : String) : Meta := { name := n }
private def flagSpec : FlagSpec := { long? := some "verbose", «meta» := mkMeta "verbose" }
private def optSpec : OptSpec String := { long? := some "name", «meta» := mkMeta "name", arity := .one }
private def cmd : CmdSpec := { name := "app", «meta» := mkMeta "app", args := [.flag flagSpec, .opt optSpec] }

#guard
  (let p := ArgParse.Spec.elaborateCommand cmd
   let st := normalize ["--verbose", "--name=foo"]
   match p st with
   | .ok part _ => part.flags.any (fun (k,v) => k = "verbose" ∧ v = true) ∧
                   part.options.any (fun (k,_) => k = "name")
    | _ => False)

-- Option forms: equals and concatenated short are both accepted.
open ArgParse.Spec in
private def shortN : Short := { c := 'n', ok := by decide }
private def optShortLong : OptSpec String := { short? := some shortN, long? := some "name", «meta» := mkMeta "name", arity := .one }
private def cmdOpt : CmdSpec := { name := "app", «meta» := mkMeta "app", args := [.opt optShortLong] }

#guard
  (let p := ArgParse.Spec.elaborateCommand cmdOpt
   let st := normalize ["--name=foo"]
   match p st with
   | .ok part _ => part.options.any (fun (k,_) => k = "name")
   | _ => False)

#guard
  (let p := ArgParse.Spec.elaborateCommand cmdOpt
   let st := normalize ["-nfoo"]
   match p st with
   | .ok part _ => part.options.any (fun (k,_) => k = "name")
   | _ => False)

open ArgParse in
#guard
  (let app : ArgParse.Spec.AppSpec := { name := "app", root := cmd }
   let out := ArgParse.runRaw app ["--help", "--name", "x"]
   match out.result with
   | .help _ => True
   | _ => False)

-- Subcommand recursion tests will be added after we stabilise option semantics
-- within recursive contexts.

-- Minimal recursion regression: ensure descending to a child yields `.ok`.
open ArgParse.Spec in
private def childOpt : OptSpec String := { long? := some "name", «meta» := mkMeta "name", arity := .one }
private def childCmd : CmdSpec := { name := "child", «meta» := mkMeta "child", args := [.opt childOpt] }
private def parentCmd : CmdSpec := { name := "app", «meta» := mkMeta "app", args := [], subs := [childCmd] }

#guard
  (let p := ArgParse.Spec.elaborateCommand parentCmd
   let st := normalize ["child", "--name=foo"]
   match p st with
   | .ok _ _ => True
   | _ => False)

-- Runner leftover detection surfaces `ErrorKind.leftover` when tokens remain.

private def expectTrue (cond : Bool) (msg : String) : Except String Unit :=
  if cond then
    .ok ()
  else
    .error msg

private def checkRunnerLeftoverPre : Except String Unit := do
  let app : AppSpec := { name := "app", root := cmd }
  let out := ArgParse.runRaw app ["--verbose", "--name=foo", "dangling"]
  match out.result with
  | .err err =>
      expectTrue (err.kind = ArgParse.ErrorKind.leftover)
        s!"expected leftover error, got {repr err.kind}"
      expectTrue (err.context = ["dangling"])
        s!"expected context [dangling], got {repr err.context}"
      expectTrue (out.state.pre = ["dangling"])
        s!"expected state.pre [dangling], got {repr out.state.pre}"
      expectTrue (out.state.post = [])
        s!"expected state.post [], got {repr out.state.post}"
  | other =>
      .error s!"expected leftover error, got {repr other}"

private def checkRunnerLeftoverPost : Except String Unit := do
  let app : AppSpec := { name := "app", root := cmd }
  let out := ArgParse.runRaw app ["--verbose", "--name=foo", "--", "tail"]
  match out.result with
  | .err err =>
      expectTrue (err.kind = ArgParse.ErrorKind.leftover)
        s!"expected leftover error, got {repr err.kind}"
      expectTrue (err.context = ["tail"])
        s!"expected context [tail], got {repr err.context}"
      expectTrue (out.state.pre = [])
        s!"expected empty pre, got {repr out.state.pre}"
      expectTrue (out.state.post = ["tail"])
        s!"expected post [tail], got {repr out.state.post}"
  | other =>
      .error s!"expected leftover error, got {repr other}"

private def checkRepeatedOne : Except String Unit := do
  let app : AppSpec := { name := "app", root := cmd }
  let out := ArgParse.runSummary app ["--name=alpha", "--name", "beta"]
  match out.result with
  | .ok summary =>
      let vals := Partial.Summary.optionValues summary "name"
      expectTrue (vals = ["alpha", "beta"] ∧ vals.getLast? = some "beta")
        s!"expected repeated .one accumulation, got {repr vals}"
  | .err err =>
      .error s!"expected ok summary, got error {repr err}"
  | other =>
      .error s!"expected ok summary, got {repr other}"

private def optMany : OptSpec String :=
  { long? := some "mode", «meta» := mkMeta "mode", arity := .many }
private def manyCmd : CmdSpec :=
  { name := "many", «meta» := mkMeta "many", args := [.opt optMany] }

private def checkManyOption : Except String Unit := do
  let app : AppSpec := { name := "many", root := manyCmd }
  let out := ArgParse.runSummary app ["--mode=alpha", "--mode", "beta", "--mode=gamma"]
  match out.result with
  | .ok summary =>
      let vals := Partial.Summary.optionValues summary "mode"
      expectTrue (vals = ["alpha", "beta", "gamma"]) s!"expected all values, got {repr vals}"
  | other =>
      .error s!"expected ok summary, got {repr other}"

private def optSome : OptSpec String :=
  { long? := some "tag", «meta» := mkMeta "tag", arity := .some }
private def someCmd : CmdSpec :=
  { name := "some", «meta» := mkMeta "some", args := [.opt optSome] }

private def checkSomeMissing : Except String Unit := do
  let app : AppSpec := { name := "some", root := someCmd }
  let out := ArgParse.runRaw app []
  match out.result with
  | .err err =>
      expectTrue (err.kind = ArgParse.ErrorKind.missingValue)
        s!"expected missing-value error, got {repr err.kind}"
  | other =>
      .error s!"expected missing-value error, got {repr other}"

private def checkSomeValues : Except String Unit := do
  let app : AppSpec := { name := "some", root := someCmd }
  let out := ArgParse.runSummary app ["--tag=alpha", "--tag=beta"]
  match out.result with
  | .ok summary =>
      let vals := Partial.Summary.optionValues summary "tag"
      expectTrue (vals = ["alpha", "beta"]) s!"expected alpha/beta, got {repr vals}"
  | other =>
      .error s!"expected ok summary, got {repr other}"

private def shortV : Short := { c := 'v', ok := by decide }
private def shortF : Short := { c := 'f', ok := by decide }
private def verboseFlag : FlagSpec := { short? := some shortV, «meta» := mkMeta "verbose" }
private def forceFlag : FlagSpec := { short? := some shortF, «meta» := mkMeta "force" }
private def bundleCmd : CmdSpec :=
  { name := "bundle", «meta» := mkMeta "bundle", args := [.flag verboseFlag, .flag forceFlag] }

private def checkBundledShortFlags : Except String Unit := do
  let app : AppSpec := { name := "bundle", root := bundleCmd }
  let out := ArgParse.runSummary app ["-vf"]
  match out.result with
  | .ok summary =>
      let verbose := Partial.Summary.flagValue? summary "verbose"
      let force := Partial.Summary.flagValue? summary "force"
      expectTrue (verbose = some true)
        s!"expected verbose flag, got {repr verbose}"
      expectTrue (force = some true)
        s!"expected force flag, got {repr force}"
  | other =>
      .error s!"expected ok summary, got {repr other}"

private def posFiles : PosSpec String := { «meta» := mkMeta "file", arity := .many }
private def sentinelCmd : CmdSpec :=
  { name := "sentinel"
  , «meta» := mkMeta "sentinel"
  , args := [.opt optShortLong, .pos posFiles] }

private def checkSentinelBoundary : Except String Unit := do
  let app : AppSpec := { name := "sentinel", root := sentinelCmd }
  let out := ArgParse.runSummary app ["--name=foo", "--", "--name=bar", "file.txt"]
  match out.result with
  | .ok summary =>
      let optVals := Partial.Summary.optionValues summary "name"
      let posVals := Partial.Summary.positionalValues summary "file"
      expectTrue (optVals = ["foo"]) s!"expected option foo, got {repr optVals}"
      expectTrue (posVals = ["--name=bar", "file.txt"])
        s!"expected positional values, got {repr posVals}"
  | other =>
      .error s!"expected ok summary, got {repr other}"

private def countSpec : OptSpec Nat :=
  { long? := some "count", «meta» := mkMeta "count", arity := .one }
private def countCmd : CmdSpec :=
  { name := "counting", «meta» := mkMeta "counting", args := [.opt countSpec] }

private def checkMissingOptionValue : Except String Unit := do
  let app : AppSpec := { name := "counting", root := countCmd }
  let out := ArgParse.runRaw app ["--count"]
  match out.result with
  | .err err =>
      expectTrue (err.kind = ArgParse.ErrorKind.missingValue)
        s!"expected missing-value error, got {repr err.kind}"
  | other =>
      .error s!"expected missing-value error, got {repr other}"

private def checkInvalidOptionPayload : Except String Unit := do
  let app : AppSpec := { name := "counting", root := countCmd }
  let out := ArgParse.runRaw app ["--count=oops"]
  match out.result with
  | .err err =>
      expectTrue (err.kind = ArgParse.ErrorKind.custom)
        s!"expected custom error, got {repr err.kind}"
  | other =>
      .error s!"expected custom error, got {repr other}"

private def rootVerbose : FlagSpec := { long? := some "root-verbose", «meta» := mkMeta "root-verbose" }
private def rootMode : OptSpec String := { long? := some "root-mode", «meta» := mkMeta "root-mode", arity := .one }
private def childDebug : FlagSpec := { long? := some "child-debug", «meta» := mkMeta "child-debug" }
private def childMode : OptSpec String := { long? := some "child-mode", «meta» := mkMeta "child-mode", arity := .one }
private def childDeep : CmdSpec :=
  { name := "child"
  , «meta» := mkMeta "child"
  , args := [.flag childDebug, .opt childMode] }
private def interleavedCmd : CmdSpec :=
  { name := "root"
  , «meta» := mkMeta "root"
  , args := [.flag rootVerbose, .opt rootMode]
  , subs := [childDeep] }

private def checkInterleavedSuccess : Except String Unit := do
  let app : AppSpec := { name := "interleaved", root := interleavedCmd }
  let out := ArgParse.runSummary app
    [ "--root-verbose"
    , "--root-mode=alpha"
    , "child"
    , "--child-debug"
    , "--child-mode=beta"
    , "--child-mode", "gamma" ]
  match out.result with
  | .ok summary =>
      let rootFlag := Partial.Summary.flagValue? summary "root-verbose"
      let rootVals := Partial.Summary.optionValues summary "root-mode"
      let childFlag := Partial.Summary.flagValue? summary "child-debug"
      let childVals := Partial.Summary.optionValues summary "child-mode"
      expectTrue (rootFlag = some true) s!"expected root flag, got {repr rootFlag}"
      expectTrue (rootVals = ["alpha"]) s!"expected root mode alpha, got {repr rootVals}"
      expectTrue (childFlag = some true) s!"expected child flag, got {repr childFlag}"
      expectTrue (childVals = ["beta", "gamma"]) s!"expected child values, got {repr childVals}"
      expectTrue (childVals.getLast? = some "gamma") s!"expected last gamma, got {repr (childVals.getLast?)}"
  | other =>
      .error s!"expected ok summary, got {repr other}"

private def checkInterleavedMissing : Except String Unit := do
  let app : AppSpec := { name := "interleaved", root := interleavedCmd }
  let out := ArgParse.runRaw app ["child", "--child-mode"]
  match out.result with
  | .err err =>
      expectTrue (err.kind = ArgParse.ErrorKind.missingValue)
        s!"expected missing-value error, got {repr err.kind}"
      expectTrue (err.context = ["--child-mode"]) s!"expected context [--child-mode], got {repr err.context}"
  | other =>
      .error s!"expected missing-value error, got {repr other}"

private def treeRootFlag : FlagSpec := { long? := some "tree-root-verbose", «meta» := mkMeta "tree-root-verbose" }
private def treeRootMode : OptSpec String :=
  { long? := some "tree-root-mode", «meta» := mkMeta "tree-root-mode", arity := .one }
private def childTreeFlag : FlagSpec := { long? := some "child-tree-debug", «meta» := mkMeta "child-tree-debug" }
private def childTreeMode : OptSpec String :=
  { long? := some "child-tree-mode", «meta» := mkMeta "child-tree-mode", arity := .one }
private def grandTreeMode : OptSpec String :=
  { long? := some "grand-tree-mode", «meta» := mkMeta "grand-tree-mode", arity := .many }
private def siblingTreeFlag : FlagSpec := { long? := some "sibling-tree-flag", «meta» := mkMeta "sibling-tree-flag" }
private def grandTreeCmd : CmdSpec :=
  { name := "grand"
  , «meta» := mkMeta "grand"
  , args := [.opt grandTreeMode] }
private def siblingTreeCmd : CmdSpec :=
  { name := "sibling"
  , «meta» := mkMeta "sibling"
  , args := [.flag siblingTreeFlag] }
private def childTreeCmd : CmdSpec :=
  { name := "child"
  , «meta» := mkMeta "child-tree"
  , args := [.flag childTreeFlag, .opt childTreeMode]
  , subs := [grandTreeCmd, siblingTreeCmd] }
private def peerTreeFlag : FlagSpec := { long? := some "peer-flag", «meta» := mkMeta "peer-flag" }
private def peerTreeCmd : CmdSpec :=
  { name := "peer"
  , «meta» := mkMeta "peer"
  , args := [.flag peerTreeFlag] }
private def treeRootCmd : CmdSpec :=
  { name := "tree"
  , «meta» := mkMeta "tree"
  , args := [.flag treeRootFlag, .opt treeRootMode]
  , subs := [childTreeCmd, peerTreeCmd] }

private def checkNestedSubcommandSuccess : Except String Unit := do
  let app : AppSpec := { name := "tree-app", root := treeRootCmd }
  let out := ArgParse.runSummary app
    [ "--tree-root-verbose"
    , "--tree-root-mode=cli"
    , "child"
    , "--child-tree-debug"
    , "--child-tree-mode=first"
    , "--child-tree-mode", "second"
    , "grand"
    , "--grand-tree-mode=one"
    , "--grand-tree-mode", "two" ]
  match out.result with
  | .ok summary =>
      let rootFlag := Partial.Summary.flagValue? summary "tree-root-verbose"
      let rootModeVals := Partial.Summary.optionValues summary "tree-root-mode"
      let childFlag := Partial.Summary.flagValue? summary "child-tree-debug"
      let childModeVals := Partial.Summary.optionValues summary "child-tree-mode"
      let grandModeVals := Partial.Summary.optionValues summary "grand-tree-mode"
      expectTrue (rootFlag = some true) s!"expected root verbose flag, got {repr rootFlag}"
      expectTrue (rootModeVals = ["cli"]) s!"expected root mode cli, got {repr rootModeVals}"
      expectTrue (childFlag = some true) s!"expected child debug flag, got {repr childFlag}"
      expectTrue (childModeVals = ["first", "second"]) s!"expected child modes, got {repr childModeVals}"
      expectTrue (childModeVals.getLast? = some "second")
        s!"expected child last value second, got {repr (childModeVals.getLast?)}"
      expectTrue (grandModeVals = ["one", "two"]) s!"expected grand modes, got {repr grandModeVals}"
  | other =>
      .error s!"expected ok summary, got {repr other}"

private def checkNestedGrandMissingValue : Except String Unit := do
  let app : AppSpec := { name := "tree-app", root := treeRootCmd }
  let out := ArgParse.runRaw app ["child", "grand", "--grand-tree-mode"]
  match out.result with
  | .err err =>
      expectTrue (err.kind = ArgParse.ErrorKind.missingValue)
        s!"expected missing-value error, got {repr err.kind}"
      expectTrue (err.context = ["--grand-tree-mode"])
        s!"expected context [--grand-tree-mode], got {repr err.context}"
  | other =>
      .error s!"expected missing-value error, got {repr other}"

private def checkNestedUnknownSubcommand : Except String Unit := do
  let app : AppSpec := { name := "tree-app", root := treeRootCmd }
  let out := ArgParse.runRaw app ["child", "bogus"]
  match out.result with
  | .err err =>
      expectTrue (err.kind = ArgParse.ErrorKind.leftover)
        s!"expected leftover error, got {repr err.kind}"
      expectTrue (err.context = ["bogus"])
        s!"expected context [bogus], got {repr err.context}"
  | other =>
      .error s!"expected leftover error, got {repr other}"

/-- Runtime regression checks executed by `lake test`. -/
def runtimeChecks : List (String × Except String Unit) :=
  [ ("runner leftover (pre)", checkRunnerLeftoverPre)
  , ("runner leftover (post)", checkRunnerLeftoverPost)
  , ("repeated .one option", checkRepeatedOne)
  , ("many option accumulation", checkManyOption)
  , ("some missing value", checkSomeMissing)
  , ("some value accumulation", checkSomeValues)
  , ("bundled short flags", checkBundledShortFlags)
  , ("sentinel boundary", checkSentinelBoundary)
  , ("missing option value", checkMissingOptionValue)
  , ("invalid option payload", checkInvalidOptionPayload)
  , ("interleaved subcommand success", checkInterleavedSuccess)
  , ("interleaved subcommand missing value", checkInterleavedMissing)
  , ("nested subcommand success", checkNestedSubcommandSuccess)
  , ("nested grandchild missing value", checkNestedGrandMissingValue)
  , ("nested unknown subcommand", checkNestedUnknownSubcommand)
  ]
end ArgParse.Tests

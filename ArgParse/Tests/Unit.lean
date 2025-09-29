import ArgParse.Core.Normalize
import ArgParse.Core.Value
import ArgParse.Spec.AST
import ArgParse.Spec.Elab
import ArgParse.Core.Runner
import ArgParse.Proofs.Sentinel

namespace ArgParse.Tests

open ArgParse Core ArgParse.Proofs

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
def mkMeta (n : String) : ArgParse.Spec.Meta := { name := n }
def flagSpec : ArgParse.Spec.FlagSpec := { long? := some "verbose", «meta» := mkMeta "verbose" }
def optSpec : ArgParse.Spec.OptSpec String := { long? := some "name", «meta» := mkMeta "name", arity := .one }
def cmd : ArgParse.Spec.CmdSpec := { name := "app", «meta» := mkMeta "app", args := [.flag flagSpec, .opt optSpec] }

#guard
  (let p := ArgParse.Spec.elaborateCommand cmd
   let st := normalize ["--verbose", "--name=foo"]
   match p st with
   | .ok part _ => part.flags.any (fun (k,v) => k = "verbose" ∧ v = true) ∧
                   part.options.any (fun (k,_) => k = "name")
    | _ => False)

-- Option forms: equals and concatenated short are both accepted.
open ArgParse.Spec in
def shortN : Short := { c := 'n', ok := by decide }
def optShortLong : OptSpec String := { short? := some shortN, long? := some "name", «meta» := mkMeta "name", arity := .one }
def cmdOpt : CmdSpec := { name := "app", «meta» := mkMeta "app", args := [.opt optShortLong] }

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
def childOpt : OptSpec String := { long? := some "name", «meta» := mkMeta "name", arity := .one }
def childCmd : CmdSpec := { name := "child", «meta» := mkMeta "child", args := [.opt childOpt] }
def parentCmd : CmdSpec := { name := "app", «meta» := mkMeta "app", args := [], subs := [childCmd] }

#guard
  (let p := ArgParse.Spec.elaborateCommand parentCmd
   let st := normalize ["child", "--name=foo"]
   match p st with
   | .ok _ _ => True
   | _ => False)

-- Runner leftover detection surfaces `ErrorKind.leftover` when tokens remain.
#guard
  (let app : ArgParse.Spec.AppSpec := { name := "app", root := cmd }
   let out := ArgParse.runRaw app ["--verbose", "--name=foo", "dangling"]
   match out.result with
   | .err err =>
       err.kind = ArgParse.ErrorKind.leftover ∧
       err.context = ["dangling"] ∧
       out.state.pre = ["dangling"] ∧
       out.state.post = []
   | _ => False)

-- Leftover tokens in the post-sentinel stream also trigger detection.
#guard
  (let app : ArgParse.Spec.AppSpec := { name := "app", root := cmd }
   let out := ArgParse.runRaw app ["--verbose", "--name=foo", "--", "tail"]
   match out.result with
   | .err err =>
       err.kind = ArgParse.ErrorKind.leftover ∧
       err.context = ["tail"] ∧
       out.state.pre = [] ∧
       out.state.post = ["tail"]
   | _ => False)

-- Repeated `.one` option invocations append values left-to-right (last wins).
#guard
  (let app : ArgParse.Spec.AppSpec := { name := "app", root := cmd }
   let out := ArgParse.runSummary app ["--name=alpha", "--name", "beta"]
   match out.result with
   | .ok summary =>
       let vals := Partial.Summary.optionValues summary "name"
       vals = ["alpha", "beta"] ∧ vals.getLast? = some "beta"
   | _ => False)

def optMany : OptSpec String :=
  { long? := some "mode", «meta» := mkMeta "mode", arity := .many }
def manyCmd : CmdSpec :=
  { name := "many", «meta» := mkMeta "many", args := [.opt optMany] }

-- `.many` options collect all values in encounter order.
#guard
  (let app : ArgParse.Spec.AppSpec := { name := "many", root := manyCmd }
   let out := ArgParse.runSummary app ["--mode=alpha", "--mode", "beta", "--mode=gamma"]
   match out.result with
   | .ok summary =>
       Partial.Summary.optionValues summary "mode" = ["alpha", "beta", "gamma"]
   | _ => False)

def optSome : OptSpec String :=
  { long? := some "tag", «meta» := mkMeta "tag", arity := .some }
def someCmd : CmdSpec :=
  { name := "some", «meta» := mkMeta "some", args := [.opt optSome] }

-- `.some` options require at least one value.
#guard
  (let app : ArgParse.Spec.AppSpec := { name := "some", root := someCmd }
   let out := ArgParse.runRaw app []
   match out.result with
   | .err err => err.kind = ArgParse.ErrorKind.missingValue
   | _ => False)

-- Providing values for `.some` options succeeds and accumulates raw strings.
#guard
  (let app : ArgParse.Spec.AppSpec := { name := "some", root := someCmd }
   let out := ArgParse.runSummary app ["--tag=alpha", "--tag=beta"]
   match out.result with
   | .ok summary => Partial.Summary.optionValues summary "tag" = ["alpha", "beta"]
   | _ => False)

open ArgParse.Spec in
def shortV : Short := { c := 'v', ok := by decide }
def shortF : Short := { c := 'f', ok := by decide }
def verboseFlag : FlagSpec := { short? := some shortV, «meta» := mkMeta "verbose" }
def forceFlag : FlagSpec := { short? := some shortF, «meta» := mkMeta "force" }
def bundleCmd : CmdSpec :=
  { name := "bundle", «meta» := mkMeta "bundle", args := [.flag verboseFlag, .flag forceFlag] }

-- Bundled short flags (`-vf`) are expanded left-to-right.
#guard
  (let app : ArgParse.Spec.AppSpec := { name := "bundle", root := bundleCmd }
   let out := ArgParse.runSummary app ["-vf"]
   match out.result with
   | .ok summary =>
       Partial.Summary.flagValue? summary "verbose" = some true ∧
       Partial.Summary.flagValue? summary "force" = some true
   | _ => False)

def posFiles : PosSpec String := { «meta» := mkMeta "file", arity := .many }
def sentinelCmd : CmdSpec :=
  { name := "sentinel"
  , «meta» := mkMeta "sentinel"
  , args := [.opt optShortLong, .pos posFiles] }

-- Sentinel boundary keeps option parsing to the pre-stream and positionals post.
#guard
  (let app : ArgParse.Spec.AppSpec := { name := "sentinel", root := sentinelCmd }
   let out := ArgParse.runSummary app ["--name=foo", "--", "--name=bar", "file.txt"]
   match out.result with
   | .ok summary =>
       let optVals := Partial.Summary.optionValues summary "name"
       let posVals := Partial.Summary.positionalValues summary "file"
       optVals = ["foo"] ∧ posVals = ["--name=bar", "file.txt"]
   | _ => False)

def countSpec : OptSpec Nat :=
  { long? := some "count", «meta» := mkMeta "count", arity := .one }
def countCmd : CmdSpec :=
  { name := "counting", «meta» := mkMeta "counting", args := [.opt countSpec] }

-- Missing option values yield `ErrorKind.missingValue`.
#guard
  (let app : ArgParse.Spec.AppSpec := { name := "counting", root := countCmd }
   let out := ArgParse.runRaw app ["--count"]
   match out.result with
   | .err err => err.kind = ArgParse.ErrorKind.missingValue
   | _ => False)

-- Invalid option payloads surface `ErrorKind.custom` from `FromArg.run` failures.
#guard
  (let app : ArgParse.Spec.AppSpec := { name := "counting", root := countCmd }
   let out := ArgParse.runRaw app ["--count=oops"]
   match out.result with
   | .err err => err.kind = ArgParse.ErrorKind.custom
   | _ => False)

def rootVerbose : FlagSpec := { long? := some "root-verbose", «meta» := mkMeta "root-verbose" }
def rootMode : OptSpec String := { long? := some "root-mode", «meta» := mkMeta "root-mode", arity := .one }
def childDebug : FlagSpec := { long? := some "child-debug", «meta» := mkMeta "child-debug" }
def childMode : OptSpec String := { long? := some "child-mode", «meta» := mkMeta "child-mode", arity := .one }
def childDeep : CmdSpec :=
  { name := "child"
  , «meta» := mkMeta "child"
  , args := [.flag childDebug, .opt childMode] }
def interleavedCmd : CmdSpec :=
  { name := "root"
  , «meta» := mkMeta "root"
  , args := [.flag rootVerbose, .opt rootMode]
  , subs := [childDeep] }

-- Subcommand options honour last-value-wins while root items persist.
#guard
  (let app : ArgParse.Spec.AppSpec := { name := "interleaved", root := interleavedCmd }
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
       rootFlag = some true ∧ rootVals = ["alpha"] ∧
       childFlag = some true ∧ childVals = ["beta", "gamma"] ∧
       childVals.getLast? = some "gamma"
   | _ => False)

-- Missing child option payload bubbles the structured error through the runner.
#guard
  (let app : ArgParse.Spec.AppSpec := { name := "interleaved", root := interleavedCmd }
   let out := ArgParse.runRaw app ["child", "--child-mode"]
   match out.result with
   | .err err => err.kind = ArgParse.ErrorKind.missingValue ∧ err.context = ["--child-mode"]
   | _ => False)

end ArgParse.Tests

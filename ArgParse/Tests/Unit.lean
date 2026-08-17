import ArgParse.Core.Normalize
import ArgParse.Core.Value
import ArgParse.Core.Combinators
import ArgParse.Core.Scan
import ArgParse.Spec.AST
import ArgParse.Proofs.Sentinel

/-!
# ArgParse.Tests.Unit

Layer-1 regression checks, stated directly over the `Core` combinators.

Until Layers 2-5 land these are the whole suite: the previous runtime tests
were written against `Spec.elaborateCommand` and the `Partial` runner, and went
out with them. Their behaviours -- leftover detection, repeated options,
interleaved and nested subcommands, order-insensitive scanning -- are restored
over `Cmd`/`P` when the runner returns.
-/

namespace ArgParse.Tests

open ArgParse Core ArgParse.Proofs

open ArgParse.Spec

/-! ### Value decoding -/

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

/-! ### Normalization and the sentinel -/

#guard (normalize ["-n", "5", "--", "file"]).pre = ["-n", "5"]
#guard (normalize ["-n", "5", "--", "file"]).post = ["file"]
#guard (normalize ["alpha", "beta"]).post = []
#guard
  (let tokens := ["-v", "--", "tail"]
   let st := normalize tokens
   tokens = st.pre ++ "--" :: st.post)

/-! ### Shared fixtures -/

/-- Helper to construct minimal metadata for test specs. -/
private def mkMeta (n : String) : Meta := { name := n }

private def shortN : Short := { c := 'n', ok := by decide }
private def shortV : Short := { c := 'v', ok := by decide }
private def shortF : Short := { c := 'f', ok := by decide }

private def verboseFlag : FlagSpec :=
  { short? := some shortV, long? := some "verbose", «meta» := mkMeta "verbose" }
private def forceFlag : FlagSpec :=
  { short? := some shortF, long? := some "force", «meta» := mkMeta "force" }
private abbrev nameOpt : OptSpec String :=
  { short? := some shortN, long? := some "name", «meta» := mkMeta "name", arity := .one }
private abbrev shortOptOnly : OptSpec String :=
  { short? := some shortN, «meta» := mkMeta "name", arity := .one }
private abbrev modeOpt : OptSpec String :=
  { long? := some "mode", «meta» := mkMeta "mode", arity := .many }
private abbrev tagOpt : OptSpec String :=
  { long? := some "tag", «meta» := mkMeta "tag", arity := .some }
private abbrev countOpt : OptSpec Nat :=
  { long? := some "count", «meta» := mkMeta "count", arity := .one }
private abbrev filePos : PosSpec String :=
  { «meta» := mkMeta "file", arity := .one }

/-! ### Option token forms -/

#guard
  (let st := normalize ["--name=foo"]
   match takeOptionStep? nameOpt st with
   | .ok step => step.consumed = 1 ∧ step.value? = some "foo"
   | _ => False)

#guard
  (let st := normalize ["-n", "foo"]
   match takeOptionStep? shortOptOnly st with
   | .ok step =>
       step.consumed = 2 ∧ step.state.cursor = st.cursor + 2 ∧
       step.value? = some "foo" ∧ step.raw? = some "foo"
   | _ => False)

#guard
  (let st := normalize ["-nfoo"]
   match takeOptionStep? nameOpt st with
   | .ok step => step.value? = some "foo"
   | _ => False)

#guard
  (let st := normalize ["--other"]
   match takeOptionStep? nameOpt st with
   | .ok step =>
       step.consumed = 0 ∧ step.state.cursor = st.cursor ∧
       step.value? = none ∧ step.raw? = none
   | _ => False)

/-! ### Runtime checks -/

private def expectTrue (cond : Bool) (msg : String) : Except String Unit :=
  if cond then
    .ok ()
  else
    .error msg

/-- `.one` options keep the last value supplied. -/
private def checkRepeatedOne : Except String Unit := do
  let st := normalize ["--name=alpha", "--name", "beta"]
  match Core.optionScan nameOpt st with
  | .ok value _ =>
      expectTrue (value = some "beta") s!"expected last value to win, got {repr value}"
  | other => .error s!"expected ok result, got {repr other}"

/-- `.many` options accumulate every value in order. -/
private def checkManyOption : Except String Unit := do
  let st := normalize ["--mode=alpha", "--mode", "beta", "--mode=gamma"]
  match Core.optionScan modeOpt st with
  | .ok values _ =>
      expectTrue (values = ["alpha", "beta", "gamma"])
        s!"expected three values in order, got {repr values}"
  | other => .error s!"expected ok result, got {repr other}"

/-- `.some` options fail when no value is supplied. -/
private def checkSomeMissing : Except String Unit := do
  match Core.optionScan tagOpt (normalize []) with
  | .err err =>
      expectTrue (err.kind = ArgParse.ErrorKind.missingValue)
        s!"expected missingValue, got {repr err.kind}"
  | other => .error s!"expected missingValue, got {repr other}"

/-- `.some` options accumulate like `.many` once a value is present. -/
private def checkSomeValues : Except String Unit := do
  let st := normalize ["--tag=alpha", "--tag=beta"]
  match Core.optionScan tagOpt st with
  | .ok values _ =>
      expectTrue (values = ["alpha", "beta"]) s!"expected both tags, got {repr values}"
  | other => .error s!"expected ok result, got {repr other}"

/-- Bundled short flags (`-vf`) satisfy both flags. -/
private def checkBundledShortFlags : Except String Unit := do
  let st := normalize ["-vf"]
  match Core.flagScan verboseFlag st with
  | .ok verbose st' =>
      expectTrue verbose "expected -v to be recognised in the bundle"
      match Core.flagScan forceFlag st' with
      | .ok force _ => expectTrue force "expected -f to survive in the rewritten bundle"
      | other => .error s!"expected ok result for -f, got {repr other}"
  | other => .error s!"expected ok result for -v, got {repr other}"

/-- Scanning never crosses the `--` sentinel. -/
private def checkSentinelBoundary : Except String Unit := do
  let st := normalize ["--name=foo", "--", "--name=bar", "file.txt"]
  match Core.optionScan nameOpt st with
  | .ok value st' =>
      expectTrue (value = some "foo") s!"expected only the pre-sentinel value, got {repr value}"
      expectTrue (st'.post = ["--name=bar", "file.txt"])
        s!"expected post-sentinel tokens untouched, got {repr st'.post}"
  | other => .error s!"expected ok result, got {repr other}"

/-- A trailing option name with no value is a `missingValue` error. -/
private def checkMissingOptionValue : Except String Unit := do
  match Core.optionScan countOpt (normalize ["--count"]) with
  | .err err =>
      expectTrue (err.kind = ArgParse.ErrorKind.missingValue)
        s!"expected missingValue, got {repr err.kind}"
  | other => .error s!"expected missingValue, got {repr other}"

/-- A value the `FromArg` instance rejects surfaces as a `custom` error. -/
private def checkInvalidOptionPayload : Except String Unit := do
  match Core.optionScan countOpt (normalize ["--count=oops"]) with
  | .err err =>
      expectTrue (err.kind = ArgParse.ErrorKind.custom)
        s!"expected custom error, got {repr err.kind}"
  | other => .error s!"expected custom error, got {repr other}"

/-- Positionals consume the tokens scanning left behind, sentinel included. -/
private def checkPositionalAfterScan : Except String Unit := do
  let st := normalize ["--name=foo", "target.txt"]
  match Core.optionScan nameOpt st with
  | .ok _ st' =>
      match Core.positional filePos st' with
      | .ok value _ =>
          expectTrue (value = some "target.txt")
            s!"expected the leftover token as the positional, got {repr value}"
      | other => .error s!"expected ok positional, got {repr other}"
  | other => .error s!"expected ok option result, got {repr other}"

/-- Order insensitivity at the combinator level: the same tokens shuffled parse
to the same values. -/
private def checkScanOrderInsensitive : Except String Unit := do
  let read (argv : List String) : Option (Bool × Option String × Option String) :=
    let st := normalize argv
    match Core.flagScan verboseFlag st with
    | .ok verbose st₁ =>
        match Core.optionScan nameOpt st₁ with
        | .ok name st₂ =>
            match Core.positional filePos st₂ with
            | .ok file _ => some (verbose, name, file)
            | _ => none
        | _ => none
    | _ => none
  let a := read ["-v", "--name", "foo", "target.txt"]
  let b := read ["target.txt", "--name=foo", "-v"]
  let c := read ["--name", "foo", "target.txt", "-v"]
  expectTrue (a == some (true, some "foo", some "target.txt"))
    s!"unexpected baseline parse: {repr a}"
  expectTrue (a == b && b == c) s!"shuffled argv disagreed: {repr a} vs {repr b} vs {repr c}"

/-- The subcommand combinator dispatches, advances, and reports its expectations. -/
private def checkCoreSubcommand : Except String Unit := do
  let entries : List (ArgParse.Core.Subcommand Nat) :=
    [ { name := "one", parser := Parser.pure 1 }
    , { name := "two", parser := Parser.pure 2 } ]
  let parser := ArgParse.Core.subcommand entries
  let stOk := normalize ["two", "extra"]
  match parser stOk with
  | .ok value st' =>
      expectTrue (value = 2) s!"expected value 2, got {value}"
      expectTrue (st'.cursor = stOk.cursor + 1)
        s!"expected cursor advance, got {st'.cursor}"
      expectTrue (st'.pre = ["extra"]) s!"expected remaining token, got {repr st'.pre}"
  | other =>
      .error s!"expected ok result, got {repr other}"
  let stUnknown := normalize ["three"]
  match parser stUnknown with
  | .err err =>
      expectTrue (err.kind = ArgParse.ErrorKind.unknownLong)
        s!"expected unknownLong, got {repr err.kind}"
      expectTrue (err.expect = [ArgParse.Expect.subcommand "one", ArgParse.Expect.subcommand "two"])
        s!"expected command hints, got {repr err.expect}"
  | other =>
      .error s!"expected unknownLong, got {repr other}"
  let stMissing := normalize ([] : List String)
  match parser stMissing with
  | .err err =>
      expectTrue (err.kind = ArgParse.ErrorKind.missingValue)
        s!"expected missingValue, got {repr err.kind}"
  | other =>
      .error s!"expected missingValue, got {repr other}"

/-- `scopedPre` keeps a parent's scanning items out of a child's segment. -/
private def checkScopedPre : Except String Unit := do
  let st := normalize ["--name=parent", "child", "--name=kid"]
  match Core.scopedPre ["child"] (Core.optionScan nameOpt) st with
  | .ok value st' =>
      expectTrue (value = some "parent")
        s!"expected the parent's value only, got {repr value}"
      expectTrue (st'.pre = ["child", "--name=kid"])
        s!"expected the child segment intact, got {repr st'.pre}"
  | other => .error s!"expected ok result, got {repr other}"

/-- Runtime regression checks executed by `lake test`. -/
def runtimeChecks : List (String × Except String Unit) :=
  [ ("repeated .one option", checkRepeatedOne)
  , ("many option accumulation", checkManyOption)
  , ("some missing value", checkSomeMissing)
  , ("some value accumulation", checkSomeValues)
  , ("bundled short flags", checkBundledShortFlags)
  , ("sentinel boundary", checkSentinelBoundary)
  , ("missing option value", checkMissingOptionValue)
  , ("invalid option payload", checkInvalidOptionPayload)
  , ("positional after scan", checkPositionalAfterScan)
  , ("scan order insensitivity", checkScanOrderInsensitive)
  , ("core subcommand combinator", checkCoreSubcommand)
  , ("scoped pre segmentation", checkScopedPre)
  ]

end ArgParse.Tests

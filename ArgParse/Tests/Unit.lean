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

-- Runner built-ins: `--help` routes to help text without parsing.
open ArgParse in
#guard
  (let app : ArgParse.Spec.AppSpec := { name := "app", root := cmd }
   let out := ArgParse.runRaw app ["--help", "--name", "x"]
   match out.result with
   | .help _ => True
   | _ => False)

-- Subcommand selection placeholder: elaborator currently consumes a matching
-- subcommand token without recursing; full recursion will be tested later.

end ArgParse.Tests

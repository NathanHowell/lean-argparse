import ArgParse.Core.Normalize
import ArgParse.Core.Value
import ArgParse.Spec.AST
import ArgParse.Spec.Elab
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

-- Subcommand selection placeholder: elaborator currently consumes a matching
-- subcommand token without recursing; full recursion will be tested later.

end ArgParse.Tests

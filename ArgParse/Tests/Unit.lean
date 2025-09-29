import ArgParse.Core.Normalize
import ArgParse.Core.Value
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

end ArgParse.Tests

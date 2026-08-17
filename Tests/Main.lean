import ArgParse.Tests.Unit

/-!
Test driver.

The nested-subcommand and order-insensitivity integration tests that lived here
were written against the `Partial` runner. They are restored over `Cmd`/`P`
once Layer 5 lands; `ArgParse.Tests.Unit` carries the combinator-level coverage
in the meantime.
-/

private def runCheck (label : String) (check : Except String Unit) : IO Bool :=
  match check with
  | .ok _ => pure true
  | .error msg =>
      IO.eprintln s!"[FAIL] {label}: {msg}" *> pure false

/-- Run every unit check, reporting failures on stderr. -/
def main : IO UInt32 := do
  let results ← ArgParse.Tests.runtimeChecks.mapM (fun (label, chk) => runCheck label chk)
  pure <| if results.all id then 0 else 1

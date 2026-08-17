import ArgParse.Tests.Unit
import ArgParse.Tests.Exec

/-!
Test driver: combinator-level checks from `Tests.Unit`, integration checks over
`Cmd`/`P`/`Exec` from `Tests.Exec`.
-/

private def runCheck (label : String) (check : Except String Unit) : IO Bool :=
  match check with
  | .ok _ => pure true
  | .error msg =>
      IO.eprintln s!"[FAIL] {label}: {msg}" *> pure false

/-- Run every unit check, reporting failures on stderr. -/
def main : IO UInt32 := do
  let checks := ArgParse.Tests.runtimeChecks ++ ArgParse.Tests.execChecks
  let results ← checks.mapM (fun (label, chk) => runCheck label chk)
  pure <| if results.all id then 0 else 1

import Lean
import Std

open System Lean

namespace LintDriver

private def defaultTargets : List FilePath :=
  ["Argparse.lean", "Main.lean", "Tests/Main.lean"].map FilePath.mk

private def lintFile (root : FilePath) (path : FilePath) : IO UInt32 := do
  IO.println s!"Linting {path}"
  let contents ← IO.FS.readFile path
  let moduleName ←
    try
      Lean.moduleNameOfFileName path (some root)
    catch e =>
      IO.eprintln s!"Failed to infer module name for {path}: {e}"
      return (1 : UInt32)
  let opts := (Options.empty).setBool `linter.all true
  match ← Lean.Elab.runFrontend contents opts path.toString moduleName with
  | some _ =>
      return (0 : UInt32)
  | none =>
      IO.eprintln s!"Linting failed for {path}"
      return (1 : UInt32)

end LintDriver

open LintDriver

def main (args : List String) : IO UInt32 := do
  let sysroot ← Lean.findSysroot
  Lean.initSearchPath sysroot
  let root ← IO.currentDir
  let targets := if args.isEmpty then defaultTargets else args.map FilePath.mk
  let mut exit : UInt32 := (0 : UInt32)
  for file in targets do
    let path := root / file
    if !(← path.pathExists) then
      IO.eprintln s!"Skipping missing file {path}"
      continue
    let code ← lintFile root path
    if code ≠ (0 : UInt32) then
      exit := code
  return exit

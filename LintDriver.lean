import Lean
import Std

open System Lean Std

namespace LintDriver

private def defaultEntries : List FilePath :=
  ["Argparse.lean", "Argparse", "Main.lean", "Tests", "LintDriver.lean"].map FilePath.mk

private def resolvePath (root entry : FilePath) : FilePath :=
  if entry.isAbsolute then entry else root / entry

private partial def collectLeanFiles (acc : Std.HashSet FilePath) (path : FilePath) : IO (Std.HashSet FilePath) := do
  if !(← path.pathExists) then
    IO.eprintln s!"Skipping missing entry {path}"
    return acc
  if ← path.isDir then
    let mut acc := acc
    for dirEntry in (← path.readDir) do
      acc ← collectLeanFiles acc dirEntry.path
    return acc
  else if path.extension == some "lean" then
    return acc.insert path
  else
    return acc

private def expandEntries (root : FilePath) (entries : List FilePath) : IO (List FilePath) := do
  let mut acc : HashSet FilePath := {}
  for entry in entries do
    acc ← collectLeanFiles acc (resolvePath root entry)
  return acc.toList

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

/-- Entry point for the lint driver executable used by `lake lint`. -/
def main (args : List String) : IO UInt32 := do
  let sysroot ← Lean.findSysroot
  Lean.initSearchPath sysroot
  let root ← IO.currentDir
  let entries := if args.isEmpty then defaultEntries else args.map FilePath.mk
  let targets ← expandEntries root entries
  let mut exit : UInt32 := (0 : UInt32)
  for path in targets do
    let code ← lintFile root path
    if code ≠ (0 : UInt32) then
      exit := code
  return exit

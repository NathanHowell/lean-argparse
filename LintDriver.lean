import Lean
import Std

open System Lean Std

namespace LintDriver

private def defaultEntries : List FilePath :=
  ["ArgParse.lean", "ArgParse", "Main.lean", "Tests", "LintDriver.lean"].map FilePath.mk

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

private unsafe def lintFile (root : FilePath) (path : FilePath) : IO UInt32 := do
  IO.println s!"Linting {path}"
  -- Per file: the frontend clears the flag once it finishes importing, so every
  -- `runFrontend` needs it set again.
  Lean.enableInitializersExecution
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

/-- Lint each file in a fresh child process, returning the worst exit code.

`runFrontend` builds a whole `Environment` per file and nothing releases it, so
linting every file in one process makes memory grow monotonically -- measured at
19 GB peak across this project's 31 files. That fits on a development machine
and does not fit on a CI runner, where it is killed partway with SIGTERM and
presents as a cancelled job rather than as an out-of-memory error.

One file per process bounds the peak to whatever the largest single file needs.
It costs process startup per file, which is small next to elaboration. -/
private def lintInChildren (targets : List FilePath) : IO UInt32 := do
  let self ← IO.appPath
  let mut exit : UInt32 := (0 : UInt32)
  for path in targets do
    let child ← IO.Process.output { cmd := self.toString, args := #[path.toString] }
    IO.print child.stdout
    IO.eprint child.stderr
    if child.exitCode ≠ (0 : UInt32) then
      exit := child.exitCode
  return exit

end LintDriver

open LintDriver

/-- Entry point for the lint driver executable used by `lake lint`. `unsafe`
because enabling module initializers is.

With no arguments this enumerates the project and lints each file in a child
process. With arguments it lints them in this process, which is both the
single-file worker the parent spawns and the way to lint an explicit subset by
hand. -/
unsafe def main (args : List String) : IO UInt32 := do
  let sysroot ← Lean.findSysroot
  Lean.initSearchPath sysroot
  let root ← IO.currentDir
  -- Ensure the project root is on the module search path so imports like
  -- `import ArgParse.Core.Value` resolve to source files during linting.
  let current ← Lean.searchPathRef.get
  Lean.searchPathRef.set (current ++ [root])
  if args.isEmpty then
    let targets ← expandEntries root defaultEntries
    lintInChildren targets
  else
    let targets ← expandEntries root (args.map FilePath.mk)
    let mut exit : UInt32 := (0 : UInt32)
    for path in targets do
      let code ← lintFile root path
      if code ≠ (0 : UInt32) then
        exit := code
    return exit

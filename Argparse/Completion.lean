import Argparse.Completion.Core
import Argparse.Completion.Bash
import Argparse.Completion.Zsh
import Argparse.Completion.Fish

namespace Argparse
namespace Completion

/-- Completion module for bash shells. -/
def bashModule : Module := Bash.module

/-- Completion module for zsh shells. -/
def zshModule : Module := Zsh.module

/-- Completion module for fish shells. -/
def fishModule : Module := Fish.module

namespace Shell

/-- Retrieve the completion module corresponding to a shell. -/
def module : Shell → Module
  | .bash => bashModule
  | .zsh => zshModule
  | .fish => fishModule

/-- Render a completion script for the requested shell. -/
def render (shell : Shell) (info : ParserInfo α) : String :=
  Completion.renderWithModule (module shell) info

end Shell

/-- All bundled completion modules. -/
def allModules : List Module := [bashModule, zshModule, fishModule]

/-- Alias of `allModules` for backwards compatibility. -/
def allCompletionModules : List Module := allModules

/-- Render completion output for a shell using the provided parser metadata. -/
def renderForShell (info : ParserInfo α) (shell : Shell) : String :=
  Shell.render shell info

end Completion

namespace ParserInfo

open Completion

/-- Render a bash completion script for this parser. -/
def renderBashCompletion (info : ParserInfo α) : String :=
  Completion.renderWithModule Completion.bashModule info

/-- Render a zsh completion script for this parser. -/
def renderZshCompletion (info : ParserInfo α) : String :=
  Completion.renderWithModule Completion.zshModule info

/-- Render a fish completion script for this parser. -/
def renderFishCompletion (info : ParserInfo α) : String :=
  Completion.renderWithModule Completion.fishModule info

/-- Render a completion script for the specified shell. -/
def renderCompletionFor (shell : Completion.Shell) (info : ParserInfo α) : String :=
  Completion.renderForShell info shell

/-- Re-export of the bash completion module for backwards compatibility. -/
def bashModule : Completion.Module := Completion.bashModule
/-- Re-export of the zsh completion module for backwards compatibility. -/
def zshModule : Completion.Module := Completion.zshModule
/-- Re-export of the fish completion module for backwards compatibility. -/
def fishModule : Completion.Module := Completion.fishModule
/-- Re-export of all completion modules for backwards compatibility. -/
def allCompletionModules : List Completion.Module := Completion.allCompletionModules

end ParserInfo
end Argparse

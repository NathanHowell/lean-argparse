import Argparse.Completion.Core
import Argparse.Completion.Bash
import Argparse.Completion.Zsh
import Argparse.Completion.Fish

namespace Argparse
namespace Completion

def bashModule : Module := Bash.module

def zshModule : Module := Zsh.module

def fishModule : Module := Fish.module

namespace Shell

def module : Shell → Module
  | .bash => bashModule
  | .zsh => zshModule
  | .fish => fishModule

def render (shell : Shell) (info : ParserInfo α) : String :=
  Completion.renderWithModule (module shell) info

end Shell

def allModules : List Module := [bashModule, zshModule, fishModule]

def allCompletionModules : List Module := allModules

def renderForShell (info : ParserInfo α) (shell : Shell) : String :=
  Shell.render shell info

end Completion

namespace ParserInfo

open Completion

def renderBashCompletion (info : ParserInfo α) : String :=
  Completion.renderWithModule Completion.bashModule info

def renderZshCompletion (info : ParserInfo α) : String :=
  Completion.renderWithModule Completion.zshModule info

def renderFishCompletion (info : ParserInfo α) : String :=
  Completion.renderWithModule Completion.fishModule info

def renderCompletionFor (shell : Completion.Shell) (info : ParserInfo α) : String :=
  Completion.renderForShell info shell

def bashModule : Completion.Module := Completion.bashModule
def zshModule : Completion.Module := Completion.zshModule
def fishModule : Completion.Module := Completion.fishModule
def allCompletionModules : List Completion.Module := Completion.allCompletionModules

end ParserInfo
end Argparse

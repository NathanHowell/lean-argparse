import LeanArgparse.Completion.Core
import LeanArgparse.Completion.Bash
import LeanArgparse.Completion.Zsh
import LeanArgparse.Completion.Fish

namespace LeanArgparse
namespace Completion

def bashModule : Module := Bash.module

def zshModule : Module := Zsh.module

def fishModule : Module := Fish.module

def allModules : List Module := [bashModule, zshModule, fishModule]

def allCompletionModules : List Module := allModules

end Completion

namespace ParserInfo

open Completion

def renderBashCompletion (info : ParserInfo α) : String :=
  Completion.renderWithModule Completion.bashModule info

def renderZshCompletion (info : ParserInfo α) : String :=
  Completion.renderWithModule Completion.zshModule info

def renderFishCompletion (info : ParserInfo α) : String :=
  Completion.renderWithModule Completion.fishModule info

def bashModule : Completion.Module := Completion.bashModule
def zshModule : Completion.Module := Completion.zshModule
def fishModule : Completion.Module := Completion.fishModule
def allCompletionModules : List Completion.Module := Completion.allCompletionModules

end ParserInfo
end LeanArgparse

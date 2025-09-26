import Std
import Argparse.Basic.Docs
import Argparse.Native.Error
import Argparse.Native.ArgStream
import Argparse.Native.Consumer

namespace Argparse
namespace Native

open Usage

/-- Result type returned by the native interpreter. -/
inductive Result (α : Type) where
  | ok (value : α) (rest : ArgStream)
  | error (info : Error)
  deriving Repr

namespace Result

@[simp] def map {α β : Type} (f : α → β) : Result α → Result β
  | .ok value rest => .ok (f value) rest
  | .error err => .error err

end Result

/-- Metadata-only grammar description. -/
structure Grammar (α : Type) where
  usage : Usage

namespace Grammar

/-- Embed a pure value (no usage contribution). -/
def pure {α : Type} (_ : α) : Grammar α :=
  { usage := Usage.empty }

/-- Record a failure branch (no usage contribution). -/
def fail {α : Type} : Grammar α :=
  { usage := Usage.empty }

/-- Map over a grammar without changing usage. -/
def map {α β : Type} (g : Grammar α) (_ : α → β) : Grammar β :=
  { usage := g.usage }

/-- Sequential application combines usage from left to right. -/
def seq {α β : Type} (gf : Grammar (α → β)) (ga : Grammar α) : Grammar β :=
  { usage := Usage.append gf.usage ga.usage }

/-- Primitive positional argument metadata. -/
def positional (doc : PositionalDoc) : Grammar String :=
  { usage := Usage.mergePositional doc Usage.empty }

/-- Flag metadata contributes the provided option doc. -/
def flag (doc : OptionDoc) : Grammar Bool :=
  { usage := Usage.mergeOption doc Usage.empty }

/-- Option metadata contributes the provided option doc. -/
def option (doc : OptionDoc) : Grammar (Option String) :=
  { usage := Usage.mergeOption doc Usage.empty }

end Grammar

/-- Interpreter pairs metadata with an evaluator on `ArgStream`. -/
structure Interpreter (α : Type) where
  grammar : Grammar α
  eval : ArgStream → Result α

namespace Interpreter

/-- Access the usage metadata for an interpreter. -/
def usage {α : Type} (i : Interpreter α) : Usage :=
  i.grammar.usage

/-- Pure value interpreter. -/
def pure {α : Type} (value : α) : Interpreter α :=
  { grammar := Grammar.pure value
    , eval := fun stream => .ok value stream }

/-- Interpreter that immediately fails with `err`. -/
def fail {α : Type} (err : Error) : Interpreter α :=
  { grammar := Grammar.fail
    , eval := fun _ => .error err }

/-- Functorial map over interpreter results. -/
def map {α β : Type} (i : Interpreter α) (f : α → β) : Interpreter β :=
  { grammar := Grammar.map i.grammar f
    , eval := fun stream => Result.map f (i.eval stream) }

/-- Sequential application of interpreters. -/
def seq {α β : Type} (ifn : Interpreter (α → β)) (ival : Interpreter α) : Interpreter β :=
  { grammar := Grammar.seq ifn.grammar ival.grammar
    , eval := fun stream =>
        match ifn.eval stream with
        | .ok fn stream' => Result.map fn (ival.eval stream')
        | .error err => .error err }

/-- Consume a positional argument using the structural stream. -/
def positional (doc : PositionalDoc) : Interpreter String :=
  { grammar := Grammar.positional doc
    , eval :=
        fun stream =>
          match ArgStream.next? stream with
          | some (tok, rest) => .ok tok rest
          | none =>
              .error {
                code := .missing,
                subject? := some doc.metavar
              } }

/-- Boolean flag that reports presence of the given token. -/
def flag {α : Type} [DecidableEq α] [Token.TokenSpec α]
    (doc : OptionDoc) (name : α) : Interpreter Bool :=
  { grammar := Grammar.flag doc
    , eval := fun stream =>
        match Consumer.consumeFlag name stream with
        | .ok (present, stream') => .ok present stream'
        | .error err => .error err }

/-- Option parser returning the associated value when present. -/
def option {α : Type} [DecidableEq α] [Token.TokenSpec α]
    (doc : OptionDoc) (name : α) : Interpreter (Option String) :=
  { grammar := Grammar.option doc
    , eval := fun stream =>
        match Consumer.consumeValue name stream with
        | .ok (value?, stream') => .ok value? stream'
        | .error err => .error err }

/-- Long flag convenience helper. -/
def longFlag (name : String)
    (doc : OptionDoc := { long? := some name, required := false }) : Interpreter Bool :=
  flag doc name

/-- Short flag convenience helper. -/
def shortFlag (name : Char)
    (doc : OptionDoc := { short? := some name, required := false }) : Interpreter Bool :=
  flag doc name

/-- Long option convenience helper. -/
def longOption (name : String)
    (doc : OptionDoc := { long? := some name, required := false }) : Interpreter (Option String) :=
  option doc name

/-- Short option convenience helper. -/
def shortOption (name : Char)
    (doc : OptionDoc := { short? := some name, required := false }) : Interpreter (Option String) :=
  option doc name

end Interpreter

end Native
end Argparse

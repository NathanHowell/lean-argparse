import ArgParse.Core.Types

/-!
# ArgParse.Core.Parser

The parser type and its instances. This is the whole runtime carrier: an opaque
function from state to result, which every layer above pairs with a description
rather than replacing.
-/

namespace ArgParse

/-- Parser over normalized state, returning either a value and new state or an `Error`. -/
abbrev Parser (α : Type) := State → Result α

namespace Parser

/-- Always succeeds without consuming input. -/
def pure (a : α) : Parser α := fun st => .ok a st

/-- Map over the result of a parser while threading the state. -/
def map (f : α → β) (p : Parser α) : Parser β := fun st =>
  match p st with
  | .ok a st' => .ok (f a) st'
  | .err e => .err e

/-- Apply a parsed function to a lazily supplied parsed value. -/
def seq (pf : Parser (α → β)) (pa : Unit → Parser α) : Parser β := fun st =>
  match pf st with
  | .ok f st' =>
      match pa () st' with
      | .ok a st'' => .ok (f a) st''
      | .err e => .err e
  | .err e => .err e

/-- Left sequencing helper. -/
def seqLeft (pa : Parser α) (pb : Unit → Parser β) : Parser α :=
  seq (map (fun a => fun (_ : β) => a) pa) pb

/-- Right sequencing helper. -/
def seqRight (pa : Parser α) (pb : Unit → Parser β) : Parser β :=
  seq (map (fun (_ : α) => id) pa) pb

/-- Fail with a supplied structured error. -/
def fail (err : Error) : Parser α := fun _ => .err err

/-- Default error used to satisfy `Alternative.failure`. -/
def emptyError : Error :=
  { kind := .custom, context := [], expect := [] }

/-- Prefer the first successful parser, falling back to the (lazy) second on error. -/
def orElse (pa : Parser α) (pb : Unit → Parser α) : Parser α := fun st =>
  match pa st with
  | .ok a st' => .ok a st'
  | .err _ => pb () st

end Parser

instance : Functor Parser where
  map := Parser.map

instance : Applicative Parser where
  map := Parser.map
  pure := Parser.pure
  seq := Parser.seq
  seqLeft := Parser.seqLeft
  seqRight := Parser.seqRight

instance : Pure Parser where
  pure := Parser.pure

instance : Seq Parser where
  seq := Parser.seq

instance : SeqLeft Parser where
  seqLeft := Parser.seqLeft

instance : SeqRight Parser where
  seqRight := Parser.seqRight

instance : Alternative Parser where
  failure := Parser.fail Parser.emptyError
  orElse := Parser.orElse

end ArgParse

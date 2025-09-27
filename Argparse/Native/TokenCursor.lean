import Std
import Argparse.Native.ParsedToken

namespace Argparse
namespace Native

/--
Forward-only cursor over classified tokens. The cursor stores the underlying
array together with the current offset and maintains `pos ≤ data.size` so proofs
can reason about indices via plain `Nat` arithmetic.
-/
structure TokenCursor where
  data : Array ParsedToken
  pos : Nat := 0
  bound : pos ≤ data.size := by
    exact Nat.zero_le _
  deriving Repr

namespace TokenCursor

def ofArray (data : Array ParsedToken) : TokenCursor :=
  { data := data }

@[simp] def ofList (tokens : List ParsedToken) : TokenCursor :=
  ofArray tokens.toArray

@[simp] def fromClassified (tokens : List String) : TokenCursor :=
  ofList (classify tokens)

@[simp] def remaining (cursor : TokenCursor) : Nat :=
  cursor.data.size - cursor.pos

@[simp] theorem pos_le_size (cursor : TokenCursor) :
    cursor.pos ≤ cursor.data.size :=
  cursor.bound

@[simp] def isFinished (cursor : TokenCursor) : Bool :=
  decide (cursor.pos = cursor.data.size)

@[simp] def current? (cursor : TokenCursor) : Option ParsedToken :=
  if h : cursor.pos < cursor.data.size then
    some (cursor.data[cursor.pos]'h)
  else
    none

@[simp] def advance (cursor : TokenCursor)
    (h : cursor.pos < cursor.data.size) : TokenCursor :=
  { cursor with
      pos := cursor.pos + 1
      bound := Nat.succ_le_of_lt h }

@[simp] def next? (cursor : TokenCursor) :
    Option (ParsedToken × TokenCursor) :=
  if h : cursor.pos < cursor.data.size then
    let tok := cursor.data[cursor.pos]'h
    let cursor' := cursor.advance h
    some (tok, cursor')
  else
    none

@[simp] def drop (cursor : TokenCursor) (n : Nat)
    (h : cursor.pos + n ≤ cursor.data.size) : TokenCursor :=
  { cursor with
      pos := cursor.pos + n
      bound := h }

end TokenCursor

end Native
end Argparse

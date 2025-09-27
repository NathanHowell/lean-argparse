import Std
import Argparse.Native.Error
import Argparse.Native.ParsedToken
import Argparse.Native.Token

namespace Argparse
namespace Native

open Token

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

/-- Remaining tokens after dropping the already-consumed prefix. -/
@[simp] def toList (cursor : TokenCursor) : List ParsedToken :=
  cursor.data.toList.drop cursor.pos

@[simp] def remainingTokens (cursor : TokenCursor) : List ParsedToken :=
  cursor.toList

@[simp] def ofRemainingList (tokens : List ParsedToken) : TokenCursor :=
  ofList tokens

class ToParsedName (α : Type) where
  toParsedName : α → ParsedName

instance : ToParsedName String where
  toParsedName name := ParsedName.long name

instance : ToParsedName Char where
  toParsedName name := ParsedName.short name

private def rebuild (revSkipped rest : List ParsedToken) : List ParsedToken :=
  revSkipped.reverse ++ rest

private def takePositionalAux
    (revSkipped : List ParsedToken)
    : List ParsedToken → Option (String × List ParsedToken)
  | [] => none
  | tok :: rest =>
      match tok with
      | .positional value =>
          let restTokens := rebuild revSkipped rest
          some (value, restTokens)
      | .option _ =>
          takePositionalAux (tok :: revSkipped) rest

/-- Remove the first positional token, leaving option tokens in place. -/
@[simp] def takePositional? (cursor : TokenCursor) :
    Option (String × TokenCursor) :=
  match takePositionalAux [] cursor.remainingTokens with
  | some (value, restTokens) => some (value, ofRemainingList restTokens)
  | none => none

private def describe {α : Type} [TokenSpec α] (name : α) : String :=
  TokenSpec.describe name

private def mismatchError {α : Type} [TokenSpec α]
    (name : α) (detail : String) : Error :=
  { code := .invalid
    , subject? := some (describe name)
    , detail? := some detail }

private def missingValueError {α : Type} [TokenSpec α] (name : α) : Error :=
  { code := .missing
    , subject? := some (describe name) }

private def consumeFlagList {α : Type} [TokenSpec α] [ToParsedName α]
    (name : α) : List ParsedToken → Except Error (Bool × List ParsedToken)
  | [] => .ok (false, [])
  | tok :: rest =>
      match tok with
      | ParsedToken.option data =>
          if data.name = ToParsedName.toParsedName name then
            match data.inlineValue? with
            | some _ =>
                .error <|
                  mismatchError name
                    s!"Flag {describe name} does not accept a value"
            | none =>
                match consumeFlagList name rest with
                | .ok (_, restTokens) => .ok (true, restTokens)
                | .error err => .error err
          else
            match consumeFlagList name rest with
            | .ok (present, restTokens) =>
                .ok (present, ParsedToken.option data :: restTokens)
            | .error err => .error err
      | tok =>
          match consumeFlagList name rest with
          | .ok (present, restTokens) => .ok (present, tok :: restTokens)
          | .error err => .error err

/-- Remove matching flag tokens, reporting whether any were present. -/
def consumeFlag {α : Type} [TokenSpec α] [ToParsedName α]
    (name : α) (cursor : TokenCursor)
    : Except Error (Bool × TokenCursor) := do
  let (present, restTokens) ← consumeFlagList name cursor.remainingTokens
  pure (present, ofRemainingList restTokens)

private def consumeOptionList {α : Type} [TokenSpec α] [ToParsedName α]
    (name : α)
    : List ParsedToken → Except Error (Option String × List ParsedToken)
  | tokens =>
      let rec go
          : List ParsedToken → List ParsedToken → Option String → Except Error (Option String × List ParsedToken)
        | [], kept, last? => .ok (last?, kept.reverse)
        | ParsedToken.option data :: rest, kept, last? =>
            if data.name = ToParsedName.toParsedName name then
              match data.inlineValue? with
              | some value =>
                  go rest kept (some value)
              | none =>
                  match rest with
                  | ParsedToken.positional value :: restTail =>
                      go restTail kept (some value)
                  | _ => .error <| missingValueError name
            else
              go rest (ParsedToken.option data :: kept) last?
        | tok :: rest, kept, last? =>
            go rest (tok :: kept) last?
      go tokens [] none

private def extractOptionValues {α : Type} [TokenSpec α] [ToParsedName α]
    (name : α) (cursor : TokenCursor)
    : Except Error (Option String × TokenCursor) := do
  let (value?, restTokens) ← consumeOptionList name cursor.remainingTokens
  pure (value?, ofRemainingList restTokens)

/-- Retrieve the last value supplied for an option, if any, using last-wins semantics. -/
def consumeValue {α : Type} [TokenSpec α] [ToParsedName α]
    (name : α) (cursor : TokenCursor)
    : Except Error (Option String × TokenCursor) := do
  extractOptionValues name cursor

end TokenCursor

end Native
end Argparse

import Argparse.Native.Error
import Argparse.Native.ParsedToken
import Argparse.Native.Token

namespace Argparse
namespace Native

-- Reference `TokenSpec` helpers without spelling out the full namespace.
open Token

/--
State representation for the parsed-token interpreter pipeline. The underlying
list maintains command-line order so structural proofs can count remaining
arguments via `tokens.length`.
-/
structure TokenStream where
  tokens : List ParsedToken
  deriving DecidableEq, Repr

namespace TokenStream

@[simp] def ofList (tokens : List ParsedToken) : TokenStream :=
  { tokens }

@[simp] def toList (stream : TokenStream) : List ParsedToken :=
  stream.tokens

@[simp] theorem toList_ofList (tokens : List ParsedToken) :
    toList (ofList tokens) = tokens := by
  rfl

@[simp] theorem ofList_toList (stream : TokenStream) :
    ofList (toList stream) = stream := by
  cases stream
  rfl

@[simp] def length (stream : TokenStream) : Nat :=
  stream.tokens.length

private def rebuild (revSkipped rest : List ParsedToken) : TokenStream :=
  ofList (revSkipped.reverse ++ rest)

private def takePositionalAux
    (revSkipped : List ParsedToken)
    : List ParsedToken → Option (String × TokenStream)
  | [] => none
  | tok :: rest =>
      match tok with
      | .positional value =>
          let stream := rebuild revSkipped rest
          some (value, stream)
      | .option _ =>
          takePositionalAux (tok :: revSkipped) rest

/-- Remove the first positional token, leaving option tokens in place. -/
@[simp] def takePositional? (stream : TokenStream) :
    Option (String × TokenStream) :=
  takePositionalAux [] stream.tokens

class ToParsedName (α : Type) where
  toParsedName : α → ParsedName

instance : ToParsedName String where
  toParsedName name := ParsedName.long name

instance : ToParsedName Char where
  toParsedName name := ParsedName.short name

private def describe [TokenSpec α] (name : α) : String :=
  TokenSpec.describe name

private def mismatchError [TokenSpec α]
    (name : α) (detail : String) : Error :=
  { code := .invalid
    , subject? := some (describe name)
    , detail? := some detail }

private def missingValueError [TokenSpec α] (name : α) : Error :=
  { code := .missing
    , subject? := some (describe name) }

private def consumeFlagLoop [TokenSpec α] [ToParsedName α]
    (name : α)
    (tokens : List ParsedToken)
    (revKept : List ParsedToken)
    (present : Bool) : Except Error (Bool × List ParsedToken) :=
  match tokens with
  | [] => .ok (present, revKept.reverse)
  | ParsedToken.option data :: rest =>
      if data.name = ToParsedName.toParsedName name then
        match data.inlineValue? with
        | some _ =>
            .error <|
              mismatchError name
                s!"Flag {describe name} does not accept a value"
        | none => consumeFlagLoop name rest revKept true
      else
        consumeFlagLoop name rest (ParsedToken.option data :: revKept) present
  | tok :: rest => consumeFlagLoop name rest (tok :: revKept) present

/-- Remove matching flag tokens, reporting whether any were present. -/
def consumeFlag [TokenSpec α] [ToParsedName α]
    (name : α) (stream : TokenStream)
    : Except Error (Bool × TokenStream) :=
  match consumeFlagLoop name stream.tokens [] false with
  | .ok (present, newTokens) => .ok (present, ofList newTokens)
  | .error err => .error err

private def consumeOptionLoop [TokenSpec α] [ToParsedName α]
    (name : α)
    (tokens : List ParsedToken)
    (revKept : List ParsedToken)
    (revValues : List String)
    : Except Error (List String × List ParsedToken) :=
  match tokens with
  | [] => .ok (revValues, revKept.reverse)
  | ParsedToken.option data :: rest =>
      if data.name = ToParsedName.toParsedName name then
        match data.inlineValue? with
        | some value =>
            consumeOptionLoop name rest revKept (value :: revValues)
        | none =>
            match rest with
            | ParsedToken.positional value :: restTail =>
                consumeOptionLoop name restTail revKept (value :: revValues)
            | _ => .error <| missingValueError name
      else
        consumeOptionLoop name rest (ParsedToken.option data :: revKept) revValues
  | tok :: rest =>
      consumeOptionLoop name rest (tok :: revKept) revValues

/-- Remove option tokens and collect their supplied values in reverse order. -/
private def extractOptionValues [TokenSpec α] [ToParsedName α]
    (name : α) (stream : TokenStream)
    : Except Error (List String × TokenStream) :=
  match consumeOptionLoop name stream.tokens [] [] with
  | .ok (values, newTokens) => .ok (values, ofList newTokens)
  | .error err => .error err

/-- Retrieve the last value supplied for an option, if any, using last-wins semantics. -/
def consumeValue [TokenSpec α] [ToParsedName α]
    (name : α) (stream : TokenStream)
    : Except Error (Option String × TokenStream) := do
  let (values, newStream) ← extractOptionValues name stream
  match values with
  | [] => pure (none, newStream)
  | value :: _ => pure (some value, newStream)

end TokenStream

end Native
end Argparse

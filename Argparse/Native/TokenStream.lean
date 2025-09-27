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

private def consumeFlagList [TokenSpec α] [ToParsedName α]
    (name : α) : List ParsedToken → Except Error (Bool × List ParsedToken)
  | [] => .ok (false, [])
  | ParsedToken.option data :: rest =>
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
  | tok :: rest =>
      match consumeFlagList name rest with
      | .ok (present, restTokens) => .ok (present, tok :: restTokens)
      | .error err => .error err

/-- Remove matching flag tokens, reporting whether any were present. -/
def consumeFlag [TokenSpec α] [ToParsedName α]
    (name : α) (stream : TokenStream)
    : Except Error (Bool × TokenStream) := do
  let (present, restTokens) ← consumeFlagList name stream.tokens
  pure (present, ofList restTokens)

private def consumeOptionList [TokenSpec α] [ToParsedName α]
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

private def extractOptionValues [TokenSpec α] [ToParsedName α]
    (name : α) (stream : TokenStream)
    : Except Error (Option String × TokenStream) := do
  let (value?, restTokens) ← consumeOptionList name stream.tokens
  pure (value?, ofList restTokens)

/-- Retrieve the last value supplied for an option, if any, using last-wins semantics. -/
def consumeValue [TokenSpec α] [ToParsedName α]
    (name : α) (stream : TokenStream)
    : Except Error (Option String × TokenStream) := do
  extractOptionValues name stream

end TokenStream

end Native
end Argparse

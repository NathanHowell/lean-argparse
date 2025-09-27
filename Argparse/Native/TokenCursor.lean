import Std
import Argparse.Native.Error
import Argparse.Native.ParsedToken
import Argparse.Native.Token

namespace Argparse
namespace Native

open Token

/--
Token cursor that separates option-like tokens from positional arguments.
Options are stored in their classified form while positionals are kept in a
queue (front-loaded) so consumers can pop values without revisiting the original
argv.
-/
structure TokenCursor where
  options : Array ParsedOption
  positionals : Array String
  deriving Repr

namespace TokenCursor

@[simp] def ofArrays
    (options : Array ParsedOption) (positionals : Array String) : TokenCursor :=
  { options, positionals }

@[simp] def fromClassified (tokens : ClassifiedTokens) : TokenCursor :=
  { options := tokens.options
    , positionals := tokens.positionals }

@[simp] def fromArgv (argv : List String) : TokenCursor :=
  fromClassified (classify argv)

@[simp] def optionCount (cursor : TokenCursor) : Nat :=
  cursor.options.size

@[simp] def positionalCount (cursor : TokenCursor) : Nat :=
  cursor.positionals.size

@[simp] def remaining (cursor : TokenCursor) : Nat :=
  cursor.optionCount + cursor.positionalCount

@[simp] def isFinished (cursor : TokenCursor) : Bool :=
  decide (cursor.remaining = 0)

@[simp] def remainingOptions (cursor : TokenCursor) : Array ParsedOption :=
  cursor.options

@[simp] def remainingPositionals (cursor : TokenCursor) : Array String :=
  cursor.positionals

@[simp] def toLists (cursor : TokenCursor) : List ParsedOption × List String :=
  (cursor.options.toList, cursor.positionals.toList)

@[simp] def ofLists (options : List ParsedOption) (positionals : List String)
    : TokenCursor :=
  ofArrays options.toArray positionals.toArray

class ToParsedName (α : Type) where
  toParsedName : α → ParsedName

instance : ToParsedName String where
  toParsedName name := ParsedName.long name

instance : ToParsedName Char where
  toParsedName name := ParsedName.short name

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

private def dropHead (arr : Array String) : Array String :=
  arr.extract 1 arr.size

/-- Remove the next positional argument, if any remain. -/
@[simp] def takePositional? (cursor : TokenCursor) :
    Option (String × TokenCursor) :=
  match cursor.positionals[0]? with
  | none => none
  | some value =>
      let rest := dropHead cursor.positionals
      some (value, { cursor with positionals := rest })

/-- Remove matching flag tokens and report whether the flag was present. -/
def consumeFlag {α : Type} [TokenSpec α] [ToParsedName α]
    (name : α) (cursor : TokenCursor)
    : Except Error (Bool × TokenCursor) := do
  let target := ToParsedName.toParsedName name
  let init : Bool × Array ParsedOption :=
    (false, Array.mkEmpty cursor.options.size)
  let (present, remaining) ←
    cursor.options.foldlM
      (fun state opt => do
        let (seen, kept) := state
        if opt.name = target then
          match opt.inlineValue? with
          | some _ =>
              throw <|
                mismatchError name
                  s!"Flag {describe name} does not accept a value"
          | none =>
              pure (true, kept)
        else
          pure (seen, kept.push opt))
      init
  pure (present, { cursor with options := remaining })

/-- Retrieve the last supplied value for an option, if present. -/
def consumeValue {α : Type} [TokenSpec α] [ToParsedName α]
    (name : α) (cursor : TokenCursor)
    : Except Error (Option String × TokenCursor) := do
  let target := ToParsedName.toParsedName name
  let init : Option String × Array ParsedOption × Array String :=
    (none, Array.mkEmpty cursor.options.size, cursor.positionals)
  let (value?, remainingOpts, remainingPos) ←
    cursor.options.foldlM
      (fun state opt => do
        let (last?, kept, pos) := state
        if opt.name = target then
          match opt.inlineValue? with
          | some value =>
              pure (some value, kept, pos)
          | none =>
              match pos[0]? with
              | some value =>
                  pure (some value, kept, dropHead pos)
              | none =>
                  throw <| missingValueError name
        else
          pure (last?, kept.push opt, pos))
      init
  pure (value?, { options := remainingOpts, positionals := remainingPos })

end TokenCursor

end Native
end Argparse

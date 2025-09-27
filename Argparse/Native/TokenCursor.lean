import Argparse.Native.Error
import Argparse.Native.ParsedToken
import Argparse.Native.Token

namespace Argparse
namespace Native

open Token

/--
`TokenCursor` stores the classified option tokens alongside the positional
arguments that remain after the first positional (or explicit `--`). The fields
use `List` so structural recursion and length arithmetic remain straightforward
in proofs.
-/
structure TokenCursor where
  options : List ParsedOption
  positionals : List String
  deriving Repr

namespace TokenCursor

@[simp] def ofLists (options : List ParsedOption) (positionals : List String)
    : TokenCursor :=
  { options, positionals }

@[simp] def fromClassified (tokens : ClassifiedTokens) : TokenCursor :=
  { options := tokens.options, positionals := tokens.positionals }

@[simp] def fromArgv (argv : List String) : TokenCursor :=
  fromClassified (classify argv)

@[simp] def optionCount (cursor : TokenCursor) : Nat :=
  cursor.options.length

@[simp] def positionalCount (cursor : TokenCursor) : Nat :=
  cursor.positionals.length

@[simp] def remaining (cursor : TokenCursor) : Nat :=
  cursor.optionCount + cursor.positionalCount

@[simp] def isFinished (cursor : TokenCursor) : Bool :=
  decide (cursor.remaining = 0)

@[simp] def remainingOptions (cursor : TokenCursor) : List ParsedOption :=
  cursor.options

@[simp] def remainingPositionals (cursor : TokenCursor) : List String :=
  cursor.positionals

@[simp] def toLists (cursor : TokenCursor) : List ParsedOption × List String :=
  (cursor.options, cursor.positionals)

class ToParsedName (α : Type) where
  toParsedName : α → ParsedName

instance : ToParsedName String where
  toParsedName name := ParsedName.long name

instance : ToParsedName Char where
  toParsedName name := ParsedName.short name

private def describe {α : Type} [TokenSpec α] (name : α) : String :=
  TokenSpec.describe name

private def missingValueError {α : Type} [TokenSpec α] (name : α) : Error :=
  { code := .missing
    , subject? := some (describe name) }

private def invalidInlineValue (subject : String) : Error :=
  { code := .invalid
    , subject? := some subject
    , detail? := some s!"Flag {subject} does not accept a value" }

/-- Remove the next positional argument, if any remain. -/
@[simp] def takePositional? (cursor : TokenCursor) :
    Option (String × TokenCursor) :=
  match cursor.positionals with
  | [] => none
  | value :: rest => some (value, { cursor with positionals := rest })

/-- Structural helper for flag consumption. Filters matching options while
tracking whether any occurrence was observed. -/
private def consumeFlagList (target : ParsedName) (subject : String)
    : List ParsedOption → Except Error (Bool × List ParsedOption)
  | [] => pure (false, [])
  | opt :: rest =>
      if opt.name == target then
        if opt.inlineValue?.isSome then
          throw (invalidInlineValue subject)
        else
          do
            let (_, kept) ← consumeFlagList target subject rest
            pure (true, kept)
      else
        do
          let (present, kept) ← consumeFlagList target subject rest
          pure (present, opt :: kept)

/-- Remove matching flag tokens and report whether the flag was present. -/
def consumeFlag {α : Type} [TokenSpec α] [ToParsedName α]
    (name : α) (cursor : TokenCursor)
    : Except Error (Bool × TokenCursor) := do
  let target := ToParsedName.toParsedName name
  let subject := describe name
  let (present, kept) ← consumeFlagList target subject cursor.options
  pure (present, { cursor with options := kept })

/-- Structural helper for option value consumption. Processes matching options
left-to-right while threading the positional list and tracking the final value
using "last occurrence wins" semantics. -/
private def consumeValueList {α : Type} [TokenSpec α] [ToParsedName α]
    (name : α) (target : ParsedName)
    (options : List ParsedOption) (positionals : List String)
    : Except Error (Option String × List ParsedOption × List String) :=
  match options with
  | [] => pure (none, [], positionals)
  | opt :: rest =>
      if opt.name == target then
        match opt.inlineValue? with
        | some value => do
            let (last?, kept, positionals') ← consumeValueList name target rest positionals
            let final :=
              match last? with
              | some _ => last?
              | none => some value
            pure (final, kept, positionals')
        | none =>
            match positionals with
            | [] => throw (missingValueError name)
            | value :: tail => do
                let (last?, kept, positionals') ← consumeValueList name target rest tail
                let final :=
                  match last? with
                  | some _ => last?
                  | none => some value
                pure (final, kept, positionals')
      else
        do
          let (last?, kept, positionals') ← consumeValueList name target rest positionals
          pure (last?, opt :: kept, positionals')

/-- Retrieve the last supplied value for an option, if present. -/
def consumeValue {α : Type} [TokenSpec α] [ToParsedName α]
    (name : α) (cursor : TokenCursor)
    : Except Error (Option String × TokenCursor) := do
  let target := ToParsedName.toParsedName name
  let (value?, kept, remainingPos) ←
    consumeValueList name target cursor.options cursor.positionals
  pure (value?, { options := kept, positionals := remainingPos })

end TokenCursor

end Native
end Argparse

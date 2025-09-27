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
  | [] => Except.ok (false, [])
  | opt :: rest =>
      if _ : opt.name = target then
        match opt.inlineValue? with
        | some _ => Except.error (invalidInlineValue subject)
        | none =>
            match consumeFlagList target subject rest with
            | Except.ok (_, kept) => Except.ok (true, kept)
            | Except.error err => Except.error err
      else
        match consumeFlagList target subject rest with
        | Except.ok (present, kept) => Except.ok (present, opt :: kept)
        | Except.error err => Except.error err

/-- Remove matching flag tokens and report whether the flag was present. -/
def consumeFlag {α : Type} [TokenSpec α] [ToParsedName α]
    (name : α) (cursor : TokenCursor)
    : Except Error (Bool × TokenCursor) := do
  let target := ToParsedName.toParsedName name
  let subject := describe name
  match consumeFlagList target subject cursor.options with
  | Except.error err => Except.error err
  | Except.ok (present, kept) =>
      Except.ok (present, { cursor with options := kept })

/-- Structural helper for option value consumption. Processes matching options
left-to-right while threading the positional list and tracking the final value
using "last occurrence wins" semantics. -/
private def consumeValueList {α : Type} [TokenSpec α] [ToParsedName α]
    (name : α) (target : ParsedName)
    (options : List ParsedOption) (positionals : List String)
    : Except Error (Option String × List ParsedOption × List String) :=
  match options with
  | [] => Except.ok (none, [], positionals)
  | opt :: rest =>
      if _ : opt.name = target then
        match opt.inlineValue? with
        | some value =>
            match consumeValueList name target rest positionals with
            | Except.ok (some last, kept, positionals') =>
                Except.ok (some last, kept, positionals')
            | Except.ok (none, kept, positionals') =>
                Except.ok (some value, kept, positionals')
            | Except.error err => Except.error err
        | none =>
            match positionals with
            | [] => Except.error (missingValueError name)
            | value :: tail =>
                match consumeValueList name target rest tail with
                | Except.ok (some last, kept, positionals') =>
                    Except.ok (some last, kept, positionals')
                | Except.ok (none, kept, positionals') =>
                    Except.ok (some value, kept, positionals')
                | Except.error err => Except.error err
      else
        match consumeValueList name target rest positionals with
        | Except.ok (last?, kept, positionals') =>
            Except.ok (last?, opt :: kept, positionals')
        | Except.error err => Except.error err

/-- Retrieve the last supplied value for an option, if present. -/
def consumeValue {α : Type} [TokenSpec α] [ToParsedName α]
    (name : α) (cursor : TokenCursor)
    : Except Error (Option String × TokenCursor) :=
  let target := ToParsedName.toParsedName name
  match consumeValueList name target cursor.options cursor.positionals with
  | Except.error err => Except.error err
  | Except.ok (value?, kept, remainingPos) =>
      Except.ok (value?, { options := kept, positionals := remainingPos })

/-! ### Length and progress facts for structural helpers -/

theorem takePositional?_progress {cursor : TokenCursor} {value : String}
    {cursor' : TokenCursor}
    (h : takePositional? cursor = some (value, cursor')) :
    cursor'.options = cursor.options ∧
      cursor'.positionals.length + 1 = cursor.positionals.length := by
  cases cursor with
  | mk options positionals =>
    cases positionals with
    | nil =>
        simp [takePositional?] at h
    | cons head tail =>
        simp [takePositional?] at h
        rcases h with ⟨_, hCursor⟩
        cases hCursor
        constructor <;> simp

theorem takePositional?_remaining {cursor : TokenCursor} {value : String}
    {cursor' : TokenCursor}
    (h : takePositional? cursor = some (value, cursor')) :
    cursor'.remaining + 1 = cursor.remaining := by
  obtain ⟨hOptions, hPos⟩ := takePositional?_progress (cursor := cursor) (cursor' := cursor') h
  have hOptLen : cursor'.options.length = cursor.options.length := by
    simpa using congrArg List.length hOptions
  calc
    cursor'.remaining + 1
        = (cursor'.options.length + cursor'.positionals.length) + 1 := by
            simp [TokenCursor.remaining, TokenCursor.optionCount, TokenCursor.positionalCount]
    _ = cursor'.options.length + (cursor'.positionals.length + 1) := by
            simp [Nat.add_assoc, Nat.add_left_comm, Nat.add_comm]
    _ = cursor'.options.length + cursor.positionals.length := by
            simp [hPos, Nat.add_comm]
    _ = cursor.options.length + cursor.positionals.length := by
            simp [hOptLen]
    _ = cursor.remaining := by
            simp [TokenCursor.remaining, TokenCursor.optionCount, TokenCursor.positionalCount]

theorem consumeFlagList_ok_match (target : ParsedName) (subject : String)
    (opts : List ParsedOption) :
    match consumeFlagList target subject opts with
    | Except.error _ => True
    | Except.ok (present, kept) =>
        kept.length ≤ opts.length ∧ (present = true → kept.length < opts.length) := by
  induction opts with
  | nil =>
      simp [consumeFlagList]
  | cons opt rest ih =>
      by_cases hName : opt.name = target
      · cases hInline : opt.inlineValue? with
        | some _ =>
            simp [consumeFlagList, hName, hInline]
        | none =>
            cases hRest : consumeFlagList target subject rest with
            | error err =>
                simp [consumeFlagList, hName, hInline, hRest]
            | ok result =>
                cases result with
                | mk presentRest keptRest =>
                    have ihRes := by
                      simpa [hRest] using ih
                    simp [consumeFlagList, hName, hInline, hRest]
                    have hLen := ihRes.left
                    have lengthBound := Nat.le_trans hLen (Nat.le_succ _)
                    have progressBound := Nat.lt_of_le_of_lt hLen (Nat.lt_succ_self _)
                    exact And.intro lengthBound progressBound
      · cases hRest : consumeFlagList target subject rest with
        | error err =>
            simp [consumeFlagList, hName, hRest]
        | ok result =>
            cases result with
            | mk presentRest keptRest =>
                have ihRes := by
                  simpa [hRest] using ih
                simp [consumeFlagList, hName, hRest]
                refine And.intro ?length ?progress
                · simpa using Nat.succ_le_succ ihRes.left
                · intro hPresent
                  have prog := ihRes.right hPresent
                  exact prog

theorem consumeFlagList_ok {target : ParsedName} {subject : String}
    {opts : List ParsedOption} {present : Bool} {kept : List ParsedOption}
    (h : consumeFlagList target subject opts = Except.ok (present, kept)) :
    kept.length ≤ opts.length ∧
      (present = true → kept.length < opts.length) := by
  have := consumeFlagList_ok_match target subject opts
  simpa [h] using this

theorem consumeFlagList_length_le {target : ParsedName} {subject : String}
    {opts : List ParsedOption} {present : Bool} {kept : List ParsedOption}
    (h : consumeFlagList target subject opts = Except.ok (present, kept)) :
    kept.length ≤ opts.length := (consumeFlagList_ok (target := target) (subject := subject) h).left

theorem consumeFlagList_present_lt {target : ParsedName} {subject : String}
    {opts : List ParsedOption} {kept : List ParsedOption}
    (h : consumeFlagList target subject opts = Except.ok (true, kept)) :
    kept.length < opts.length :=
  (consumeFlagList_ok (target := target) (subject := subject) (opts := opts) (present := true) h).right rfl

end TokenCursor

end Native
end Argparse

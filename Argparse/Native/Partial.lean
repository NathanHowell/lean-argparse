import Argparse.Native.Error

namespace Argparse
namespace Native

/--
`Assigned α` tracks whether a field in the partial parse has been hydrated. The
`.unset` constructor represents a field that has not been updated yet, while
`.value` stores the most recent value assigned to the field. This makes the
"last value wins" semantics explicit without conflating "unset" with a
potentially meaningful value such as `Option.none`.
-/
inductive Assigned (α : Type) where
  | unset : Assigned α
  | value (value : α) : Assigned α
  deriving Repr, DecidableEq

namespace Assigned

/-- Unset is the default inhabitant for any `Assigned` field. -/
instance instInhabited {α : Type} : Inhabited (Assigned α) := ⟨Assigned.unset⟩

@[simp] def pure {α : Type} (value : α) : Assigned α := .value value

/--
Extract the underlying value as an `Option`. Returns `none` when the field has
never been assigned.
-/
@[simp] def value? {α : Type} : Assigned α → Option α
  | .unset => none
  | .value val => some val

/-- True when the field has been assigned at least once. -/
@[simp] def isSet {α : Type} : Assigned α → Bool
  | .unset => false
  | .value _ => true

/--
Overwrite the stored value (if any) using `f`. When the field is unset the
function is not applied.
-/
@[simp] def map {α β : Type} (f : α → β) : Assigned α → Assigned β
  | .unset => .unset
  | .value val => .value (f val)

/--
Bind over the stored value, propagating `unset` unchanged.
-/
@[simp] def bind {α β : Type} : Assigned α → (α → Assigned β) → Assigned β
  | .unset, _ => .unset
  | .value val, f => f val

/-- Retrieve the stored value or return `default` if the field is unset. -/
@[simp] def getD {α : Type} (self : Assigned α) (default : α) : α :=
  match self with
  | .unset => default
  | .value val => val

/--
Turn an `Assigned` value into an `Except`, emitting `err` when the field has not
been assigned. This helper is used when required fields are finalised during the
completion pass.
-/
@[simp] def require {α : Type} (err : Error) : Assigned α → Except Error α
  | .unset => Except.error err
  | .value val => Except.ok val

/-- Reinterpret an `Option` as an `Assigned`. -/
@[simp] def ofOption {α : Type} : Option α → Assigned α
  | some val => .value val
  | none => .unset

/-- Convert an `Assigned` field back into an `Option`. -/
@[simp] def toOption {α : Type} (self : Assigned α) : Option α := self.value?

/-- `Assigned` behaves like an `Option` with respect to the standard type classes. -/
instance instFunctor : Functor Assigned where
  map := map

instance instPure : Pure Assigned := ⟨pure⟩

instance instSeq : Seq Assigned where
  seq
  | .value f, k =>
      match k () with
      | .value x => .value (f x)
      | .unset => .unset
  | .unset, _ => .unset

instance instSeqLeft : SeqLeft Assigned where
  seqLeft
  | .value x, k =>
      match k () with
      | .value _ => .value x
      | .unset => .unset
  | .unset, _ => .unset

instance instSeqRight : SeqRight Assigned where
  seqRight
  | .value _, k =>
      match k () with
      | .value y => .value y
      | .unset => .unset
  | .unset, _ => .unset

instance instApplicative : Applicative Assigned where
  pure := pure
  seq := Seq.seq
  map := Functor.map

instance instBind : Bind Assigned where
  bind := bind

instance instMonad : Monad Assigned :=
  { instApplicative with
      bind := bind
      map := Functor.map }

end Assigned

end Native
end Argparse

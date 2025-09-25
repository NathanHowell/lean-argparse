import Std

namespace Argparse

open Std

/-- Wrapper around a conversion from raw strings to typed values. -/
structure ValueReader (α : Type u) where
  /-- Attempt to decode the supplied string, returning either an error message or a value. -/
  run : String → Except String α

namespace ValueReader

/-- Reader that returns the original string unchanged. -/
def id : ValueReader String := ⟨fun s => .ok s⟩

/-- Compose a reader with an additional validation or transformation step. -/
def map {α β} (r : ValueReader α) (f : α → Except String β) : ValueReader β :=
  ⟨fun s => do
    let a ← r.run s
    f a⟩

/-- Reader for natural numbers. -/
def nat : ValueReader Nat := ⟨fun s =>
  match s.toNat? with
  | some n => .ok n
  | none => .error s!"expected a natural number, found '{s}'"⟩

/-- Reader for integers. -/
def int : ValueReader Int := ⟨fun s =>
  match s.toInt? with
  | some n => .ok n
  | none => .error s!"expected an integer, found '{s}'"⟩

end ValueReader

end Argparse

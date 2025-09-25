import Std

namespace LeanArgparse

open Std

structure ValueReader (α : Type u) where
  run : String → Except String α

namespace ValueReader

def id : ValueReader String := ⟨fun s => .ok s⟩

def map {α β} (r : ValueReader α) (f : α → Except String β) : ValueReader β :=
  ⟨fun s => do
    let a ← r.run s
    f a⟩

def nat : ValueReader Nat := ⟨fun s =>
  match s.toNat? with
  | some n => .ok n
  | none => .error s!"expected a natural number, found '{s}'"⟩

def int : ValueReader Int := ⟨fun s =>
  match s.toInt? with
  | some n => .ok n
  | none => .error s!"expected an integer, found '{s}'"⟩

end ValueReader

end LeanArgparse

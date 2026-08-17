import ArgParse.Core.Parser

/-!
# ArgParse.Proofs.Determinism

Parsers are pure functions from state to result, so determinism holds by
construction. Extensionality is the only consequence that is stated over the
parser type itself; the runner-level uniqueness lemmas that used to live here
were statements about the retired `Partial` front door and are superseded by
`ArgParse.Correspondence`.
-/

namespace ArgParse.Proofs

open ArgParse

/-- Two parsers are equal when they agree on every normalized state. -/
theorem parser_ext {α} {p q : Parser α}
    (h : ∀ st, p st = q st) : p = q :=
  funext h

/-- A parser's result at a state is unique: two successful readings of the same
run agree on both payload and final state. -/
theorem parser_ok_unique {α} {p : Parser α} {st : State}
    {a₁ a₂ : α} {st₁ st₂ : State}
    (h₁ : p st = .ok a₁ st₁) (h₂ : p st = .ok a₂ st₂) :
    a₁ = a₂ ∧ st₁ = st₂ := by
  rw [h₁] at h₂
  cases h₂
  exact ⟨rfl, rfl⟩

end ArgParse.Proofs

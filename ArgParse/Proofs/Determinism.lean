import ArgParse.Core.Parser
import ArgParse.Core.Runner

/-!
# ArgParse.Proofs.Determinism

Parsers are pure functions from state to result, so determinism holds by
construction. The lemmas here record the consequences a caller can rely on:
successful runner outcomes are unique, and parsing depends only on the
normalized token stream.
-/

namespace ArgParse.Proofs

open ArgParse
open ArgParse.Spec

/-- Two parsers are equal when they agree on every normalized state. -/
theorem parser_ext {α} {p q : Parser α}
    (h : ∀ st, p st = q st) : p = q :=
  funext h

/-- A successful raw run determines the payload and final state uniquely. -/
theorem runRaw_ok_unique
    (app : AppSpec) (tokens : Tokens)
    {p₁ p₂ : Spec.Partial} {st₁ st₂ : State}
    (h₁ : ArgParse.runRaw app tokens = RunOutcome.ok p₁ st₁)
    (h₂ : ArgParse.runRaw app tokens = RunOutcome.ok p₂ st₂) :
    p₁ = p₂ ∧ st₁ = st₂ := by
  have h := h₁.symm.trans h₂
  simpa [RunOutcome.ok] using h

/-- Folded runs likewise produce at most one payload/state pair. -/
theorem run_ok_unique {α}
    (app : AppSpec) (fold : Spec.Partial → α) (tokens : Tokens)
    {a₁ a₂ : α} {st₁ st₂ : State}
    (h₁ : ArgParse.run app fold tokens = RunOutcome.ok a₁ st₁)
    (h₂ : ArgParse.run app fold tokens = RunOutcome.ok a₂ st₂) :
    a₁ = a₂ ∧ st₁ = st₂ := by
  have h := h₁.symm.trans h₂
  simpa [RunOutcome.ok] using h

/-- Summary runs inherit uniqueness of successful outcomes. -/
theorem runSummary_ok_unique
    (app : AppSpec) (tokens : Tokens)
    {s₁ s₂ : Spec.Partial.Summary} {st₁ st₂ : State}
    (h₁ : ArgParse.runSummary app tokens = RunOutcome.ok s₁ st₁)
    (h₂ : ArgParse.runSummary app tokens = RunOutcome.ok s₂ st₂) :
    s₁ = s₂ ∧ st₁ = st₂ := by
  have h := h₁.symm.trans h₂
  simpa [RunOutcome.ok] using h

/-- Parsing depends only on the normalized token stream: argv lists that
normalize identically parse identically. -/
theorem runRaw_congr_normalize
    (app : AppSpec) {tokens₁ tokens₂ : Tokens}
    (h : Core.normalize tokens₁ = Core.normalize tokens₂) :
    ArgParse.runRaw app tokens₁ = ArgParse.runRaw app tokens₂ := by
  unfold ArgParse.runRaw
  rw [h]

/-- Summary parsing likewise depends only on the normalized token stream. -/
theorem runSummary_congr_normalize
    (app : AppSpec) {tokens₁ tokens₂ : Tokens}
    (h : Core.normalize tokens₁ = Core.normalize tokens₂) :
    ArgParse.runSummary app tokens₁ = ArgParse.runSummary app tokens₂ := by
  unfold ArgParse.runSummary
  rw [runRaw_congr_normalize app h]

end ArgParse.Proofs

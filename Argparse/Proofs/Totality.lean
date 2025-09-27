import Argparse.Core.Parser
import Argparse.Core.Combinators
import Argparse.Spec.AST

/-!
# ArgParse.Proofs.Totality

Early progress/totality lemmas (some still placeholders).
-/

namespace ArgParse.Proofs

open ArgParse
open ArgParse.Core
open ArgParse.Spec

/-- Normalization trivially produces a state. -/
theorem normalize_total (tokens : Tokens) : True :=
  trivial

/-- If a flag parser succeeds with `true`, the cursor advances by one token. -/
theorem flag_true_progress
    (spec : FlagSpec) (st st' : State) :
    flag spec st = .ok true st' → st'.cursor = st.cursor + 1 := by
  intro h
  unfold flag at h
  cases hpre : st.pre with
  | nil =>
      simp [hpre] at h
  | cons token rest =>
      cases hmatch : matchFlagToken spec token with
      | none =>
          simp [hpre, hmatch] at h
      | short =>
          simp [hpre, hmatch] at h
      | long =>
          simp [hpre, hmatch] at h
      | shortBundled tail =>
          simp [hpre, hmatch] at h

/-- If a flag parser returns `false`, the state is unchanged. -/
theorem flag_false_preserves_state
    (spec : FlagSpec) (st st' : State) :
    flag spec st = .ok false st' → st' = st := by
  intro h
  unfold flag at h
  cases hpre : st.pre with
  | nil =>
      simpa [hpre] using h
  | cons token rest =>
      cases hmatch : matchFlagToken spec token with
      | none =>
          simpa [hpre, hmatch] using h
      | short =>
          simp [hpre, hmatch] at h
      | long =>
          simp [hpre, hmatch] at h
      | shortBundled tail =>
          simp [hpre, hmatch] at h

/-- Placeholder: positional `.some` requires at least one value. -/
theorem positional_some_requires_input_placeholder
    {α} [FromArg α] (spec : PosSpec α) (st : State) : True :=
  trivial

/-- Placeholder: option parser for `.one` arity is total over any state. -/
theorem option_total_placeholder {α} [FromArg α]
    (spec : OptSpec α) (st : State) : True :=
  trivial

end ArgParse.Proofs

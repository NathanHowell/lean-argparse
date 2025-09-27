import Argparse.Core.Parser
import Argparse.Core.Combinators
import Argparse.Spec.AST

/-!
# ArgParse.Proofs.Totality

Early progress/totality lemmas accompanying the new combinators.
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

/-- Successful concatenated short-option parsing consumes exactly one token. -/
lemma parseConcatValue_cursor
    {α} [FromArg α] (spec : OptSpec α) (token raw : String)
    (pending : List String) (st : State) (expect : Expect)
    (value : α) (st' : State) :
    parseConcatValue spec token raw pending st expect = .ok (some value, st') →
    st'.cursor = st.cursor + 1 := by
  intro h
  unfold parseConcatValue at h
  split at h with hraw : raw = ""
  · simp [hraw] at h
  · simp [hraw] at h
    cases hrun : FromArg.run raw with
    | ok =>
        simp [hrun] at h
        cases h
        rfl
    | error msg =>
        cases hsplit : findConcatSplit? raw with
        | none => simp [hrun, hsplit] at h
        | some pair =>
            cases pair with
            | intro value' remainder =>
                simp [hrun, hsplit] at h
                cases h
                simp [Nat.succ_eq_add_one]

/-- `takePositionalStep?` consumes exactly one token whenever it succeeds. -/
lemma takePositionalStep_some_progress
    {α} [FromArg α] (spec : PosSpec α) (st st' : State)
    (value : α) (c : Nat) :
    takePositionalStep? spec st = .ok { value? := some value, state := st', consumed := c } →
    c = 1 ∧ st'.cursor = st.cursor + 1 := by
  intro h
  unfold takePositionalStep? at h
  classical
  have expect := expectPositional spec
  cases hpreList : st.pre with
  | nil =>
      have hPre : State.consumePre? st = none := by
        simp [State.consumePre?, hpreList]
      simp [hPre] at h
      cases hpostList : st.post with
      | nil =>
          have hPost : State.consumePost? st = none := by
            simp [State.consumePost?, hpostList]
          simp [hPost] at h
      | cons postHead postTail =>
          have hPost : State.consumePost? st =
              some (postHead, { st with post := postTail, cursor := st.cursor + 1 }) := by
            simp [State.consumePost?, hpostList]
          simp [hPost] at h
          cases hrun : FromArg.run postHead with
          | ok parsed =>
              simp [hrun] at h
              cases h
              constructor
              · rfl
              · simp [State.consumePost?, hpostList]
          | error msg => simp [hrun] at h
  | cons head tail =>
      have hPre : State.consumePre? st =
          some (head, { st with pre := tail, cursor := st.cursor + 1 }) := by
        simp [State.consumePre?, hpreList]
      simp [hPre] at h
      cases hrun : FromArg.run head with
      | ok parsed =>
          simp [hrun] at h
          cases h
          constructor
          · rfl
          · simp [State.consumePre?, hpreList]
      | error msg => simp [hrun] at h

/-- `takePositionalValue?` advances the cursor on success. -/
theorem takePositionalValue_some_progress
    {α} [FromArg α] (spec : PosSpec α) (st st' : State)
    (value : α) :
    takePositionalValue? spec st = .ok (some value, st') →
    st'.cursor = st.cursor + 1 := by
  intro h
  unfold takePositionalValue? at h
  classical
  cases hstep : takePositionalStep? spec st with
  | error err => simp [hstep] at h
  | ok step =>
      simp [hstep] at h
      rcases step with ⟨value?, state', consumed⟩
      cases value? with
      | none => simp at h
      | some valueStep =>
          simp at h
          intro hstate
          cases hstate
          have hprogress := takePositionalStep_some_progress
            (spec := spec) (st := st) (st' := state')
            (value := value) (c := consumed) hstep
          exact hprogress.2

/-- `takePositionalStep?` consumes exactly one token whenever it succeeds. -/
lemma takePositionalStep_some_progress
    {α} [FromArg α] (spec : PosSpec α) (st st' : State)
    (value : α) (c : Nat) :
    takePositionalStep? spec st = .ok { value? := some value, state := st', consumed := c } →
    c = 1 ∧ st'.cursor = st.cursor + 1 := by
  intro h
  unfold takePositionalStep? at h
  classical
  have expect := expectPositional spec
  cases hpreList : st.pre with
  | nil =>
      have hPre : State.consumePre? st = none := by
        simp [State.consumePre?, hpreList]
      simp [hPre] at h
      cases hpostList : st.post with
      | nil =>
          have hPost : State.consumePost? st = none := by
            simp [State.consumePost?, hpostList]
          simp [hPost] at h
      | cons postHead postTail =>
          have hPost : State.consumePost? st =
              some (postHead, { st with post := postTail, cursor := st.cursor + 1 }) := by
            simp [State.consumePost?, hpostList]
          simp [hPost] at h
          cases hrun : FromArg.run postHead with
          | ok parsed =>
              simp [hrun] at h
              cases h with
              | intro hv hs =>
                  cases hv; cases hs
                  constructor
                  · rfl
                  · simp [State.consumePost?, hpostList]
          | error msg => simp [hrun] at h
  | cons head tail =>
      have hPre : State.consumePre? st =
          some (head, { st with pre := tail, cursor := st.cursor + 1 }) := by
        simp [State.consumePre?, hpreList]
      simp [hPre] at h
      cases hrun : FromArg.run head with
      | ok parsed =>
          simp [hrun] at h
          cases h with
          | intro hv hs =>
              cases hv; cases hs
              constructor
              · rfl
              · simp [State.consumePre?, hpreList]
      | error msg => simp [hrun] at h

/-- `positional` with arity `.some` never returns an empty list. -/
theorem positional_some_nonempty
    {α} [FromArg α] (spec : PosSpec α) (st st' : State)
    (values : List α) :
    spec.arity = .some → positional spec st = .ok values st' → values ≠ [] := by
  intro harity hres
  subst harity
  unfold positional at hres
  cases hcollect : collectPositionalValues spec st with
  | error err =>
      simp [hcollect] at hres
  | ok result =>
      cases result with
      | intro vs stAfter =>
          cases vs with
          | nil =>
              simp [hcollect] at hres
          | cons x xs =>
              simp [hcollect] at hres
              cases hres
              intro hnil
              cases hnil

/-- `option` with arity `.some` never returns an empty list. -/
theorem option_some_nonempty
    {α} [FromArg α] (spec : OptSpec α) (st st' : State)
    (values : List α) :
    spec.arity = .some → option spec st = .ok values st' → values ≠ [] := by
  intro harity hres
  subst harity
  unfold option at hres
  cases hcollect : collectOptionValues spec st with
  | error err =>
      simp [hcollect] at hres
  | ok result =>
      cases result with
      | intro vs stAfter =>
          cases vs with
          | nil =>
              simp [hcollect] at hres
          | cons x xs =>
              simp [hcollect] at hres
              cases hres
              intro hnil
              cases hnil

end ArgParse.Proofs

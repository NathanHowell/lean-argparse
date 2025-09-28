import Argparse.Core.Parser
import Argparse.Core.Combinators
import Argparse.Spec.AST
import Argparse.Spec.Elab

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
        simp [Nat.succ_eq_add_one]
    | error msg =>
        cases hsplit : findConcatSplit? raw with
        | none => simp [hrun, hsplit] at h
        | some pair =>
            cases pair with
            | intro value' remainder =>
                simp [hrun, hsplit] at h
                cases h
                simp [Nat.succ_eq_add_one]

/-- When concatenation splitting succeeds, the leftover bundle is pushed back to `pre`. -/
lemma parseConcatValue_split_state
    {α} [FromArg α] (spec : OptSpec α) (token raw : String)
    (pending : List String) (st : State) (expect : Expect)
    (msg : String) (value : α) (st' : State) :
    raw ≠ "" →
    FromArg.run raw = .error msg →
    parseConcatValue spec token raw pending st expect = .ok (some value, st') →
    ∃ remainder,
      findConcatSplit? raw = some (value, remainder) ∧
      st' = { st with pre := ("-" ++ remainder) :: pending, cursor := st.cursor + 1 } := by
  intro hRaw hRun h
  classical
  unfold parseConcatValue at h
  simp [hRaw, hRun] at h
  cases hsplit : findConcatSplit? raw with
  | none => simp [hsplit] at h
  | some pair =>
      rcases pair with ⟨value', remainder⟩
      simp [hsplit] at h
      rcases h with ⟨rfl, rfl⟩
      exact ⟨remainder, ⟨rfl, rfl⟩⟩

/-- `takeOptionStep?` succeeds only after consuming one or two tokens. -/
lemma takeOptionStep_some_progress
    {α} [FromArg α] (spec : OptSpec α)
    {st st' : State} {value : α} {c : Nat} :
    takeOptionStep? spec st = .ok { value? := some value, state := st', consumed := c } →
    (c = 1 ∧ st'.cursor = st.cursor + 1) ∨ (c = 2 ∧ st'.cursor = st.cursor + 2) := by
  classical
  intro h
  unfold takeOptionStep? at h
  cases hpre : st.pre with
  | nil => simp [hpre] at h
  | cons token rest =>
      set expect := expectOption spec
      cases hlong : spec.long? with
      | none =>
          cases hshort : spec.short? with
          | none => simp [hpre, hlong, hshort] at h
          | some short =>
              set prefix := shortLexeme short
              by_cases htok : token = prefix
              · subst htok
                cases rest with
                | nil => simp [hpre, hlong, hshort] at h
                | cons valueTok restTail =>
                    cases hrun : FromArg.run valueTok with
                    | ok parsed =>
                        simp [hpre, hlong, hshort, hrun] at h
                        rcases h with ⟨hv, hs, hc⟩
                        cases hv; cases hs; cases hc
                        exact Or.inr ⟨rfl, by simp [State.consumePre?, hpre, Nat.add_comm, Nat.add_left_comm, Nat.add_assoc]⟩
                    | error msg => simp [hpre, hlong, hshort, hrun] at h
              · have : token ≠ prefix := htok
                by_cases hconcat : spec.concatVal? ∧ token.startsWith prefix
                · simp [hpre, hlong, hshort, this, hconcat] at h
                  rcases h with ⟨hv, hs, hc⟩
                  cases hv; cases hs; cases hc
                  exact Or.inl ⟨rfl,
                    parseConcatValue_cursor (spec := spec) (token := token)
                      (raw := token.drop prefix.length) (pending := rest)
                      (st := st) (expect := expect) (value := value) rfl⟩
                · simp [hpre, hlong, hshort, this, hconcat] at h
  | some name =>
      set prefix := longLexeme name
      set eqPrefix := prefix ++ "="
      by_cases hconcat : spec.eqVal? ∧ token.startsWith eqPrefix
      · simp [hpre, hlong, hconcat] at h
        rcases h with ⟨hv, hs, hc⟩
        cases hv; cases hs; cases hc
        exact Or.inl ⟨rfl,
          parseConcatValue_cursor (spec := spec) (token := token)
            (raw := token.drop eqPrefix.length) (pending := rest)
            (st := st) (expect := expect) (value := value) rfl⟩
          · have : ¬ (spec.eqVal? ∧ token.startsWith eqPrefix) := hconcat
            by_cases hprefix : token = prefix
            · subst hprefix
              simp [hpre, hlong, hconcat] at h
              cases rest with
              | nil => simp at h
              | cons valueTok restTail =>
                  cases hrun : FromArg.run valueTok with
                  | ok parsed =>
                      simp [hrun] at h
                      rcases h with ⟨hv, hs, hc⟩
                      cases hv; cases hs; cases hc
                      exact Or.inr ⟨rfl, by simp [State.consumePre?, hpre, Nat.add_comm, Nat.add_left_comm, Nat.add_assoc]⟩
                  | error msg => simp [hrun] at h
            · have : token ≠ prefix := hprefix
              simp [hpre, hlong, hconcat, this] at h
              cases hshort : spec.short? with
              | none => simp [hshort] at h
              | some short =>
                  set prefixShort := shortLexeme short
                  by_cases htok : token = prefixShort
                  · subst htok
                    simp [hshort] at h
                    cases rest with
                    | nil => simp at h
                    | cons valueTok restTail =>
                        cases hrun : FromArg.run valueTok with
                        | ok parsed =>
                            simp [hrun] at h
                            rcases h with ⟨hv, hs, hc⟩
                            cases hv; cases hs; cases hc
                            exact Or.inr ⟨rfl, by simp [State.consumePre?, hpre, Nat.add_comm, Nat.add_left_comm, Nat.add_assoc]⟩
                        | error msg => simp [hrun] at h
                  · have : token ≠ prefixShort := htok
                    by_cases hconcatShort : spec.concatVal? ∧ token.startsWith prefixShort
                    · simp [hshort, this, hconcatShort] at h
                    · simp [hshort, this, hconcatShort] at h

/-- Aggregates cursor progress across the option collector loop. -/
lemma collectOptionStepsLoop_progress
    {α} [FromArg α] (spec : OptSpec α) :
    ∀ fuel acc consumed st (result : CollectResult α),
      collectOptionStepsLoop (spec := spec) fuel acc consumed st = .ok result →
      ∃ δ,
        result.consumed = consumed + δ ∧
        result.state.cursor = st.cursor + δ := by
  classical
  intro fuel
  induction fuel with
  | zero =>
      intro acc consumed st result h
      simp [collectOptionStepsLoop] at h
      cases h
      refine ⟨0, ?_, ?_⟩
      · simp
      · simp
  | succ fuel ih =>
      intro acc consumed st result h
      simp [collectOptionStepsLoop] at h
      cases hstep : takeOptionStep? spec st with
      | error err =>
          simp [hstep] at h
      | ok step =>
          cases step with
          | mk value? state consumedStep =>
              cases hvalue : value? with
              | none =>
                  simp [hstep, hvalue] at h
                  cases h
                  refine ⟨0, ?_, ?_⟩
                  · simp
                  · simp
              | some value =>
                  have hrecEq :
                      collectOptionStepsLoop (spec := spec) fuel (value :: acc)
                        (consumed + consumedStep) state = .ok result := by
                    simpa [hstep, hvalue] using h
                  have hrec := ih (acc := value :: acc)
                    (consumed := consumed + consumedStep)
                    (st := state) (result := result) hrecEq
                  rcases hrec with ⟨δ, hcons, hcursor⟩
                  have hstepEq :
                      takeOptionStep? spec st =
                        .ok { value? := some value, state := state, consumed := consumedStep } := by
                    simpa [hstep, hvalue]
                  have hprogress := takeOptionStep_some_progress
                    (spec := spec) (st := st) (st' := state)
                    (value := value) (c := consumedStep) hstepEq
                  have hstepCursor :
                      state.cursor = st.cursor + consumedStep := by
                    cases hprogress with
                    | inl h1 =>
                        rcases h1 with ⟨hconsumed, hcursorStep⟩
                        simp [hconsumed, hcursorStep]
                    | inr h2 =>
                        rcases h2 with ⟨hconsumed, hcursorStep⟩
                        simp [hconsumed, hcursorStep]
                  refine ⟨consumedStep + δ, ?_, ?_⟩
                  · simp [hcons, Nat.add_comm, Nat.add_left_comm, Nat.add_assoc]
                  · simpa [hcursor, hstepCursor, Nat.add_comm, Nat.add_left_comm, Nat.add_assoc]

/-- Cursor delta for the option collector starting from an empty accumulator. -/
theorem collectOptionSteps_progress
    {α} [FromArg α] (spec : OptSpec α) (st : State) (result : CollectResult α) :
    collectOptionSteps (spec := spec) st = .ok result →
    result.state.cursor = st.cursor + result.consumed := by
  intro h
  unfold collectOptionSteps at h
  classical
  have hloop := collectOptionStepsLoop_progress (spec := spec)
    (fuel := st.pre.length + st.post.length + 1)
    (acc := []) (consumed := 0) (st := st) (result := result) h
  rcases hloop with ⟨δ, hconsumed, hcursor⟩
  have : result.consumed = δ := by simpa using hconsumed
  simpa [this]

/-- Cursor delta for `collectOptionValues`. -/
lemma collectOptionValues_progress
    {α} [FromArg α] (spec : OptSpec α) (st st' : State) (values : List α) :
    collectOptionValues (spec := spec) st = .ok (values, st') →
    ∃ consumed, st'.cursor = st.cursor + consumed := by
  intro h
  unfold collectOptionValues at h
  classical
  cases hcollect : collectOptionSteps spec st with
  | error err => simp [hcollect] at h
  | ok result =>
      simp [hcollect] at h
      have hpair : (values, st') = (result.values, result.state) := by
        simpa using h
      cases hpair
      refine ⟨result.consumed, ?_⟩
      simpa using collectOptionSteps_progress (spec := spec)
        (st := st) (result := result) hcollect

/-- `takeOptionValue?` advances the cursor by one or two tokens on success. -/
lemma takeOptionValue_none_preserves_state
    {α} [FromArg α] (spec : OptSpec α) (st st' : State) :
    takeOptionValue? spec st = .ok (none, st') → st' = st := by
  intro h
  unfold takeOptionValue? at h
  classical
  cases hstep : takeOptionStep? spec st with
  | error err => simp [hstep] at h
  | ok step =>
      rcases step with ⟨value?, state', consumed⟩
      cases hvalue : value? with
      | none =>
          simp [hstep, hvalue] at h
          cases h with
          | intro _ hstate =>
              simpa using hstate
      | some value =>
          simp [hstep, hvalue] at h

theorem takeOptionValue_some_progress
    {α} [FromArg α] (spec : OptSpec α) (st st' : State) (value : α) :
    takeOptionValue? spec st = .ok (some value, st') →
    st'.cursor = st.cursor + 1 ∨ st'.cursor = st.cursor + 2 := by
  intro h
  unfold takeOptionValue? at h
  classical
  cases hstep : takeOptionStep? spec st with
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
          have hprogress := takeOptionStep_some_progress
            (spec := spec) (st := st) (st' := state')
            (value := value) (c := consumed) hstep
          cases hprogress with
          | inl h1 => exact Or.inl h1.2
          | inr h2 => exact Or.inr h2.2

/-- Optional option success moves the cursor when a value is present. -/
theorem option_one_some_progress
    {α} [FromArg α] (spec : OptSpec α) (st st' : State) (value : α) :
    spec.arity = .one →
    option spec st = .ok (some value) st' →
    st'.cursor = st.cursor + 1 ∨ st'.cursor = st.cursor + 2 := by
  intro harity hres
  subst harity
  unfold option at hres
  classical
  cases htake : takeOptionValue? spec st with
  | error err => simp [htake] at hres
  | ok result =>
      rcases result with ⟨value?, stAfter⟩
      cases hvalue : value? with
      | none => simp [htake, hvalue] at hres
      | some parsed =>
          have hpair : (some value, st') = (some parsed, stAfter) := by
            simpa [htake, hvalue] using hres
          cases hpair with
          | intro hval hstate =>
              cases hval
              exact takeOptionValue_some_progress
                (spec := spec) (st := st) (st' := st') (value := value)
                (by simpa [htake, hvalue])

/-- Optional option absence leaves the state untouched. -/
theorem option_one_none_preserves_state
    {α} [FromArg α] (spec : OptSpec α) (st st' : State) :
    spec.arity = .one →
    option spec st = .ok none st' → st' = st := by
  intro harity hres
  subst harity
  unfold option at hres
  classical
  cases htake : takeOptionValue? spec st with
  | error err => simp [htake] at hres
  | ok result =>
      rcases result with ⟨value?, stAfter⟩
      cases hvalue : value? with
      | none =>
          have hpair : (none, st') = (none, stAfter) := by
            simpa [htake, hvalue] using hres
          cases hpair with
          | intro _ hstate =>
              have hstate' := takeOptionValue_none_preserves_state
                (spec := spec) (st := st) (st' := stAfter)
                (by simpa [htake, hvalue])
              simpa [hstate'] using hstate
      | some parsed => simp [htake, hvalue] at hres

/-- Successful `.many` option parsing advances by the collector delta. -/
theorem option_many_progress
    {α} [FromArg α] (spec : OptSpec α) (st st' : State)
    (values : List α) :
    spec.arity = .many →
    option spec st = .ok values st' →
    ∃ consumed, st'.cursor = st.cursor + consumed := by
  intro harity hres
  subst harity
  unfold option at hres
  classical
  cases hcollect : collectOptionValues spec st with
  | error err => simp [hcollect] at hres
  | ok result =>
      have hvalues : collectOptionValues spec st = .ok (values, st') := by
        simpa [hcollect] using hres
      exact collectOptionValues_progress (spec := spec)
        (st := st) (st' := st') (values := values) hvalues

/-- Successful `.some` option parsing advances by the collector delta. -/
theorem option_some_progress
    {α} [FromArg α] (spec : OptSpec α) (st st' : State)
    (values : List α) :
    spec.arity = .some →
    option spec st = .ok values st' →
    ∃ consumed, st'.cursor = st.cursor + consumed := by
  intro harity hres
  subst harity
  unfold option at hres
  classical
  cases hcollect : collectOptionValues spec st with
  | error err => simp [hcollect] at hres
  | ok result =>
      rcases result with ⟨valuesRaw, stRaw⟩
      cases hlist : valuesRaw with
      | nil => simp [hcollect, hlist] at hres
      | cons head tail =>
          have hpair : (values, st') = (head :: tail, stRaw) := by
            simpa [hcollect, hlist] using hres
          cases hpair
          have hvalues : collectOptionValues spec st = .ok (values, st') := by
            simpa [hcollect, hlist]
          exact collectOptionValues_progress (spec := spec)
            (st := st) (st' := st') (values := values) hvalues

/-- Elaboration transformer for options preserves the cursor facts from `option`. -/
theorem interpretOption_one_progress
    {α} [FromArg α] (spec : OptSpec α) (st st' : State)
    (updater : Spec.Partial → Spec.Partial) :
    spec.arity = .one →
    interpretOption (spec := spec) st = .ok updater st' →
    st' = st ∨ st'.cursor = st.cursor + 1 ∨ st'.cursor = st.cursor + 2 := by
  intro harity hres
  subst harity
  classical
  unfold Spec.interpretOption at hres
  simp [Parser.map] at hres
  cases hop : option spec st with
  | err => simp [hop] at hres
  | ok result =>
      rcases result with ⟨value?, stAfter⟩
      simp [hop] at hres
      cases hres
      cases hvalue : value? with
      | none =>
          have hstate := option_one_none_preserves_state
            (spec := spec) (st := st) (st' := stAfter) rfl
            (by simpa [hop, hvalue])
          exact Or.inl hstate
      | some value =>
          have hprog := option_one_some_progress
            (spec := spec) (st := st) (st' := stAfter) (value := value) rfl
            (by simpa [hop, hvalue])
          cases hprog with
          | inl h1 => exact Or.inr (Or.inl h1)
          | inr h2 => exact Or.inr (Or.inr h2)

/-- Elaboration transformer for options with `.many` arity inherits cursor deltas. -/
theorem interpretOption_many_progress
    {α} [FromArg α] (spec : OptSpec α) (st st' : State)
    (updater : Spec.Partial → Spec.Partial) :
    spec.arity = .many →
    interpretOption (spec := spec) st = .ok updater st' →
    ∃ consumed, st'.cursor = st.cursor + consumed := by
  intro harity hres
  subst harity
  classical
  unfold Spec.interpretOption at hres
  simp [Parser.map] at hres
  cases hop : option spec st with
  | err => simp [hop] at hres
  | ok result =>
      rcases result with ⟨values, stAfter⟩
      simp [hop] at hres
      cases hres
      exact option_many_progress (spec := spec) (st := st) (st' := stAfter)
        (values := values) rfl (by simpa [hop])

/-- Elaboration transformer for options with `.some` arity inherits cursor deltas. -/
theorem interpretOption_some_progress
    {α} [FromArg α] (spec : OptSpec α) (st st' : State)
    (updater : Spec.Partial → Spec.Partial) :
    spec.arity = .some →
    interpretOption (spec := spec) st = .ok updater st' →
    ∃ consumed, st'.cursor = st.cursor + consumed := by
  intro harity hres
  subst harity
  classical
  unfold Spec.interpretOption at hres
  simp [Parser.map] at hres
  cases hop : option spec st with
  | err => simp [hop] at hres
  | ok result =>
      rcases result with ⟨values, stAfter⟩
      simp [hop] at hres
      cases hres
      exact option_some_progress (spec := spec) (st := st) (st' := stAfter)
        (values := values) rfl (by simpa [hop])

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
lemma takePositionalValue_none_preserves_state
    {α} [FromArg α] (spec : PosSpec α) (st st' : State) :
    takePositionalValue? spec st = .ok (none, st') → st' = st := by
  intro h
  unfold takePositionalValue? at h
  classical
  cases hstep : takePositionalStep? spec st with
  | error err => simp [hstep] at h
  | ok step =>
      rcases step with ⟨value?, state', consumed⟩
      cases hvalue : value? with
      | none =>
          simp [hstep, hvalue] at h
          cases h with
          | intro _ hstate =>
              simpa using hstate
      | some value =>
          simp [hstep, hvalue] at h

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

/-- Optional positional success advances the cursor. -/
theorem positional_one_some_progress
    {α} [FromArg α] (spec : PosSpec α) (st st' : State) (value : α) :
    spec.arity = .one →
    positional spec st = .ok (some value) st' →
    st'.cursor = st.cursor + 1 := by
  intro harity hres
  subst harity
  unfold positional at hres
  classical
  cases htake : takePositionalValue? spec st with
  | error err => simp [htake] at hres
  | ok result =>
      rcases result with ⟨value?, stAfter⟩
      cases hvalue : value? with
      | none => simp [htake, hvalue] at hres
      | some parsed =>
          have hpair : (some value, st') = (some parsed, stAfter) := by
            simpa [htake, hvalue] using hres
          cases hpair with
          | intro hval hstate =>
              cases hval
              exact takePositionalValue_some_progress
                (spec := spec) (st := st) (st' := st') (value := value)
                (by simpa [htake, hvalue])

/-- Optional positional absence leaves the state untouched. -/
theorem positional_one_none_preserves_state
    {α} [FromArg α] (spec : PosSpec α) (st st' : State) :
    spec.arity = .one →
    positional spec st = .ok none st' → st' = st := by
  intro harity hres
  subst harity
  unfold positional at hres
  classical
  cases htake : takePositionalValue? spec st with
  | error err => simp [htake] at hres
  | ok result =>
      rcases result with ⟨value?, stAfter⟩
      cases hvalue : value? with
      | none =>
          have hpair : (none, st') = (none, stAfter) := by
            simpa [htake, hvalue] using hres
          cases hpair with
          | intro _ hstate =>
              have hstate' := takePositionalValue_none_preserves_state
                (spec := spec) (st := st) (st' := stAfter)
                (by simpa [htake, hvalue])
              simpa [hstate'] using hstate
      | some parsed => simp [htake, hvalue] at hres

/-- Elaboration transformer for positionals preserves optional cursor facts. -/
theorem interpretPositional_one_progress
    {α} [FromArg α] (spec : PosSpec α) (st st' : State)
    (updater : Spec.Partial → Spec.Partial) :
    spec.arity = .one →
    interpretPositional (spec := spec) st = .ok updater st' →
    st' = st ∨ st'.cursor = st.cursor + 1 := by
  intro harity hres
  subst harity
  classical
  unfold Spec.interpretPositional at hres
  simp [Parser.map] at hres
  cases hop : positional spec st with
  | err => simp [hop] at hres
  | ok result =>
      rcases result with ⟨value?, stAfter⟩
      simp [hop] at hres
      cases hres
      cases hvalue : value? with
      | none =>
          have hstate := positional_one_none_preserves_state
            (spec := spec) (st := st) (st' := stAfter) rfl
            (by simpa [hop, hvalue])
          exact Or.inl hstate
      | some value =>
          have hprog := positional_one_some_progress
            (spec := spec) (st := st) (st' := stAfter) (value := value) rfl
            (by simpa [hop, hvalue])
          exact Or.inr hprog

/-- Elaboration transformer for positionals with `.many` arity inherits cursor deltas. -/
theorem interpretPositional_many_progress
    {α} [FromArg α] (spec : PosSpec α) (st st' : State)
    (updater : Spec.Partial → Spec.Partial) :
    spec.arity = .many →
    interpretPositional (spec := spec) st = .ok updater st' →
    ∃ consumed, st'.cursor = st.cursor + consumed := by
  intro harity hres
  subst harity
  classical
  unfold Spec.interpretPositional at hres
  simp [Parser.map] at hres
  cases hop : positional spec st with
  | err => simp [hop] at hres
  | ok result =>
      rcases result with ⟨values, stAfter⟩
      simp [hop] at hres
      cases hres
      exact positional_many_progress (spec := spec) (st := st) (st' := stAfter)
        (values := values) rfl (by simpa [hop])

/-- Elaboration transformer for positionals with `.some` arity inherits cursor deltas. -/
theorem interpretPositional_some_progress
    {α} [FromArg α] (spec : PosSpec α) (st st' : State)
    (updater : Spec.Partial → Spec.Partial) :
    spec.arity = .some →
    interpretPositional (spec := spec) st = .ok updater st' →
    ∃ consumed, st'.cursor = st.cursor + consumed := by
  intro harity hres
  subst harity
  classical
  unfold Spec.interpretPositional at hres
  simp [Parser.map] at hres
  cases hop : positional spec st with
  | err => simp [hop] at hres
  | ok result =>
      rcases result with ⟨values, stAfter⟩
      simp [hop] at hres
      cases hres
      exact positional_some_progress (spec := spec) (st := st) (st' := stAfter)
        (values := values) rfl (by simpa [hop])

lemma collectPositionalStepsLoop_progress
    {α} [FromArg α] (spec : PosSpec α) :
    ∀ fuel acc consumed st (result : CollectResult α),
      collectPositionalStepsLoop (spec := spec) fuel acc consumed st = .ok result →
      ∃ δ,
        result.consumed = consumed + δ ∧
        result.state.cursor = st.cursor + δ := by
  classical
  intro fuel
  induction fuel with
  | zero =>
      intro acc consumed st result h
      simp [collectPositionalStepsLoop] at h
      cases h
      refine ⟨0, ?_, ?_⟩
      · simp
      · simp
  | succ fuel ih =>
      intro acc consumed st result h
      simp [collectPositionalStepsLoop] at h
      cases hstep : takePositionalStep? spec st with
      | error err =>
          simp [hstep] at h
      | ok step =>
          cases step with
          | mk value? state consumedStep =>
              cases hvalue : value? with
              | none =>
                  simp [hstep, hvalue] at h
                  cases h
                  refine ⟨0, ?_, ?_⟩
                  · simp
                  · simp
              | some value =>
                  have hrecEq :
                      collectPositionalStepsLoop (spec := spec) fuel (value :: acc)
                        (consumed + consumedStep) state = .ok result := by
                    simpa [hstep, hvalue] using h
                  have hrec := ih (acc := value :: acc)
                    (consumed := consumed + consumedStep)
                    (st := state) (result := result) hrecEq
                  rcases hrec with ⟨δ, hcons, hcursor⟩
                  have hstepEq :
                      takePositionalStep? spec st =
                        .ok { value? := some value, state := state, consumed := consumedStep } := by
                    simpa [hstep, hvalue]
                  have hprogress := takePositionalStep_some_progress
                    (spec := spec) (st := st) (st' := state)
                    (value := value) (c := consumedStep) hstepEq
                  have hstepCursor :
                      state.cursor = st.cursor + consumedStep := by
                    rcases hprogress with ⟨hconsumed, hcursorStep⟩
                    simp [hconsumed, hcursorStep]
                  refine ⟨consumedStep + δ, ?_, ?_⟩
                  · simp [hcons, Nat.add_comm, Nat.add_left_comm, Nat.add_assoc]
                  · simpa [hcursor, hstepCursor, Nat.add_comm, Nat.add_left_comm, Nat.add_assoc]

/-- Cursor delta for positional collectors. -/
theorem collectPositionalSteps_progress
    {α} [FromArg α] (spec : PosSpec α) (st : State) (result : CollectResult α) :
    collectPositionalSteps (spec := spec) st = .ok result →
    result.state.cursor = st.cursor + result.consumed := by
  intro h
  unfold collectPositionalSteps at h
  classical
  have hloop := collectPositionalStepsLoop_progress (spec := spec)
    (fuel := st.pre.length + st.post.length + 1)
    (acc := []) (consumed := 0) (st := st) (result := result) h
  rcases hloop with ⟨δ, hconsumed, hcursor⟩
  have : result.consumed = δ := by simpa using hconsumed
  simpa [this]

/-- Cursor delta for `collectPositionalValues`. -/
lemma collectPositionalValues_progress
    {α} [FromArg α] (spec : PosSpec α) (st st' : State) (values : List α) :
    collectPositionalValues (spec := spec) st = .ok (values, st') →
    ∃ consumed, st'.cursor = st.cursor + consumed := by
  intro h
  unfold collectPositionalValues at h
  classical
  cases hcollect : collectPositionalSteps spec st with
  | error err => simp [hcollect] at h
  | ok result =>
      simp [hcollect] at h
      have hpair : (values, st') = (result.values, result.state) := by
        simpa using h
      cases hpair
      refine ⟨result.consumed, ?_⟩
      simpa using collectPositionalSteps_progress (spec := spec)
        (st := st) (result := result) hcollect

/-- Successful `.many` positional parsing advances by the collector delta. -/
theorem positional_many_progress
    {α} [FromArg α] (spec : PosSpec α) (st st' : State)
    (values : List α) :
    spec.arity = .many →
    positional spec st = .ok values st' →
    ∃ consumed, st'.cursor = st.cursor + consumed := by
  intro harity hres
  subst harity
  unfold positional at hres
  classical
  cases hcollect : collectPositionalValues spec st with
  | error err => simp [hcollect] at hres
  | ok result =>
      have hvalues : collectPositionalValues spec st = .ok (values, st') := by
        simpa [hcollect] using hres
      exact collectPositionalValues_progress (spec := spec)
        (st := st) (st' := st') (values := values) hvalues

/-- Successful `.some` positional parsing advances by the collector delta. -/
theorem positional_some_progress
    {α} [FromArg α] (spec : PosSpec α) (st st' : State)
    (values : List α) :
    spec.arity = .some →
    positional spec st = .ok values st' →
    ∃ consumed, st'.cursor = st.cursor + consumed := by
  intro harity hres
  subst harity
  unfold positional at hres
  classical
  cases hcollect : collectPositionalValues spec st with
  | error err => simp [hcollect] at hres
  | ok result =>
      rcases result with ⟨valuesRaw, stRaw⟩
      cases hlist : valuesRaw with
      | nil => simp [hcollect, hlist] at hres
      | cons head tail =>
          have hpair : (values, st') = (head :: tail, stRaw) := by
            simpa [hcollect, hlist] using hres
          cases hpair
          have hvalues : collectPositionalValues spec st = .ok (values, st') := by
            simpa [hcollect, hlist]
          exact collectPositionalValues_progress (spec := spec)
            (st := st) (st' := st') (values := values) hvalues

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

/-- Flag interpreters either consume one token or leave the state untouched. -/
lemma interpretFlag_progress
    (spec : FlagSpec) (st st' : State)
    (updater : Spec.Partial → Spec.Partial) :
    interpretFlag spec st = .ok updater st' →
    st' = st ∨ st'.cursor = st.cursor + 1 := by
  intro hres
  classical
  unfold Spec.interpretFlag at hres
  simp [Parser.map] at hres
  cases hop : flag spec st with
  | err => simp [hop] at hres
  | ok result =>
      rcases result with ⟨enabled, stAfter⟩
      simp [hop] at hres
      cases hres
      cases enabled with
      | false =>
          have := flag_false_preserves_state
            (spec := spec) (st := st) (st' := stAfter) (by simpa [hop])
          exact Or.inl (by simpa [this])
      | true =>
          have := flag_true_progress
            (spec := spec) (st := st) (st' := stAfter) (by simpa [hop])
          exact Or.inr (by simpa using this)

/-- Flag interpreters expose their cursor delta as a natural number. -/
lemma interpretFlag_progress_consumed
    (spec : FlagSpec) (st st' : State)
    (updater : Spec.Partial → Spec.Partial) :
    interpretFlag spec st = .ok updater st' →
    ∃ consumed, st'.cursor = st.cursor + consumed := by
  intro hres
  have h := interpretFlag_progress (spec := spec) (st := st) (st' := st')
    (updater := updater) hres
  refine h.elim ?zero ?one
  · intro hstate; exact ⟨0, by simpa [hstate]⟩
  · intro hstep; exact ⟨1, hstep⟩

/-- Zero-arity options leave the cursor unchanged. -/
lemma interpretOption_zero_progress
    {α} [FromArg α] (spec : OptSpec α) (st st' : State)
    (updater : Spec.Partial → Spec.Partial) :
    spec.arity = .zero →
    interpretOption (spec := spec) st = .ok updater st' →
    st' = st := by
  intro harity hres
  subst harity
  classical
  unfold Spec.interpretOption at hres
  simp [Parser.map, option] at hres

/-- Optional options consume at most two tokens. -/
lemma interpretOption_one_progress_consumed
    {α} [FromArg α] (spec : OptSpec α) (st st' : State)
    (updater : Spec.Partial → Spec.Partial) :
    spec.arity = .one →
    interpretOption (spec := spec) st = .ok updater st' →
    ∃ consumed, st'.cursor = st.cursor + consumed := by
  intro harity hres
  have h := interpretOption_one_progress
    (spec := spec) (st := st) (st' := st') (updater := updater) harity hres
  refine h.elim ?zero ?pos
  · intro hstate; exact ⟨0, by simpa [hstate]⟩
  · intro hcons
    cases hcons with
    | inl h1 => exact ⟨1, h1⟩
    | inr h2 => exact ⟨2, h2⟩

/-- Zero-arity positionals leave the cursor unchanged. -/
lemma interpretPositional_zero_progress
    {α} [FromArg α] (spec : PosSpec α) (st st' : State)
    (updater : Spec.Partial → Spec.Partial) :
    spec.arity = .zero →
    interpretPositional (spec := spec) st = .ok updater st' →
    st' = st := by
  intro harity hres
  subst harity
  classical
  unfold Spec.interpretPositional at hres
  simp [Parser.map, positional] at hres

/-- Optional positionals consume at most one token. -/
lemma interpretPositional_one_progress_consumed
    {α} [FromArg α] (spec : PosSpec α) (st st' : State)
    (updater : Spec.Partial → Spec.Partial) :
    spec.arity = .one →
    interpretPositional (spec := spec) st = .ok updater st' →
    ∃ consumed, st'.cursor = st.cursor + consumed := by
  intro harity hres
  have h := interpretPositional_one_progress
    (spec := spec) (st := st) (st' := st') (updater := updater) harity hres
  refine h.elim ?zero ?one
  · intro hstate; exact ⟨0, by simpa [hstate]⟩
  · intro hstep; exact ⟨1, hstep⟩

/-- Individual command items inherit cursor progress from their primitives. -/
lemma elaborateItem_progress
    (item : ItemSpec) :
    ∀ {st st' transformer},
      Spec.elaborateItem item st = .ok transformer st' →
      ∃ consumed, st'.cursor = st.cursor + consumed := by
  classical
  cases item with
  | flag spec =>
      intro st st' transformer h
      simpa [Spec.elaborateItem] using
        interpretFlag_progress_consumed (spec := spec) (st := st) (st' := st')
          (updater := transformer) h
  | opt spec =>
      intro st st' transformer h
      dsimp [Spec.elaborateItem] at h
      cases harity : spec.arity with
      | zero =>
          have hstate := interpretOption_zero_progress
            (spec := spec) (st := st) (st' := st') (updater := transformer)
            harity h
          exact ⟨0, by simpa [hstate]⟩
      | one =>
          exact interpretOption_one_progress_consumed
            (spec := spec) (st := st) (st' := st') (updater := transformer)
            harity h
      | many =>
          exact interpretOption_many_progress
            (spec := spec) (st := st) (st' := st') (updater := transformer)
            harity h
      | some =>
          exact interpretOption_some_progress
            (spec := spec) (st := st) (st' := st') (updater := transformer)
            harity h
  | pos spec =>
      intro st st' transformer h
      dsimp [Spec.elaborateItem] at h
      cases harity : spec.arity with
      | zero =>
          have hstate := interpretPositional_zero_progress
            (spec := spec) (st := st) (st' := st') (updater := transformer)
            harity h
          exact ⟨0, by simpa [hstate]⟩
      | one =>
          exact interpretPositional_one_progress_consumed
            (spec := spec) (st := st) (st' := st') (updater := transformer)
            harity h
      | many =>
          exact interpretPositional_many_progress
            (spec := spec) (st := st) (st' := st') (updater := transformer)
            harity h
      | some =>
          exact interpretPositional_some_progress
            (spec := spec) (st := st) (st' := st') (updater := transformer)
            harity h

/-- Folding command items accumulates cursor consumption. -/
lemma foldItems_progress
    : ∀ items {st st' transformer},
        Spec.foldItems items st = .ok transformer st' →
        ∃ consumed, st'.cursor = st.cursor + consumed := by
  classical
  intro items
  induction items with
  | nil =>
      intro st st' transformer h
      simp [Spec.foldItems] at h
      cases h
      exact ⟨0, rfl⟩
  | cons item rest ih =>
      intro st st' transformer h
      unfold Spec.foldItems at h
      simp [Spec.foldItems, Parser.seq, Parser.map] at h
      rcases hitem : Spec.elaborateItem item st with
      | err err => simp [hitem] at h
      | ok transformerHead st1 =>
          rcases hrest : Spec.foldItems rest st1 with
          | err err => simp [hitem, hrest] at h
          | ok transformerTail st2 =>
              rcases h with ⟨hval, hstate⟩
              have ⟨cHead, hHead⟩ :=
                elaborateItem_progress item (st := st) (st' := st1)
                  (transformer := transformerHead) hitem
              have ⟨cTail, hTail⟩ := ih (st := st1) (st' := st2)
                (transformer := transformerTail) hrest
              subst hstate
              refine ⟨cHead + cTail, ?_⟩
              simp [hHead, hTail, Nat.add_comm, Nat.add_left_comm, Nat.add_assoc]

/-- Elaborated commands accumulate cursor consumption from their items. -/
lemma elaborateCommand_progress
    (cmd : CmdSpec) {st st' payload} :
    Spec.elaborateCommand cmd st = .ok payload st' →
    ∃ consumed, st'.cursor = st.cursor + consumed := by
  classical
  intro h
  unfold Spec.elaborateCommand at h
  simp [Parser.map] at h
  rcases h with ⟨transformer, hfold, rfl⟩
  exact foldItems_progress cmd.args hfold

end ArgParse.Proofs

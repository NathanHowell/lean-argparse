import ArgParse.Core.Parser
import ArgParse.Core.Combinators
import ArgParse.Core.Scan
import ArgParse.Core.Value

/-!
# ArgParse.Proofs.Totality

Progress lemmas for the rebuilt runtime: flag parsers always succeed with an
explicit witness, and the option/positional collectors advance the cursor in
lockstep with the tokens they consume.
-/

namespace ArgParse.Proofs

open ArgParse
open ArgParse.Core
open ArgParse.Spec
open Classical

namespace Totality

/-- Flag parsers never fail: they always return an `.ok` result. -/
@[simp] theorem flag_result_ok (spec : FlagSpec) (st : State) :
    ∃ (b : Bool) (st' : State), Core.flag spec st = Result.ok b st' := by
  classical
  cases hpre : st.pre with
  | nil =>
      exact ⟨false, st, by simp [Core.flag, hpre]⟩
  | cons token rest =>
      cases hmatch : Core.matchFlagToken spec token with
      | none =>
          exact ⟨false, st, by simp [Core.flag, hpre, hmatch]⟩
      | short =>
          let st' : State := State.withPre st rest 1
          exact ⟨true, st', by simp [Core.flag, hpre, hmatch, st']⟩
      | long =>
          let st' : State := State.withPre st rest 1
          exact ⟨true, st', by simp [Core.flag, hpre, hmatch, st']⟩
      | shortBundled tail =>
          let st' : State := State.withPre st (("-" ++ tail) :: rest) 1
          exact ⟨true, st', by simp [Core.flag, hpre, hmatch, st']⟩

/-- Flag parsers either leave the cursor untouched or advance by one token. -/
theorem flag_cursor_progress {spec : FlagSpec} {st : State} {b : Bool} {st' : State}
    (h : Core.flag spec st = Result.ok b st') :
    st'.cursor = st.cursor ∨ st'.cursor = st.cursor + 1 := by
  classical
  cases hpre : st.pre with
  | nil =>
      have h' : b = false ∧ st = st' := by
        simpa [Core.flag, hpre] using h
      rcases h' with ⟨hb, hst⟩
      subst hb
      cases hst
      simp
  | cons token rest =>
      cases hmatch : Core.matchFlagToken spec token with
      | none =>
          have h' : b = false ∧ st = st' := by
            simpa [Core.flag, hpre, hmatch] using h
          rcases h' with ⟨hb, hst⟩
          subst hb
          cases hst
          simp
      | short =>
          have h' : b = true ∧ State.withPre st rest 1 = st' := by
            simpa [Core.flag, hpre, hmatch] using h
          rcases h' with ⟨hb, hst⟩
          subst hb
          cases hst
          simp [State.withPre]
      | long =>
          have h' : b = true ∧ State.withPre st rest 1 = st' := by
            simpa [Core.flag, hpre, hmatch] using h
          rcases h' with ⟨hb, hst⟩
          subst hb
          cases hst
          simp [State.withPre]
      | shortBundled tail =>
          have h' : b = true ∧
              State.withPre st (("-" ++ tail) :: rest) 1 = st' := by
            simpa [Core.flag, hpre, hmatch] using h
          rcases h' with ⟨hb, hst⟩
          subst hb
          cases hst
          simp [State.withPre]

/-- Flag cursors never retreat and grow by at most one. -/
theorem flag_cursor_bounds {spec : FlagSpec} {st : State} {b : Bool} {st' : State}
    (h : Core.flag spec st = Result.ok b st') :
    st.cursor ≤ st'.cursor ∧ st'.cursor ≤ st.cursor + 1 := by
  classical
  have := flag_cursor_progress (spec := spec) (st := st) (b := b) (st' := st') h
  cases this with
  | inl hEq =>
      constructor <;> simp [hEq]
  | inr hEq =>
      constructor
      ·
        have hBound : st.cursor ≤ st.cursor + 1 := Nat.le_of_lt (Nat.lt_succ_self _)
        have hEqSymm := hEq.symm
        exact hEqSymm ▸ hBound
      · simp [hEq]

/-- Cursor alignment for the generic collection loop: whenever the supplied
step function advances the cursor by exactly the tokens it consumes, so does
the whole loop. -/
theorem collectStepsLoop_cursor
    {α : Type} {takeStep : State → Except Error (Core.CollectStep α)}
    (hStep : ∀ st (step : Core.CollectStep α), takeStep st = .ok step →
      step.state.cursor = st.cursor + step.consumed) :
    ∀ fuel accVals accRaws consumed (st : State) (cursor0 : Nat)
      {result : Core.CollectResult α},
      st.cursor = cursor0 + consumed →
      Core.collectStepsLoop takeStep fuel accVals accRaws consumed st = .ok result →
      result.state.cursor = cursor0 + result.consumed := by
  classical
  intro fuel
  induction fuel with
  | zero =>
      intro accVals accRaws consumed st cursor0 result hCursor hLoop
      simp [Core.collectStepsLoop] at hLoop
      cases hLoop
      simp [hCursor]
  | succ fuel ih =>
      intro accVals accRaws consumed st cursor0 result hCursor hLoop
      simp [Core.collectStepsLoop] at hLoop
      cases hStepEq : takeStep st with
      | error err =>
          simp [hStepEq] at hLoop
      | ok step =>
          have hStepCursor := hStep st step hStepEq
          cases hValue : step.value? with
          | none =>
              simp [hStepEq, hValue] at hLoop
              cases hLoop
              simp [hCursor]
          | some value =>
              cases hRaw : step.raw? with
              | none =>
                  simp [hStepEq, hValue, hRaw] at hLoop
                  cases hLoop
                  simp [hCursor]
              | some raw =>
                  have hCursor' : step.state.cursor =
                      cursor0 + (consumed + step.consumed) := by
                    calc
                      step.state.cursor = st.cursor + step.consumed := hStepCursor
                      _ = (cursor0 + consumed) + step.consumed := by simp [hCursor]
                      _ = cursor0 + (consumed + step.consumed) :=
                        Nat.add_assoc cursor0 consumed step.consumed
                  have hLoop' := hLoop
                  simp [hStepEq, hValue, hRaw] at hLoop'
                  exact ih (value :: accVals) (raw :: accRaws)
                    (consumed + step.consumed) step.state cursor0 hCursor' hLoop'

/-- Cursor alignment for the positional collector. -/
theorem collectPositionalSteps_cursor
    {α : Type} [FromArg α] {spec : PosSpec α} {st : State}
    {result : Core.CollectResult α}
    (h : Core.collectPositionalSteps spec st = .ok result) :
    result.state.cursor = st.cursor + result.consumed := by
  classical
  have hLoop := h
  simp [Core.collectPositionalSteps] at hLoop
  exact collectStepsLoop_cursor
    (fun st step hStep =>
      Core.takePositionalStep?_cursor (spec := spec) (st := st) (step := step) hStep)
    (st.pre.length + st.post.length + 1) [] [] 0 st st.cursor rfl hLoop

/-- Scanning flag parsers never fail: they always return an `.ok` result. -/
@[simp] theorem flagScan_result_ok (spec : FlagSpec) (st : State) :
    ∃ (b : Bool) (st' : State), Core.flagScan spec st = Result.ok b st' := by
  classical
  cases h : Core.scanFlagPre spec st.pre with
  | none => exact ⟨false, st, by simp [Core.flagScan, h]⟩
  | some pre' => exact ⟨true, State.withPre st pre' 1, by simp [Core.flagScan, h]⟩

/-- Cursor alignment for the scanning option collector. -/
theorem collectOptionScanSteps_cursor
    {α : Type} [FromArg α] {spec : OptSpec α} {st : State}
    {result : Core.CollectResult α}
    (h : Core.collectOptionScanSteps spec st = .ok result) :
    result.state.cursor = st.cursor + result.consumed := by
  classical
  have hLoop := h
  simp [Core.collectOptionScanSteps] at hLoop
  exact collectStepsLoop_cursor
    (fun st step hStep =>
      Core.takeOptionScanStep?_cursor (spec := spec) (st := st) (step := step) hStep)
    (st.pre.length + st.post.length + 1) [] [] 0 st st.cursor rfl hLoop

/-- Cursor alignment for the option collector. -/
theorem collectOptionSteps_cursor
    {α : Type} [FromArg α] {spec : OptSpec α} {st : State}
    {result : Core.CollectResult α}
    (h : Core.collectOptionSteps spec st = .ok result) :
    result.state.cursor = st.cursor + result.consumed := by
  classical
  have hLoop := h
  simp [Core.collectOptionSteps] at hLoop
  exact collectStepsLoop_cursor
    (fun st step hStep =>
      Core.takeOptionStep?_cursor (spec := spec) (st := st) (step := step) hStep)
    (st.pre.length + st.post.length + 1) [] [] 0 st st.cursor rfl hLoop

end Totality

end ArgParse.Proofs

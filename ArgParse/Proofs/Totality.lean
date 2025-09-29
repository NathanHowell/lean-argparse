import ArgParse.Core.Parser
import ArgParse.Core.Combinators
import ArgParse.Core.Runner
import ArgParse.Core.Value
import ArgParse.Spec.Elab

/-!
# ArgParse.Proofs.Totality

Totality lemmas showing that the rebuilt runtime never produces undefined
results; combinators and elaborators either succeed with a value/state pair or
raise a structured error.
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
      constructor <;> simpa [hEq]
  | inr hEq =>
      constructor
      · have : st.cursor ≤ st.cursor + 1 := Nat.le_of_lt (Nat.lt_succ_self _)
        simpa [hEq] using this
      · simpa [hEq]

/-- Option parsers either succeed with a value/state or emit an error. -/
@[simp] theorem option_result_cases
    {α : Type} [FromArg α] (spec : OptSpec α) (st : State) :
    (∃ value st', Core.option spec st = Result.ok value st') ∨
    (∃ err, Core.option spec st = Result.err err) := by
  classical
  cases Core.option spec st with
  | ok value st' => exact Or.inl ⟨value, st', rfl⟩
  | err err => exact Or.inr ⟨err, rfl⟩

/-- Positional parsers either succeed with a value/state or emit an error. -/
@[simp] theorem positional_result_cases
    {α : Type} [FromArg α] (spec : PosSpec α) (st : State) :
    (∃ value st', Core.positional spec st = Result.ok value st') ∨
    (∃ err, Core.positional spec st = Result.err err) := by
  classical
  cases Core.positional spec st with
  | ok value st' => exact Or.inl ⟨value, st', rfl⟩
  | err err => exact Or.inr ⟨err, rfl⟩

/-- Elaboration of a single item yields either a transformer/state or an error. -/
@[simp] theorem elaborateItem_result_cases
    (item : ItemSpec) (st : State) :
    (∃ f st', Spec.elaborateItem item st = Result.ok f st') ∨
    (∃ err, Spec.elaborateItem item st = Result.err err) := by
  classical
  cases Spec.elaborateItem item st with
  | ok f st' => exact Or.inl ⟨f, st', rfl⟩
  | err err => exact Or.inr ⟨err, rfl⟩

/-- Elaborating a command is total modulo the underlying option/positional errors. -/
@[simp] theorem elaborateCommand_result_cases (cmd : CmdSpec) (st : State) :
    (∃ payload st', Spec.elaborateCommand cmd st = Result.ok payload st') ∨
    (∃ err, Spec.elaborateCommand cmd st = Result.err err) := by
  classical
  cases Spec.elaborateCommand cmd st with
  | ok payload st' => exact Or.inl ⟨payload, st', rfl⟩
  | err err => exact Or.inr ⟨err, rfl⟩

/-- Application elaboration is likewise total up to the underlying parser errors. -/
@[simp] theorem elaborateApp_result_cases (app : AppSpec) (st : State) :
    (∃ payload st', Spec.elaborateApp app st = Result.ok payload st') ∨
    (∃ err, Spec.elaborateApp app st = Result.err err) := by
  classical
  cases Spec.elaborateApp app st with
  | ok payload st' => exact Or.inl ⟨payload, st', rfl⟩
  | err err => exact Or.inr ⟨err, rfl⟩

/-- Runner execution always yields a concrete `RunOutcome`. -/
@[simp] theorem runNormalized_cases {α : Type}
    (app : AppSpec) (fold : Spec.Partial → α)
    (st : State) :
    ∃ outcome, outcome = ArgParse.runNormalized app fold st := ⟨_, rfl⟩

end Totality

end ArgParse.Proofs

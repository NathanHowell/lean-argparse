import ArgParse.Core.Scan

/-!
# ArgParse.Proofs.Scan

Agreement between the scanning combinators (`Core.flagScan`, `Core.optionScan`)
and the front-of-stream ones (`Core.flag`, `Core.option`).

The two families cannot agree on every input — that is the whole point of
scanning — but they agree on *canonically ordered* argv, where each item's
tokens already sit at the front of the stream when that item runs. The results
below isolate exactly that condition:

* single steps agree whenever the item matches at the head of the stream, and
  errors propagate identically (`flagScan_eq_flag_of_head`,
  `takeOptionScanStep?_eq_of_head`, `takeOptionScanStep?_error`);
* the multi-value collectors and `optionScan`/`Core.option` agree under
  `StepsAgreeAt`, a state invariant asserting the two single steps coincide at
  each state the collector visits.

`StepsAgreeAt` is deliberately a property of the *state*, not of the spec: for
any option that can match at all there are streams where scanning legitimately
sees more, which is the entire point of the scanning layer.
-/

namespace ArgParse.Proofs

open ArgParse
open ArgParse.Core
open ArgParse.Spec

namespace Scan

/-! ### Flags -/

/-- A stream-wide scan miss implies the head token does not match either. -/
theorem matchFlagToken_none_of_scan_none
    {spec : FlagSpec} {token : String} {rest : List String}
    (h : scanFlagPre spec (token :: rest) = none) :
    matchFlagToken spec token = FlagMatch.none := by
  classical
  cases hMatch : matchFlagToken spec token with
  | none => rfl
  | short => simp [scanFlagPre, hMatch] at h
  | long => simp [scanFlagPre, hMatch] at h
  | shortBundled tail => simp [scanFlagPre, hMatch] at h

/-- When the flag matches the head token, scanning reduces to the
front-of-stream parser. -/
theorem flagScan_eq_flag_of_head
    {spec : FlagSpec} {st : State} {token : String} {rest : List String}
    (hPre : st.pre = token :: rest)
    (hMatch : matchFlagToken spec token ≠ FlagMatch.none) :
    flagScan spec st = Core.flag spec st := by
  classical
  cases hM : matchFlagToken spec token with
  | none => exact absurd hM hMatch
  | short => simp [flagScan, Core.flag, scanFlagPre, hPre, hM]
  | long => simp [flagScan, Core.flag, scanFlagPre, hPre, hM]
  | shortBundled tail => simp [flagScan, Core.flag, scanFlagPre, hPre, hM]

/-- When the flag occurs nowhere in the stream, scanning reduces to the
front-of-stream parser: both report absence without consuming input. -/
theorem flagScan_eq_flag_of_scan_none
    {spec : FlagSpec} {st : State}
    (h : scanFlagPre spec st.pre = none) :
    flagScan spec st = Core.flag spec st := by
  classical
  cases hPre : st.pre with
  | nil => simp [flagScan, Core.flag, hPre, scanFlagPre]
  | cons token rest =>
      have hScan : scanFlagPre spec (token :: rest) = none := by
        simpa [hPre] using h
      have hMatch : matchFlagToken spec token = FlagMatch.none :=
        matchFlagToken_none_of_scan_none (spec := spec) (token := token) (rest := rest) hScan
      simp [flagScan, Core.flag, hPre, hMatch, hScan]

/-! ### Options -/

/-- Scanning inspects the head of the stream first, so head errors propagate. -/
theorem takeOptionScanStep?_error
    {α : Type} [FromArg α] {spec : OptSpec α} {st : State} {err : Error}
    (h : takeOptionStep? spec st = .error err) :
    takeOptionScanStep? spec st = .error err := by
  classical
  unfold takeOptionScanStep?
  cases hPre : st.pre with
  | nil =>
      simp [takeOptionStep?, hPre] at h
  | cons token rest =>
      have hSt : ({ st with pre := token :: rest } : State) = st := by
        cases st; simp_all
      simp [takeOptionScanStepGo, hSt, h]

/-- When the option matches at the head of the stream, the scanning step is
exactly the front-of-stream step. -/
theorem takeOptionScanStep?_eq_of_head
    {α : Type} [FromArg α] {spec : OptSpec α} {st : State} {step : CollectStep α}
    (h : takeOptionStep? spec st = .ok step)
    (hValue : step.value?.isSome) :
    takeOptionScanStep? spec st = .ok step := by
  classical
  unfold takeOptionScanStep?
  cases hPre : st.pre with
  | nil =>
      simp [takeOptionStep?, hPre] at h
      cases h
      simp [CollectStep.stay] at hValue
  | cons token rest =>
      have hSt : ({ st with pre := token :: rest } : State) = st := by
        cases st; simp_all
      cases hVal : step.value? with
      | none => simp [hVal] at hValue
      | some value =>
          simp only [takeOptionScanStepGo, hSt, h, hVal]
          cases step with
          | mk value? raw? state consumed =>
              cases state
              simp_all

/-- Canonical ordering, as an invariant on parser states: at `st` the scanning
step sees exactly what the front-of-stream step sees. By
`stepsAgreeAt_of_head` this holds at every state where the option matches the
head of the stream, and by `stepsAgreeAt_of_pre_nil` at every exhausted state —
i.e. precisely when the option's occurrences already sit at the front when its
turn comes.

Note this is a property of the *state*, not of the spec: for any option that
can match at all there are states where scanning legitimately sees more, which
is the entire purpose of the scanning layer. -/
def StepsAgreeAt {α : Type} [FromArg α] (spec : OptSpec α) (st : State) : Prop :=
  takeOptionScanStep? spec st = takeOptionStep? spec st

/-- A nameless option never matches a token, at any position. -/
theorem takeOptionStep?_stay_of_no_names
    {α : Type} [FromArg α] {spec : OptSpec α}
    (hLong : spec.long? = none) (hShort : spec.short? = none) (st : State) :
    takeOptionStep? spec st = .ok (CollectStep.stay st) := by
  classical
  cases hPre : st.pre with
  | nil => simp [takeOptionStep?, hPre]
  | cons token rest =>
      simp [takeOptionStep?, hPre, hLong, takeOptionShortToken?, hShort]

/-- Scanning a nameless option likewise never matches. -/
theorem takeOptionScanStep?_stay_of_no_names
    {α : Type} [FromArg α] {spec : OptSpec α}
    (hLong : spec.long? = none) (hShort : spec.short? = none) (st : State) :
    takeOptionScanStep? spec st = .ok (CollectStep.stay st) := by
  classical
  unfold takeOptionScanStep?
  suffices h : ∀ (suffix seen : List String),
      takeOptionScanStepGo spec st seen suffix = .ok (CollectStep.stay st) from
    h st.pre []
  intro suffix
  induction suffix with
  | nil => intro seen; simp [takeOptionScanStepGo]
  | cons token rest ih =>
      intro seen
      have hStep := takeOptionStep?_stay_of_no_names (spec := spec) hLong hShort
        { st with pre := token :: rest }
      simp [takeOptionScanStepGo, hStep, CollectStep.stay]
      exact ih (token :: seen)

/-- A nameless option agrees everywhere: a witness that `StepsAgreeAt` is
satisfiable, independent of the head-match and absence lemmas. -/
theorem stepsAgreeAt_of_no_names
    {α : Type} [FromArg α] {spec : OptSpec α}
    (hLong : spec.long? = none) (hShort : spec.short? = none) (st : State) :
    StepsAgreeAt spec st := by
  unfold StepsAgreeAt
  rw [takeOptionScanStep?_stay_of_no_names hLong hShort st,
    takeOptionStep?_stay_of_no_names hLong hShort st]

/-- An exhausted stream agrees: neither parser can match anything. -/
theorem stepsAgreeAt_of_pre_nil
    {α : Type} [FromArg α] {spec : OptSpec α} {st : State} (hPre : st.pre = []) :
    StepsAgreeAt spec st := by
  unfold StepsAgreeAt takeOptionScanStep?
  simp [takeOptionScanStepGo, takeOptionStep?, hPre]

/-- The head-match lemma, restated as the agreement invariant. -/
theorem stepsAgreeAt_of_head
    {α : Type} [FromArg α] {spec : OptSpec α} {st : State} {step : CollectStep α}
    (h : takeOptionStep? spec st = .ok step) (hValue : step.value?.isSome) :
    StepsAgreeAt spec st := by
  unfold StepsAgreeAt
  rw [takeOptionScanStep?_eq_of_head h hValue, h]

/-- On canonically ordered input the scanning collector loop agrees with the
front-of-stream one. `Visited` is any invariant holding at the start state and
preserved by the steps the loop takes; agreement is only required at states the
loop actually visits. -/
theorem collectStepsLoop_scan_eq
    {α : Type} [FromArg α] {spec : OptSpec α} (Visited : State → Prop)
    (hAgree : ∀ st, Visited st → StepsAgreeAt spec st)
    (hPres : ∀ st step, Visited st → takeOptionStep? spec st = .ok step →
      Visited step.state) :
    ∀ (fuel : Nat) (accVals : List α) (accRaws : List String)
      (consumed : Nat) (st : State), Visited st →
      collectStepsLoop (takeOptionScanStep? spec) fuel accVals accRaws consumed st =
        collectStepsLoop (takeOptionStep? spec) fuel accVals accRaws consumed st := by
  classical
  intro fuel
  induction fuel with
  | zero => intro accVals accRaws consumed st _; rfl
  | succ fuel ih =>
      intro accVals accRaws consumed st hVisited
      have hSame : takeOptionScanStep? spec st = takeOptionStep? spec st :=
        hAgree st hVisited
      cases hPlain : takeOptionStep? spec st with
      | error err =>
          simp [collectStepsLoop, hSame, hPlain]
      | ok step =>
          cases hVal : step.value? with
          | none => simp [collectStepsLoop, hSame, hPlain, hVal]
          | some value =>
              cases hRaw : step.raw? with
              | none => simp [collectStepsLoop, hSame, hPlain, hVal, hRaw]
              | some raw =>
                  simp only [collectStepsLoop, hSame, hPlain, hVal, hRaw]
                  exact ih (value :: accVals) (raw :: accRaws)
                    (consumed + step.consumed) step.state
                    (hPres st step hVisited hPlain)

/-- Collector agreement on canonically ordered input. -/
theorem collectOptionScanSteps_eq
    {α : Type} [FromArg α] {spec : OptSpec α} (Visited : State → Prop)
    (hAgree : ∀ st, Visited st → StepsAgreeAt spec st)
    (hPres : ∀ st step, Visited st → takeOptionStep? spec st = .ok step →
      Visited step.state)
    (st : State) (hSt : Visited st) :
    collectOptionScanSteps spec st = collectOptionSteps spec st :=
  collectStepsLoop_scan_eq Visited hAgree hPres _ _ _ _ st hSt

/-- Value-level collector agreement on canonically ordered input. -/
theorem collectOptionScanValues_eq
    {α : Type} [FromArg α] {spec : OptSpec α} (Visited : State → Prop)
    (hAgree : ∀ st, Visited st → StepsAgreeAt spec st)
    (hPres : ∀ st step, Visited st → takeOptionStep? spec st = .ok step →
      Visited step.state)
    (st : State) (hSt : Visited st) :
    collectOptionScanValues spec st = collectOptionValues spec st := by
  simp [collectOptionScanValues, collectOptionValues,
    collectOptionScanSteps_eq Visited hAgree hPres st hSt]

/-- The scanning and front-of-stream option parsers agree on canonically
ordered input, at every arity. -/
theorem optionScan_eq_option
    {α : Type} [FromArg α] {spec : OptSpec α} (Visited : State → Prop)
    (hAgree : ∀ st, Visited st → StepsAgreeAt spec st)
    (hPres : ∀ st step, Visited st → takeOptionStep? spec st = .ok step →
      Visited step.state)
    (st : State) (hSt : Visited st) :
    optionScan spec st = Core.option spec st := by
  classical
  unfold optionScan Core.option
  rw [collectOptionScanValues_eq Visited hAgree hPres st hSt]
  rfl

/-- Concrete instance: a nameless option agrees at every state, so the two
parsers coincide outright. -/
theorem optionScan_eq_option_of_no_names
    {α : Type} [FromArg α] {spec : OptSpec α}
    (hLong : spec.long? = none) (hShort : spec.short? = none) (st : State) :
    optionScan spec st = Core.option spec st :=
  optionScan_eq_option (fun _ => True)
    (fun st' _ => stepsAgreeAt_of_no_names hLong hShort st')
    (fun _ _ _ _ => trivial) st trivial

end Scan

end ArgParse.Proofs

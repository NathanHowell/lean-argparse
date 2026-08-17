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
  each state the collector visits;
* `Canonical` derives that invariant from the *syntax* of argv rather than
  assuming it — `optionToken?` classifies tokens by inspection, and
  `optionScan_eq_option_of_canonical` needs no further hypothesis.

Both conditions are properties of the *state*, not of the spec: for any option
that can match at all there are streams where scanning legitimately sees more,
which is the entire point of the scanning layer. `canonicalExample` witnesses
that `Canonical` is satisfiable by a real named option on a real stream.
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

/-! ### From syntax to behaviour

`optionToken?` classifies tokens by inspection; these lemmas connect that
classification to what the parsers actually do, so canonicality can be stated
syntactically instead of assumed. -/

/-- A token with no such prefix does not start with it, as a `Bool`. -/
theorem startsWith_eq_false {token p : String}
    (hp : ¬ (p.toList <+: token.toList)) : token.startsWith p = false := by
  simpa [String.startsWith_string_iff] using hp

/-- A token the option does not claim leaves the front-of-stream step in place. -/
theorem takeOptionStep?_stay_of_not_optionToken
    {α : Type} [FromArg α] {spec : OptSpec α} {st : State}
    {token : String} {rest : List String}
    (hPre : st.pre = token :: rest)
    (hTok : optionToken? spec token = false) :
    takeOptionStep? spec st = .ok (CollectStep.stay st) := by
  classical
  cases hL : spec.long? <;> cases hS : spec.short? <;>
    cases hE : spec.eqVal? <;> cases hC : spec.concatVal? <;>
    simp_all [takeOptionStep?, takeOptionLongToken?, takeOptionShortToken?,
      optionToken?, optionTokenShort?, CollectStep.stay] <;>
    -- The concatenated-value branches scrutinize `token.startsWith` as a `Bool`,
    -- where `hTok` is stated as a list-prefix predicate; `startsWith_eq_false`
    -- crosses between the two forms.
    (rw [startsWith_eq_false]; simp_all)

/-- A stream containing no token the option claims leaves the scanning step in
place: scanning walks the whole stream and finds nothing. -/
theorem takeOptionScanStep?_stay_of_no_match
    {α : Type} [FromArg α] {spec : OptSpec α} {st : State}
    (h : ∀ tok ∈ st.pre, optionToken? spec tok = false) :
    takeOptionScanStep? spec st = .ok (CollectStep.stay st) := by
  classical
  unfold takeOptionScanStep?
  suffices hGo : ∀ (suffix seen : List String),
      (∀ tok ∈ suffix, optionToken? spec tok = false) →
      takeOptionScanStepGo spec st seen suffix = .ok (CollectStep.stay st) from
    hGo st.pre [] h
  intro suffix
  induction suffix with
  | nil => intro seen _; simp [takeOptionScanStepGo]
  | cons token rest ih =>
      intro seen hAll
      have hStep := takeOptionStep?_stay_of_not_optionToken
        (spec := spec) (st := { st with pre := token :: rest })
        (token := token) (rest := rest) rfl (hAll token (by simp))
      simp [takeOptionScanStepGo, hStep, CollectStep.stay]
      exact ih (token :: seen) (fun tok hMem => hAll tok (by simp [hMem]))

/-- Syntactic absence implies agreement: if no token in the stream is one the
option claims, scanning and front-of-stream parsing both decline it. -/
theorem stepsAgreeAt_of_no_match
    {α : Type} [FromArg α] {spec : OptSpec α} {st : State}
    (h : ∀ tok ∈ st.pre, optionToken? spec tok = false) :
    StepsAgreeAt spec st := by
  classical
  unfold StepsAgreeAt
  rw [takeOptionScanStep?_stay_of_no_match h]
  cases hPre : st.pre with
  | nil => simp [takeOptionStep?, hPre]
  | cons token rest =>
      exact (takeOptionStep?_stay_of_not_optionToken (spec := spec) (st := st) hPre
        (h token (by simp [hPre]))).symm

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

/-! ### Canonically ordered input -/

/-- `Canonical` says the option's occurrences already sit at the front of the
stream: consume matches while they are at the head, and end at a stream (or an
error) where nothing further is claimed. Unlike a spec-level condition this is
satisfiable by real named options — see `canonicalExample` below — and it is
closed under the collector's own stepping, which is what makes the agreement
theorem unconditional on such input. -/
inductive Canonical {α : Type} [FromArg α] (spec : OptSpec α) : State → Prop where
  /-- No remaining token is one the option claims. -/
  | exhausted {st : State}
      (h : ∀ tok ∈ st.pre, optionToken? spec tok = false) : Canonical spec st
  /-- The stream is malformed for this option; both parsers report it alike. -/
  | failure {st : State} {err : Error}
      (h : takeOptionStep? spec st = .error err) : Canonical spec st
  /-- The option matches at the head, and the residual stream is canonical. -/
  | consume {st : State} {step : CollectStep α}
      (h : takeOptionStep? spec st = .ok step) (hValue : step.value?.isSome)
      (hNext : Canonical spec step.state) : Canonical spec st

/-- Canonical states satisfy the agreement invariant. -/
theorem canonical_stepsAgreeAt
    {α : Type} [FromArg α] {spec : OptSpec α} {st : State}
    (h : Canonical spec st) : StepsAgreeAt spec st := by
  cases h with
  | exhausted hNo => exact stepsAgreeAt_of_no_match hNo
  | failure hErr =>
      unfold StepsAgreeAt
      rw [takeOptionScanStep?_error hErr, hErr]
  | consume hStep hValue _ => exact stepsAgreeAt_of_head hStep hValue

/-- Canonicality is preserved by the steps the collector takes. -/
theorem canonical_preserved
    {α : Type} [FromArg α] {spec : OptSpec α} {st : State} {step : CollectStep α}
    (h : Canonical spec st) (hStep : takeOptionStep? spec st = .ok step) :
    Canonical spec step.state := by
  classical
  cases h with
  | exhausted hNo =>
      have hStay : takeOptionStep? spec st = .ok (CollectStep.stay st) := by
        cases hPre : st.pre with
        | nil => simp [takeOptionStep?, hPre]
        | cons token rest =>
            exact takeOptionStep?_stay_of_not_optionToken (spec := spec) (st := st) hPre
              (hNo token (by simp [hPre]))
      have : step = CollectStep.stay st := by
        have := hStep.symm.trans hStay
        simpa using this
      subst this
      exact Canonical.exhausted hNo
  | failure hErr => rw [hErr] at hStep; simp at hStep
  | @consume _ step' hStep' _ hNext =>
      have hEq : step = step' := by
        have := hStep.symm.trans hStep'
        simpa using this
      subst hEq
      exact hNext

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

/-- **Agreement on canonically ordered argv.** No hypothesis beyond the
syntactic canonicality of the input: where the option's occurrences already sit
at the front of the stream, scanning and front-of-stream parsing return exactly
the same result. -/
theorem optionScan_eq_option_of_canonical
    {α : Type} [FromArg α] {spec : OptSpec α} {st : State}
    (h : Canonical spec st) :
    optionScan spec st = Core.option spec st :=
  optionScan_eq_option (Canonical spec)
    (fun _ hc => canonical_stepsAgreeAt hc)
    (fun _ _ hc hStep => canonical_preserved hc hStep) st h

/-- Agreement whenever the option is simply absent from the stream. -/
theorem optionScan_eq_option_of_no_match
    {α : Type} [FromArg α] {spec : OptSpec α} {st : State}
    (h : ∀ tok ∈ st.pre, optionToken? spec tok = false) :
    optionScan spec st = Core.option spec st :=
  optionScan_eq_option_of_canonical (Canonical.exhausted h)

/-- Concrete instance: a nameless option agrees at every state, so the two
parsers coincide outright. -/
theorem optionScan_eq_option_of_no_names
    {α : Type} [FromArg α] {spec : OptSpec α}
    (hLong : spec.long? = none) (hShort : spec.short? = none) (st : State) :
    optionScan spec st = Core.option spec st :=
  optionScan_eq_option (fun _ => True)
    (fun st' _ => stepsAgreeAt_of_no_names hLong hShort st')
    (fun _ _ _ _ => trivial) st trivial

/-! ### Non-vacuity

A fully concrete instance with a real, named, repeatable option on a real
canonically ordered stream, discharged by computation. This rules out the
possibility that `Canonical` is satisfiable only by degenerate specs. -/

/-- A `--name` option collecting many detached values, used to witness
`Canonical`. `eqVal?`/`concatVal?` are off so that token classification reduces
to string equality: `String.startsWith` is opaque to the kernel, so the `=`-form
branch could not otherwise be discharged by computation. -/
def demoOpt : OptSpec String :=
  { long? := some "name", «meta» := { name := "name" }, arity := .many
    , eqVal? := false, concatVal? := false }

/-- Canonically ordered argv for `demoOpt`: both occurrences precede the
non-matching tail. -/
def demoState : State :=
  { pre := ["--name", "alpha", "--name", "beta", "file.txt"], post := [], cursor := 0 }

/-- The demo stream is canonical for the demo option. -/
theorem canonicalExample : Canonical demoOpt demoState := by
  refine Canonical.consume (h := rfl) rfl ?_
  refine Canonical.consume (h := rfl) rfl ?_
  refine Canonical.exhausted ?_
  simp only [demoOpt, demoState, CollectStep.ofPre, State.withPre, List.mem_singleton]
  rintro tok rfl
  rfl

/-- Consequently the two parsers agree there — a closed, hypothesis-free
instance of the agreement theorem for a named option. -/
theorem optionScan_eq_option_example :
    optionScan demoOpt demoState = Core.option demoOpt demoState :=
  optionScan_eq_option_of_canonical canonicalExample

end Scan

end ArgParse.Proofs

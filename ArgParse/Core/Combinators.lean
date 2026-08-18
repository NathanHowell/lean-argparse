import ArgParse.Core.Parser
import ArgParse.Core.Value
import ArgParse.Core.Normalize
import ArgParse.Spec.AST

/-!
# ArgParse.Core.Combinators

Baseline flag/option/positional parsers operating on the core `State`.
-/

namespace ArgParse.Core

open ArgParse
open ArgParse.Spec

/-!
Helpers
-/

namespace State

/-- Replace the `pre` stream while advancing the cursor by `delta`. -/
@[inline] def withPre (st : State) (pre : List String) (delta : Nat) : State :=
  { st with pre := pre, cursor := st.cursor + delta }

/-- Replace the `post` stream while advancing the cursor by `delta`. -/
@[inline] def withPost (st : State) (post : List String) (delta : Nat) : State :=
  { st with post := post, cursor := st.cursor + delta }

@[simp] theorem withPre_pre (st : ArgParse.State) (pre : List String) (delta : Nat) :
    (withPre st pre delta).pre = pre := rfl

@[simp] theorem withPre_cursor (st : ArgParse.State) (pre : List String) (delta : Nat) :
    (withPre st pre delta).cursor = st.cursor + delta := rfl

@[simp] theorem withPre_post (st : ArgParse.State) (pre : List String) (delta : Nat) :
    (withPre st pre delta).post = st.post := rfl

@[simp] theorem withPost_post (st : ArgParse.State) (post : List String) (delta : Nat) :
    (withPost st post delta).post = post := rfl

@[simp] theorem withPost_cursor (st : ArgParse.State) (post : List String) (delta : Nat) :
    (withPost st post delta).cursor = st.cursor + delta := rfl

@[simp] theorem withPost_pre (st : ArgParse.State) (post : List String) (delta : Nat) :
    (withPost st post delta).pre = st.pre := rfl

/-- Cursor monotonicity for `withPre`. -/
@[simp] theorem cursor_le_withPre (st : ArgParse.State) (pre : List String) (delta : Nat) :
    st.cursor ≤ (withPre st pre delta).cursor := by
  simp [withPre, Nat.le_add_right]

/-- Cursor monotonicity for `withPost`. -/
@[simp] theorem cursor_le_withPost (st : ArgParse.State) (post : List String) (delta : Nat) :
    st.cursor ≤ (withPost st post delta).cursor := by
  simp [withPost, Nat.le_add_right]

/-- Remove the next token from `pre`, updating the cursor. -/
def consumePre? (st : State) : Option (String × State) :=
  match st.pre with
  | [] => none
  | tok :: rest =>
      let st' : State := withPre st rest 1
      some (tok, st')

/-- Remove the next token from `post`, updating the cursor. -/
def consumePost? (st : State) : Option (String × State) :=
  match st.post with
  | [] => none
  | tok :: rest =>
      let st' : State := withPost st rest 1
      some (tok, st')

/-- Successful `consumePre?` advances the cursor by one. -/
@[simp] theorem consumePre?_cursor
    {st : State} {tok : String} {st' : State}
    (h : consumePre? st = some (tok, st')) :
    st'.cursor = st.cursor + 1 := by
  classical
  cases hpre : st.pre with
  | nil =>
      simp [consumePre?, hpre] at h
  | cons head rest =>
      simp [consumePre?, hpre] at h
      rcases h with ⟨rfl, rfl⟩
      simp [State.withPre]

/-- Successful `consumePost?` advances the cursor by one. -/
@[simp] theorem consumePost?_cursor
    {st : State} {tok : String} {st' : State}
    (h : consumePost? st = some (tok, st')) :
    st'.cursor = st.cursor + 1 := by
  classical
  cases hpost : st.post with
  | nil =>
      simp [consumePost?, hpost] at h
  | cons head rest =>
      simp [consumePost?, hpost] at h
      rcases h with ⟨rfl, rfl⟩
      simp [State.withPost]

end State

/-- Surface lexeme for a short-form name (`-x`). -/
def shortLexeme (short : Short) : String :=
  "-" ++ String.singleton short.c

/-- Surface lexeme for a long-form name (`--name`). -/
def longLexeme (name : String) : String :=
  "--" ++ name

private def expectFlag (spec : FlagSpec) : Expect :=
  .flag (spec.short?.map (·.c)) spec.long?

private def expectOption {α : Type} [ArgParse.FromArg α] (spec : OptSpec α) : Expect :=
  .optionVal spec.«meta».name

private def missingValueError (token : String) (expect : Expect) : Error :=
  { kind := .missingValue, context := [token], expect := [expect] }

private def invalidValueError (token _msg : String) (expect : Expect) : Error :=
  { kind := .custom, context := [token], expect := [expect] }

private def missingOptionError {α : Type} [ArgParse.FromArg α] (spec : OptSpec α) : Error :=
  { kind := .missingValue, context := [], expect := [expectOption spec] }

private def expectPositional {α : Type} [ArgParse.FromArg α] (spec : PosSpec α) : Expect :=
  .positional spec.«meta».name

/--- Result of attempting to consume one option or positional token. -/
structure CollectStep (α : Type) where
  /-- Parsed value, if the token provided one. -/
  value? : Option α
  /-- Raw token string associated with the parsed value. -/
  raw?   : Option String
  /-- Parser state after processing the token. -/
  state : State
  /-- Number of tokens consumed while handling the token. -/
  consumed : Nat

namespace CollectStep

/-- A step that consumes nothing and leaves the state as it was. -/
@[inline] def stay {α : Type} (st : State) : CollectStep α :=
  { value? := none, raw? := none, state := st, consumed := 0 }

/-- A step consuming `delta` tokens from `pre`, leaving `rest` behind. -/
@[inline] def ofPre {α : Type}
    (st : State) (rest : List String) (delta : Nat)
    (value? : Option α) (raw? : Option String) : CollectStep α :=
  { value? := value?
  , raw? := raw?
  , state := State.withPre st rest delta
  , consumed := delta }

/-- A step consuming one token that carried its value concatenated. -/
@[inline] def ofConcat {α : Type}
    (payload : Option (α × String)) (state : State) : CollectStep α :=
  { value? := payload.map Prod.fst
  , raw? := payload.map Prod.snd
  , state := state
  , consumed := 1 }

end CollectStep

/--- Accumulator output used by the multi-value collectors. -/
structure CollectResult (α : Type) where
  /-- Collected values, stored in chronological order. -/
  values : List α
  /-- Raw tokens associated with the collected values. -/
  raws   : List String
  /-- Parser state after the collection completes. -/
  state  : State
  /-- Total number of tokens consumed during collection. -/
  consumed : Nat

/-- Entry describing a subcommand parser branch. -/
structure Subcommand (α : Type) where
  /-- Subcommand name matched against the next token. -/
  name : String
  /-- Parser invoked when the subcommand name matches. -/
  parser : Parser α

/-! ### Subcommand helpers -/

@[inline] private def subcommandError
    (kind : ErrorKind) (token? : Option String) (expect : List Expect) : Error :=
  { kind := kind
  , context := token?.toList
  , expect := expect }

/-- Match the next token against `entries` and run the parser it names. -/
@[inline] def subcommand {α : Type} (entries : List (Subcommand α)) : Parser α :=
  let expects := entries.map (fun e => Expect.subcommand e.name)
  fun st =>
    match entries with
    | [] => .err (subcommandError .missingValue none expects)
    | _ =>
        match st.pre with
        | [] => .err (subcommandError .missingValue none expects)
        | token :: rest =>
            let rec loop : List (Subcommand α) → Result α
              | [] => .err (subcommandError .unknownLong (some token) expects)
              | entry :: tail =>
                  if token = entry.name then
                    entry.parser (State.withPre st rest 1)
                  else
                    loop tail
            loop entries

/--- Take the first `n` characters from a string. -/
@[inline] def stringTake (s : String) (n : Nat) : String :=
  String.ofList (s.toList.take n)

/--- Drop the first `n` characters from a string. -/
@[inline] def stringDrop (s : String) (n : Nat) : String :=
  String.ofList (s.toList.drop n)

/--- Attempt to split a concatenated option token into a value and residual suffix. -/
@[inline] def findConcatSplit? {α : Type} [ArgParse.FromArg α] (raw : String) : Option (α × String) :=
  let candidates := ((List.range raw.length).drop 1).reverse
  let rec loop : List Nat → Option (α × String)
    | [] => none
    | idx :: rest =>
        let head := stringTake raw idx
        let suffix := stringDrop raw idx
        if suffix.isEmpty then
          loop rest
        else
          match FromArg.run head with
          | .ok value => some (value, suffix)
          | .error _ => loop rest
  loop candidates

/-- Determine whether the token matches the flag specification. -/
inductive FlagMatch
  /-- The token does not match the flag specification. -/
  | none
  /-- The token matches a single short flag. -/
  | short
  /-- The token matches a short flag and leaves bundled tail characters. -/
  | shortBundled (rest : String)
  /-- The token matches the long-form flag. -/
  | long

/-- Classify `token` against the flag's short and long forms. -/
@[inline] def matchFlagToken (spec : FlagSpec) (token : String) : FlagMatch :=
  match spec.long? with
  | some name =>
      if token = longLexeme name then
        FlagMatch.long
      else
        match spec.short? with
        | some short =>
            let shortLex := shortLexeme short
            if token = shortLex then
              FlagMatch.short
            else if token.startsWith shortLex then
              let rest := (token.drop shortLex.length).toString
              if rest.isEmpty then
                FlagMatch.short
              else if token.startsWith "--" then
                FlagMatch.none
              else
                FlagMatch.shortBundled rest
            else
              FlagMatch.none
        | none => FlagMatch.none
  | none =>
      match spec.short? with
      | some short =>
          let shortLex := shortLexeme short
          if token = shortLex then
            FlagMatch.short
          else if token.startsWith shortLex then
            let rest := (token.drop shortLex.length).toString
            if rest.isEmpty then
              FlagMatch.short
            else if token.startsWith "--" then
              FlagMatch.none
            else
              FlagMatch.shortBundled rest
          else
            FlagMatch.none
      | none => FlagMatch.none

/-- Parser for boolean flags; returns `true` when the next token matches. -/
def flag (spec : FlagSpec) : Parser Bool := fun st =>
  match st.pre with
  | token :: rest =>
      match matchFlagToken spec token with
      | .none => .ok false st
      | .short =>
          let st' : State := State.withPre st rest 1
          .ok true st'
      | .long =>
          let st' : State := State.withPre st rest 1
          .ok true st'
      | .shortBundled tail =>
          let remainder := "-" ++ tail
          let st' : State := State.withPre st (remainder :: rest) 1
          .ok true st'
  | [] => .ok false st

/-- Parse the value portion of an option token that may bundle its argument. -/
@[inline] def parseConcatValue
    {α : Type} [ArgParse.FromArg α] (_spec : OptSpec α) (token raw : String)
    (pending : List String) (st : State) (expect : Expect) :
    Except Error (Option (α × String) × State) :=
  let stAfter := State.withPre st pending 1
  match FromArg.run raw with
  | .ok value => .ok (some (value, raw), stAfter)
  | .error msg =>
      -- `--name=` names an empty value. Whether that is one is the value type's
      -- call, so it is asked first: `String` takes it, and a type that cannot
      -- gets `missingValue` rather than a decoding complaint about nothing.
      if raw = "" then
        .error (missingValueError token expect)
      else
        match findConcatSplit? (raw := raw) with
        | some (value, remainder) =>
            let newState :=
              State.withPre stAfter (("-" ++ remainder) :: pending) 0
            let consumedToken := stringTake raw (raw.length - remainder.length)
            .ok (some (value, consumedToken), newState)
        | none => .error (invalidValueError raw msg expect)

/-- Successful concatenated option parsing always advances the cursor by one token. -/
@[simp] theorem parseConcatValue_cursor
    {α : Type} [FromArg α] {spec : OptSpec α} {token raw : String}
    {pending : List String} {st : State} {expect : Expect}
    {payload : Option (α × String)} {st' : State}
    (h : parseConcatValue spec token raw pending st expect = .ok (payload, st')) :
    st'.cursor = st.cursor + 1 := by
  classical
  cases hRun : FromArg.run (α := α) raw with
  | ok value =>
      have hEval := h
      simp [parseConcatValue, hRun] at hEval
      rcases hEval with ⟨hPayload, hState⟩
      cases hPayload; cases hState
      simp [State.withPre]
  | error msg =>
      by_cases hRaw : raw = ""
      · rw [hRaw] at hRun
        simp [parseConcatValue, hRaw, hRun] at h
      · cases hSplit : findConcatSplit? (α := α) (raw := raw) with
        | none => simp [parseConcatValue, hRaw, hRun, hSplit] at h
        | some result =>
            obtain ⟨value, remainder⟩ := result
            have hEval := h
            simp [parseConcatValue, hRaw, hRun, hSplit] at hEval
            rcases hEval with ⟨hPayload, hState⟩
            cases hPayload; cases hState
            simp [State.withPre]

/-- Attempt a single option parsing step when the value is detached as the next token. -/
@[inline] def takeOptionDetachedValue?
    {α : Type} [ArgParse.FromArg α]
    (token : String) (rest : List String) (st : State) (expect : Expect) :
    Except Error (CollectStep α) :=
  match rest with
  | valueTok :: restTail =>
      match FromArg.run valueTok with
      | .ok value =>
          .ok (CollectStep.ofPre st restTail 2 (some value) (some valueTok))
      | .error msg => .error (invalidValueError valueTok msg expect)
  | [] => .error (missingValueError token expect)

/-- Attempt a single option parsing step when the value is bundled with the token. -/
@[inline] def takeOptionConcatPayload?
    {α : Type} [ArgParse.FromArg α] (spec : OptSpec α)
    (token raw : String) (rest : List String) (st : State) (expect : Expect) :
    Except Error (CollectStep α) :=
  match parseConcatValue spec token raw rest st expect with
  | .ok (payload, st') => .ok (CollectStep.ofConcat payload st')
  | .error err => .error err

/-- Handle short option tokens, accounting for detached and concatenated forms. -/
@[inline] def takeOptionShortToken?
    {α : Type} [ArgParse.FromArg α] (spec : OptSpec α)
    (token : String) (rest : List String) (st : State) (expect : Expect) :
    Except Error (CollectStep α) :=
  match spec.short? with
  | some short =>
      if token = shortLexeme short then
        takeOptionDetachedValue? token rest st expect
      else
        match spec.concatVal? with
        | true =>
            match token.startsWith (shortLexeme short) with
            | true =>
                let raw := (token.drop (shortLexeme short).length).toString
                takeOptionConcatPayload? spec token raw rest st expect
            | false => .ok (CollectStep.stay st)
        | false => .ok (CollectStep.stay st)
  | none => .ok (CollectStep.stay st)

/-- Handle long option tokens, delegating to short-token logic when needed. -/
@[inline] def takeOptionLongToken?
    {α : Type} [ArgParse.FromArg α] (spec : OptSpec α)
    (name token : String) (rest : List String) (st : State) (expect : Expect) :
    Except Error (CollectStep α) :=
  if spec.eqVal? then
    if token.startsWith (longLexeme name ++ "=") then
      let raw := (token.drop (longLexeme name ++ "=").length).toString
      takeOptionConcatPayload? spec token raw rest st expect
    else if token = longLexeme name then
      takeOptionDetachedValue? token rest st expect
    else
      takeOptionShortToken? spec token rest st expect
  else if token = longLexeme name then
    takeOptionDetachedValue? token rest st expect
  else
    takeOptionShortToken? spec token rest st expect

/-- Attempt a single option parsing step, recording progress metadata. -/
@[inline] def takeOptionStep?
    {α : Type} [ArgParse.FromArg α] (spec : OptSpec α) (st : State) :
    Except Error (CollectStep α) :=
  match st.pre with
  | [] => .ok (CollectStep.stay st)
  | token :: rest =>
      let expect := expectOption spec
      match spec.long? with
      | some name => takeOptionLongToken? spec name token rest st expect
      | none => takeOptionShortToken? spec token rest st expect

/-- Detached option values advance the cursor according to their recorded cost. -/
@[simp] theorem takeOptionDetachedValue?_cursor
    {α : Type} [FromArg α] {token : String} {rest : List String}
    {st : State} {expect : Expect} {step : CollectStep α}
    (h : takeOptionDetachedValue? token rest st expect = .ok step) :
    step.state.cursor = st.cursor + step.consumed := by
  classical
  unfold takeOptionDetachedValue? at h
  cases rest with
  | nil =>
      simp at h
  | cons valueTok restTail =>
      cases hRun : FromArg.run (α := α) valueTok with
      | ok value =>
          have hStep := h
          simp [hRun] at hStep
          cases hStep
          simp [CollectStep.ofPre, State.withPre]
      | error msg =>
          simp [hRun] at h

/-- Concatenated option payloads advance the cursor by one token. -/
@[simp] theorem takeOptionConcatPayload?_cursor
    {α : Type} [FromArg α] {spec : OptSpec α} {token raw : String}
    {rest : List String} {st : State} {expect : Expect} {step : CollectStep α}
    (h : takeOptionConcatPayload? spec token raw rest st expect = .ok step) :
    step.state.cursor = st.cursor + step.consumed := by
  classical
  unfold takeOptionConcatPayload? at h
  cases hParse : parseConcatValue spec token raw rest st expect with
  | error err =>
      simp [hParse] at h
  | ok payloadState =>
      rcases payloadState with ⟨payload, st'⟩
      have hStep := h
      simp [hParse] at hStep
      cases hStep
      have hCursor :=
        parseConcatValue_cursor (spec := spec) (token := token) (raw := raw)
          (pending := rest) (st := st) (expect := expect)
          (payload := payload) (st' := st') hParse
      simp [CollectStep.ofConcat, hCursor]

/-- Cursor progression for short-option handling. -/
@[simp] theorem takeOptionShortToken?_cursor
    {α : Type} [FromArg α] {spec : OptSpec α} {token : String}
    {rest : List String} {st : State} {expect : Expect} {step : CollectStep α}
    (h : takeOptionShortToken? spec token rest st expect = .ok step) :
    step.state.cursor = st.cursor + step.consumed := by
  classical
  unfold takeOptionShortToken? at h
  cases hShort : spec.short? with
  | none =>
      simp [hShort] at h
      cases h
      simp [CollectStep.stay]
  | some short =>
      by_cases hEq : token = shortLexeme short
      · have hBranch : takeOptionDetachedValue? token rest st expect = .ok step := by
          simpa [hShort, hEq] using h
        exact takeOptionDetachedValue?_cursor (token := token) (rest := rest)
          (st := st) (expect := expect) (step := step) hBranch
      · have hBranch := h
        simp [hShort, hEq] at hBranch
        cases hConcat : spec.concatVal? with
        | false =>
            simp [hConcat] at hBranch
            cases hBranch
            simp [CollectStep.stay]
        | true =>
            cases hStart : token.startsWith (shortLexeme short) with
            | false =>
                simp [hConcat, hStart] at hBranch
                cases hBranch
                simp [CollectStep.stay]
            | true =>
                have hPayload :
                    takeOptionConcatPayload? spec token
                        ((token.drop (shortLexeme short).length).toString) rest st expect = .ok step := by
                  simpa [hConcat, hStart] using hBranch
                exact takeOptionConcatPayload?_cursor (spec := spec) (token := token)
                  (raw := (token.drop (shortLexeme short).length).toString) (rest := rest) (st := st)
                  (expect := expect) (step := step) hPayload

/-- Cursor progression for long-option handling. -/
@[simp] theorem takeOptionLongToken?_cursor
    {α : Type} [FromArg α] {spec : OptSpec α} {name token : String}
    {rest : List String} {st : State} {expect : Expect} {step : CollectStep α}
    (h : takeOptionLongToken? spec name token rest st expect = .ok step) :
    step.state.cursor = st.cursor + step.consumed := by
  classical
  unfold takeOptionLongToken? at h
  -- Each branch body is handled by its own cursor lemma, so the conditions
  -- never need to be reasoned about: `split` peels them off and leaves the
  -- bodies untouched.
  split at h
  · split at h
    · exact takeOptionConcatPayload?_cursor (spec := spec) (token := token)
        (rest := rest) (st := st) (expect := expect) (step := step) h
    · split at h
      · exact takeOptionDetachedValue?_cursor (token := token) (rest := rest)
          (st := st) (expect := expect) (step := step) h
      · exact takeOptionShortToken?_cursor (spec := spec) (token := token)
          (rest := rest) (st := st) (expect := expect) (step := step) h
  · split at h
    · exact takeOptionDetachedValue?_cursor (token := token) (rest := rest)
        (st := st) (expect := expect) (step := step) h
    · exact takeOptionShortToken?_cursor (spec := spec) (token := token)
        (rest := rest) (st := st) (expect := expect) (step := step) h

/-- Successful option steps advance the cursor by the recorded amount. -/
@[simp] theorem takeOptionStep?_cursor
    {α : Type} [FromArg α] {spec : OptSpec α} {st : State} {step : CollectStep α}
    (h : takeOptionStep? spec st = .ok step) :
    step.state.cursor = st.cursor + step.consumed := by
  classical
  unfold takeOptionStep? at h
  cases hPre : st.pre with
  | nil =>
      simp [hPre] at h
      cases h
      simp [CollectStep.stay]
  | cons token rest =>
      have hStep := h
      simp [hPre] at hStep
      cases hLong : spec.long? with
      | some name =>
          have hBranch : takeOptionLongToken? spec name token rest st (expectOption spec) = .ok step := by
            simpa [hLong] using hStep
          exact takeOptionLongToken?_cursor (spec := spec) (name := name) (token := token)
            (rest := rest) (st := st) (expect := expectOption spec) (step := step) hBranch
      | none =>
          have hBranch : takeOptionShortToken? spec token rest st (expectOption spec) = .ok step := by
            simpa [hLong] using hStep
          exact takeOptionShortToken?_cursor (spec := spec) (token := token)
            (rest := rest) (st := st) (expect := expectOption spec) (step := step) hBranch

/-- Extract only the value/state pair from `takeOptionStep?`. -/
@[inline] def takeOptionValue?
    {α : Type} [ArgParse.FromArg α] (spec : OptSpec α) (st : State) :
    Except Error (Option (α × String) × State) :=
  match takeOptionStep? spec st with
  | .ok step =>
      let combo :=
        match step.value?, step.raw? with
        | some value, some raw => some (value, raw)
        | _, _ => none
      .ok (combo, step.state)
  | .error err => .error err

/-- Tail-recursive worker that gathers values from `takeStep` until it stops yielding them. -/
@[specialize] def collectStepsLoop {α : Type}
    (takeStep : State → Except Error (CollectStep α)) :
    Nat → List α → List String → Nat → State → Except Error (CollectResult α)
  | 0, accVals, accRaws, consumed, st =>
      .ok { values := accVals.reverse, raws := accRaws.reverse, state := st, consumed := consumed }
  | Nat.succ fuel, accVals, accRaws, consumed, st =>
      match takeStep st with
      | .error err => .error err
      | .ok step =>
          let consumed' := consumed + step.consumed
          match step.value?, step.raw? with
          | some value, some raw =>
              collectStepsLoop takeStep fuel (value :: accVals) (raw :: accRaws) consumed' step.state
          | some _, none =>
              .ok { values := accVals.reverse, raws := accRaws.reverse, state := st, consumed := consumed }
          | none, _ => .ok { values := accVals.reverse, raws := accRaws.reverse, state := st, consumed := consumed }

/-- Collect option parsing steps until the next token no longer satisfies the specification. -/
@[inline] def collectOptionSteps
    {α : Type} [ArgParse.FromArg α] (spec : OptSpec α) (st : State) : Except Error (CollectResult α) :=
  let fuel := st.pre.length + st.post.length + 1
  collectStepsLoop (takeOptionStep? spec) fuel [] [] 0 st

/-- Collect concrete option values alongside the updated parser state. -/
@[inline] def collectOptionValues
    {α : Type} [ArgParse.FromArg α] (spec : OptSpec α) (st : State) :
    Except Error (List α × List String × State) := do
  let result ← collectOptionSteps spec st
  return (result.values, result.raws, result.state)

/-- Parser for options supporting `.one`/`.many`/`.some` arities. -/
def option {α : Type} [ArgParse.FromArg α] (spec : OptSpec α) :
    Parser (match spec.arity with
      | .zero => PUnit
      | .one  => Option α
      | .many => List α
      | .some => List α) := fun st =>
  match spec.arity with
  | .zero => .ok PUnit.unit st
  | .one =>
      match collectOptionValues spec st with
      | .error err => .err err
      | .ok (values, _, st') =>
          .ok (values.getLast?) st'
  | .many =>
      match collectOptionValues spec st with
      | .error err => .err err
      | .ok (values, _, st') => .ok values st'
  | .some =>
      match collectOptionValues spec st with
      | .error err => .err err
      | .ok (values, _, st') =>
          match values with
          | [] => .err (missingOptionError spec)
          | _ => .ok values st'

/-- Whether a `pre` token looks like something a flag or option should claim,
rather than a positional's value.

A lone `-` is conventionally stdin and `-5` is a negative number, so neither
counts; everything else with a leading dash does. Past the `--` sentinel this
question is not asked at all, which is what makes `--` the escape hatch for a
positional value that really does start with a dash. -/
def optionLike (token : String) : Bool :=
  match token.toList with
  | '-' :: c :: _ => !c.isDigit
  | _ => false

/-- Read a positional from the post-sentinel stream. Everything after `--` is a
positional value, so no token there is ever declined. -/
@[inline] def takePositionalFromPost
    {α : Type} [ArgParse.FromArg α] (spec : PosSpec α) (st : State) :
    Except Error (CollectStep α) :=
  match State.consumePost? st with
  | some (token, st') =>
      match FromArg.run token with
      | .ok value => .ok { value? := some value, raw? := some token, state := st', consumed := 1 }
      | .error msg => .error (invalidValueError token msg (expectPositional spec))
  | none => .ok { value? := none, raw? := none, state := st, consumed := 0 }

/-- Post-sentinel steps advance the cursor by the recorded amount. -/
theorem takePositionalFromPost_cursor
    {α : Type} [ArgParse.FromArg α] {spec : PosSpec α} {st : State}
    {step : CollectStep α}
    (h : takePositionalFromPost spec st = .ok step) :
    step.state.cursor = st.cursor + step.consumed := by
  classical
  unfold takePositionalFromPost at h
  cases hPost : State.consumePost? st with
  | some pair =>
      rcases pair with ⟨token, st'⟩
      cases hRun : FromArg.run (α := α) token with
      | ok value =>
          simp only [hPost, hRun, Except.ok.injEq] at h
          cases h
          simpa using State.consumePost?_cursor (st := st) (tok := token) (st' := st') hPost
      | error msg =>
          simp [hPost, hRun] at h
  | none =>
      simp only [hPost, Except.ok.injEq] at h
      cases h
      simp

/-- Attempt a single positional parsing step, capturing progress metadata. -/
@[inline] def takePositionalStep?
    {α : Type} [ArgParse.FromArg α] (spec : PosSpec α) (st : State) :
    Except Error (CollectStep α) :=
  match State.consumePre? st with
  | some (token, st') =>
      if optionLike token then
        -- Something a flag or option was meant to claim. Binding a lexeme as
        -- this positional's value is never what was meant, and after
        -- `Core.prepare` these sit at the tail of the segment -- so reaching one
        -- means the positional values are already used up.
        takePositionalFromPost spec st
      else
        match FromArg.run token with
        | .ok value => .ok { value? := some value, raw? := some token, state := st', consumed := 1 }
        | .error msg =>
            .error (invalidValueError token msg (expectPositional spec))
  | none => takePositionalFromPost spec st

/-- Successful positional steps advance the cursor by the recorded amount. -/
@[simp] theorem takePositionalStep?_cursor
    {α : Type} [ArgParse.FromArg α] {spec : PosSpec α} {st : State}
    {step : CollectStep α}
    (h : takePositionalStep? spec st = .ok step) :
    step.state.cursor = st.cursor + step.consumed := by
  classical
  unfold takePositionalStep? at h
  cases hPre : State.consumePre? st with
  | none =>
      simp only [hPre] at h
      exact takePositionalFromPost_cursor h
  | some prePair =>
      rcases prePair with ⟨token, st'⟩
      simp only [hPre] at h
      by_cases hOpt : optionLike token = true
      · rw [if_pos hOpt] at h
        exact takePositionalFromPost_cursor h
      · rw [if_neg hOpt] at h
        cases hRun : FromArg.run (α := α) token with
        | ok value =>
            simp only [hRun, Except.ok.injEq] at h
            cases h
            simpa using State.consumePre?_cursor (st := st) (tok := token) (st' := st') hPre
        | error msg =>
            simp [hRun] at h

/-- Extract only the value/state pair from `takePositionalStep?`. -/
@[inline] def takePositionalValue?
    {α : Type} [ArgParse.FromArg α] (spec : PosSpec α) (st : State) :
    Except Error (Option (α × String) × State) :=
  match takePositionalStep? spec st with
  | .ok step =>
      let combo :=
        match step.value?, step.raw? with
        | some value, some raw => some (value, raw)
        | _, _ => none
      .ok (combo, step.state)
  | .error err => .error err

/-- Collect positional parsing steps while the specification continues to match. -/
@[inline] def collectPositionalSteps
    {α : Type} [ArgParse.FromArg α] (spec : PosSpec α) (st : State) : Except Error (CollectResult α) :=
  let fuel := st.pre.length + st.post.length + 1
  collectStepsLoop (takePositionalStep? spec) fuel [] [] 0 st

/-- Collect positional argument values alongside the updated parser state. -/
@[inline] def collectPositionalValues
    {α : Type} [ArgParse.FromArg α] (spec : PosSpec α) (st : State) :
    Except Error (List α × List String × State) := do
  let result ← collectPositionalSteps spec st
  return (result.values, result.raws, result.state)

/-- Parser for positional arguments supporting arities. -/
def positional {α : Type} [ArgParse.FromArg α] (spec : PosSpec α) :
    Parser (match spec.arity with
      | .zero => PUnit
      | .one  => Option α
      | .many => List α
      | .some => List α) := fun st =>
  match spec.arity with
  | .zero => .ok PUnit.unit st
  | .one =>
      match takePositionalValue? spec st with
      | .error err => .err err
      | .ok (value?, st') =>
          let typed := value?.map Prod.fst
          .ok typed st'
  | .many =>
      match collectPositionalValues spec st with
      | .error err => .err err
      | .ok (values, _, st') => .ok values st'
  | .some =>
      match collectPositionalValues spec st with
      | .error err => .err err
      | .ok (values, _, st') =>
          match values with
          | [] => .err { kind := .missingValue, context := [], expect := [expectPositional spec] }
          | _ => .ok values st'

end ArgParse.Core

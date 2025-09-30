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

private def shortLexeme (short : Short) : String :=
  "-" ++ String.singleton short.c

private def longLexeme (name : String) : String :=
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

/--- Result of attempting to consume an option token. -/
structure OptionStep (α : Type) where
  /-- Parsed value, if the token provided one. -/
  value? : Option α
  /-- Raw token string associated with the parsed value. -/
  raw?   : Option String
  /-- Parser state after processing the token. -/
  state : State
  /-- Number of tokens consumed while handling the option. -/
  consumed : Nat

/--- Result of attempting to consume a positional argument. -/
structure PosStep (α : Type) where
  /-- Parsed value, when the positional was present. -/
  value? : Option α
  /-- Raw token string associated with the parsed value. -/
  raw?   : Option String
  /-- Parser state after moving past the positional. -/
  state : State
  /-- Number of tokens consumed while handling the positional. -/
  consumed : Nat

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
  String.mk (s.data.take n)

/--- Drop the first `n` characters from a string. -/
@[inline] def stringDrop (s : String) (n : Nat) : String :=
  String.mk (s.data.drop n)

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
              let rest := token.drop shortLex.length
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
            let rest := token.drop shortLex.length
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
  if raw = "" then
    .error (missingValueError token expect)
  else
    let stAfter := State.withPre st pending 1
    match FromArg.run raw with
    | .ok value => .ok (some (value, raw), stAfter)
    | .error msg =>
        match findConcatSplit? (raw := raw) with
        | some (value, remainder) =>
            let newState :=
              State.withPre stAfter (("-" ++ remainder) :: pending) 0
            let consumedToken := stringTake raw (raw.length - remainder.length)
            .ok (some (value, consumedToken), newState)
        | none => .error (invalidValueError raw msg expect)

/-- Attempt a single option parsing step, recording progress metadata. -/
@[inline] def takeOptionStep?
    {α : Type} [ArgParse.FromArg α] (spec : OptSpec α) (st : State) :
    Except Error (OptionStep α) :=
  match st.pre with
  | [] => .ok { value? := none, raw? := none, state := st, consumed := 0 }
  | token :: rest =>
      let expect := expectOption spec
      match spec.long? with
      | some name =>
          let longLex := longLexeme name
          let eqPrefix := longLex ++ "="
          if spec.eqVal? ∧ token.startsWith eqPrefix then
            let raw := token.drop eqPrefix.length
            match parseConcatValue spec token raw rest st expect with
            | .ok (value?, st') =>
                let value := value?.map Prod.fst
                let rawVal := value?.map Prod.snd
                .ok { value? := value, raw? := rawVal, state := st', consumed := 1 }
            | .error err => .error err
          else if token = longLex then
            match rest with
            | valueTok :: restTail =>
                match FromArg.run valueTok with
                | .ok value =>
                    let st' : State := State.withPre st restTail 2
                    .ok { value? := some value, raw? := some valueTok, state := st', consumed := 2 }
                | .error msg => .error (invalidValueError valueTok msg expect)
            | [] => .error (missingValueError token expect)
          else
            match spec.short? with
            | some short =>
                let shortLex := shortLexeme short
                if token = shortLex then
                  match rest with
                  | valueTok :: restTail =>
                      match FromArg.run valueTok with
                      | .ok value =>
                          let st' : State := State.withPre st restTail 2
                          .ok { value? := some value, raw? := some valueTok, state := st', consumed := 2 }
                      | .error msg => .error (invalidValueError valueTok msg expect)
                  | [] => .error (missingValueError token expect)
                else if spec.concatVal? ∧ token.startsWith shortLex then
                  let raw := token.drop shortLex.length
                  match parseConcatValue spec token raw rest st expect with
                  | .ok (value?, st') =>
                      let value := value?.map Prod.fst
                      let rawVal := value?.map Prod.snd
                      .ok { value? := value, raw? := rawVal, state := st', consumed := 1 }
                  | .error err => .error err
                else
                  .ok { value? := none, raw? := none, state := st, consumed := 0 }
            | none => .ok { value? := none, raw? := none, state := st, consumed := 0 }
      | none =>
          match spec.short? with
          | some short =>
              let shortLex := shortLexeme short
              if token = shortLex then
                match rest with
                | valueTok :: restTail =>
                    match FromArg.run valueTok with
                    | .ok value =>
                        let st' : State := State.withPre st restTail 2
                        .ok { value? := some value, raw? := some valueTok, state := st', consumed := 2 }
                    | .error msg => .error (invalidValueError valueTok msg expect)
                | [] => .error (missingValueError token expect)
              else if spec.concatVal? ∧ token.startsWith shortLex then
                let raw := token.drop shortLex.length
                match parseConcatValue spec token raw rest st expect with
                | .ok (value?, st') =>
                    let value := value?.map Prod.fst
                    let rawVal := value?.map Prod.snd
                    .ok { value? := value, raw? := rawVal, state := st', consumed := 1 }
                | .error err => .error err
              else
                .ok { value? := none, raw? := none, state := st, consumed := 0 }
          | none => .ok { value? := none, raw? := none, state := st, consumed := 0 }

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

/-- Tail-recursive worker that gathers option values until the specification ceases to match. -/
@[specialize] def collectOptionStepsLoop
    {α : Type} [ArgParse.FromArg α] (spec : OptSpec α) :
    Nat → List α → List String → Nat → State → Except Error (CollectResult α)
  | 0, accVals, accRaws, consumed, st =>
      .ok { values := accVals.reverse, raws := accRaws.reverse, state := st, consumed := consumed }
  | Nat.succ fuel, accVals, accRaws, consumed, st =>
      match takeOptionStep? spec st with
      | .error err => .error err
      | .ok step =>
          let consumed' := consumed + step.consumed
          match step.value?, step.raw? with
          | some value, some raw =>
              collectOptionStepsLoop spec fuel (value :: accVals) (raw :: accRaws) consumed' step.state
          | some _, none =>
              .ok { values := accVals.reverse, raws := accRaws.reverse, state := st, consumed := consumed }
          | none, _ => .ok { values := accVals.reverse, raws := accRaws.reverse, state := st, consumed := consumed }

/-- Collect option parsing steps until the next token no longer satisfies the specification. -/
@[inline] def collectOptionSteps
    {α : Type} [ArgParse.FromArg α] (spec : OptSpec α) (st : State) : Except Error (CollectResult α) :=
  let fuel := st.pre.length + st.post.length + 1
  collectOptionStepsLoop spec fuel [] [] 0 st

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

/-- Attempt a single positional parsing step, capturing progress metadata. -/
@[inline] def takePositionalStep?
    {α : Type} [ArgParse.FromArg α] (spec : PosSpec α) (st : State) :
    Except Error (PosStep α) :=
  let expect := expectPositional spec
  match State.consumePre? st with
  | some (token, st') =>
      match FromArg.run token with
      | .ok value => .ok { value? := some value, raw? := some token, state := st', consumed := 1 }
      | .error msg => .error (invalidValueError token msg expect)
  | none =>
      match State.consumePost? st with
      | some (token, st') =>
          match FromArg.run token with
          | .ok value => .ok { value? := some value, raw? := some token, state := st', consumed := 1 }
          | .error msg => .error (invalidValueError token msg expect)
      | none => .ok { value? := none, raw? := none, state := st, consumed := 0 }

/-- Successful positional steps advance the cursor by the recorded amount. -/
@[simp] theorem takePositionalStep?_cursor
    {α : Type} [ArgParse.FromArg α] {spec : PosSpec α} {st : State}
    {step : PosStep α}
    (h : takePositionalStep? spec st = .ok step) :
    step.state.cursor = st.cursor + step.consumed := by
  classical
  unfold takePositionalStep? at h
  cases hPre : State.consumePre? st with
  | some prePair =>
      rcases prePair with ⟨token, st'⟩
      cases hRun : FromArg.run (α := α) token with
      | ok value =>
          have hStep := h
          simp [hPre, hRun] at hStep
          cases hStep
          simpa using State.consumePre?_cursor (st := st) (tok := token) (st' := st') hPre
      | error msg =>
          simp [hPre, hRun] at h
  | none =>
      cases hPost : State.consumePost? st with
      | some postPair =>
          rcases postPair with ⟨token, st'⟩
          cases hRun : FromArg.run (α := α) token with
          | ok value =>
              have hStep := h
              simp [hPre, hPost, hRun] at hStep
              cases hStep
              simpa using State.consumePost?_cursor (st := st) (tok := token) (st' := st') hPost
          | error msg =>
              simp [hPre, hPost, hRun] at h
      | none =>
          have hStep := h
          simp [hPre, hPost] at hStep
          cases hStep
          simp

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

/-- Tail-recursive worker that gathers positional values until the specification ceases to match. -/
@[specialize] def collectPositionalStepsLoop
    {α : Type} [ArgParse.FromArg α] (spec : PosSpec α) :
    Nat → List α → List String → Nat → State → Except Error (CollectResult α)
  | 0, accVals, accRaws, consumed, st =>
      .ok { values := accVals.reverse, raws := accRaws.reverse, state := st, consumed := consumed }
  | Nat.succ fuel, accVals, accRaws, consumed, st =>
      match takePositionalStep? spec st with
      | .error err => .error err
      | .ok step =>
          let consumed' := consumed + step.consumed
          match step.value?, step.raw? with
          | some value, some raw =>
              collectPositionalStepsLoop spec fuel (value :: accVals) (raw :: accRaws) consumed' step.state
          | some _, none =>
              .ok { values := accVals.reverse, raws := accRaws.reverse, state := st, consumed := consumed }
          | none, _ => .ok { values := accVals.reverse, raws := accRaws.reverse, state := st, consumed := consumed }

/-- Collect positional parsing steps while the specification continues to match. -/
@[inline] def collectPositionalSteps
    {α : Type} [ArgParse.FromArg α] (spec : PosSpec α) (st : State) : Except Error (CollectResult α) :=
  let fuel := st.pre.length + st.post.length + 1
  collectPositionalStepsLoop spec fuel [] [] 0 st

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

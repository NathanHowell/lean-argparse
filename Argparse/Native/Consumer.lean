import Argparse.Native.Error
import Argparse.Native.ArgStream
import Argparse.Native.Token

namespace Argparse
namespace Native
namespace Consumer

open Token
open Except

private def restoreFront (revSkipped : List String) (stream : ArgStream) : ArgStream :=
  revSkipped.foldl (fun acc tok => ArgStream.step tok acc) stream

@[simp] theorem restoreFront_nil (stream : ArgStream) :
    restoreFront [] stream = stream := by
  rfl

@[simp] theorem restoreFront_cons (tok : String) (revSkipped : List String) (stream : ArgStream) :
    restoreFront (tok :: revSkipped) stream =
      restoreFront revSkipped (ArgStream.step tok stream) := by
  unfold restoreFront
  simp [List.foldl]

@[simp] theorem tailList_restoreFront (revSkipped : List String) (stream : ArgStream) :
    ArgStream.tailList (restoreFront revSkipped stream) = ArgStream.tailList stream := by
  unfold restoreFront
  induction revSkipped generalizing stream with
  | nil => simp
  | cons tok rest ih =>
      simpa using ih (ArgStream.step tok stream)

theorem toList_restoreFront (revSkipped : List String) (stream : ArgStream) :
    ArgStream.toList (restoreFront revSkipped stream) =
      revSkipped.reverse ++ ArgStream.toList stream := by
  unfold restoreFront
  induction revSkipped generalizing stream with
  | nil => simp
  | cons tok rest ih =>
      have := ih (ArgStream.step tok stream)
      simp [List.foldl, List.reverse_cons, List.append_assoc, this]

theorem remaining_length_restoreFront (revSkipped : List String) (stream : ArgStream) :
    (ArgStream.remaining (restoreFront revSkipped stream)).length =
      revSkipped.length + (ArgStream.remaining stream).length := by
  have h := ArgStream.remaining_length (stream := restoreFront revSkipped stream)
  have hStream := ArgStream.remaining_length (stream := stream)
  simp [tailList_restoreFront, toList_restoreFront, List.length_append,
    List.length_reverse] at h
  simpa [hStream, Nat.add_comm, Nat.add_left_comm, Nat.add_assoc] using h

private def takePositionalLoop (revSkipped : List String) :
    ArgStream → Option (String × ArgStream)
  | .step tok rest =>
      if isOptionLike tok then
        takePositionalLoop (tok :: revSkipped) rest
      else
        let stream' := restoreFront revSkipped rest
        some (tok, stream')
  | .tail [] => none
  | .tail (tok :: tailTokens) =>
      let stream' := restoreFront revSkipped (ArgStream.tail tailTokens)
      some (tok, stream')

/-- Remove the next positional argument, skipping option-like tokens in the front section. -/
def takePositional? (stream : ArgStream) : Option (String × ArgStream) :=
  takePositionalLoop [] stream

theorem takePositionalLoop_progress (revSkipped : List String) (stream : ArgStream)
    {tok : String} {rest : ArgStream} :
    takePositionalLoop revSkipped stream = some (tok, rest) →
      (ArgStream.remaining rest).length <
        (ArgStream.remaining (restoreFront revSkipped stream)).length := by
  intro h
  unfold takePositionalLoop at h
  cases stream with
  | tail tailTokens =>
      cases tailTokens with
      | nil =>
          simp at h
      | cons tok tailTokens =>
          simp at h
          rcases h with ⟨hTok, hRest⟩
          subst hTok
          subst hRest
          have hNext : ArgStream.next? (ArgStream.tail (tok :: tailTokens)) =
              some (tok, ArgStream.tail tailTokens) := by simp [ArgStream.next?]
          have hProgress := ArgStream.next?_remaining_length_lt
              (stream := ArgStream.tail (tok :: tailTokens))
              (tok := tok)
              (rest := ArgStream.tail tailTokens)
              hNext
          have := Nat.add_lt_add_left hProgress revSkipped.length
          simpa [remaining_length_restoreFront]
  | step head rest =>
      by_cases hOpt : isOptionLike head
      · simp [hOpt] at h
        exact takePositionalLoop_progress (revSkipped := head :: revSkipped)
          (stream := rest) h
      · simp [hOpt] at h
        rcases h with ⟨hTok, hRest⟩
        subst hTok
        subst hRest
        have hNext : ArgStream.next? (ArgStream.step head rest) = some (head, rest) := by
          simp [ArgStream.next?]
        have hLt := ArgStream.next?_remaining_length_lt
            (stream := ArgStream.step head rest)
            (tok := head)
            (rest := rest)
            hNext
        have := Nat.add_lt_add_left hLt revSkipped.length
        simpa [remaining_length_restoreFront] using this

theorem takePositional?_progress {stream rest : ArgStream} {tok : String} :
    takePositional? stream = some (tok, rest) →
      (ArgStream.remaining rest).length < (ArgStream.remaining stream).length := by
  intro h
  have hLoop : takePositionalLoop [] stream = some (tok, rest) := by
    simpa [takePositional?] using h
  have hProgress := takePositionalLoop_progress (revSkipped := []) (stream := stream)
      (tok := tok) (rest := rest) hLoop
  simpa [restoreFront_nil] using hProgress

private def describe [TokenSpec α] (name : α) : String :=
  TokenSpec.describe name

private def mismatchError [TokenSpec α]
    (name : α) (detail : String) : Error :=
  { code := .invalid, subject? := some (describe name), detail? := some detail }

private def missingValueError [TokenSpec α] (name : α) : Error :=
  { code := .missing, subject? := some (describe name) }

private def rebuild (front tail : List String) : ArgStream :=
  ArgStream.ofFrontTail front tail

/-- Scan the front tokens for `name`, returning the updated front when found. -/
private def consumeFlagFront [DecidableEq α] [TokenSpec α]
    (name : α) : List String → Except Error (Bool × List String)
  | [] => .ok (false, [])
  | tok :: rest =>
      match TokenSpec.parse (α := α) tok with
      | some (found, value?) =>
          if found = name then
            match value? with
            | some _ =>
                .error <|
                  mismatchError name s!"Flag {describe name} does not accept a value"
            | none => .ok (true, rest)
          else
            match consumeFlagFront name rest with
            | .ok (present, newRest) => .ok (present, tok :: newRest)
            | .error err => .error err
      | none =>
          match consumeFlagFront name rest with
          | .ok (present, newRest) => .ok (present, tok :: newRest)
          | .error err => .error err

/-- Removing a present flag shortens the front list by one. -/
theorem consumeFlagFront_ok_length [DecidableEq α] [TokenSpec α]
    (name : α) :
    ∀ front {newFront : List String},
      consumeFlagFront name front = .ok (true, newFront) →
        newFront.length + 1 = front.length := by
  intro front
  induction front with
  | nil =>
      intro newFront h
      simp [consumeFlagFront] at h
  | cons tok rest ih =>
      intro newFront h
      dsimp [consumeFlagFront] at h
      cases hParse : TokenSpec.parse (α := α) tok with
      | none =>
          cases hRec : consumeFlagFront name rest with
          | error err =>
              simp [hParse, hRec] at h
          | ok result =>
              rcases result with ⟨present, newRest⟩
              cases present with
              | false =>
                  simp [hParse, hRec] at h
              | true =>
                  have hRecTrue : consumeFlagFront name rest = .ok (true, newRest) := by
                    simpa using hRec
                  have hLen := ih (newFront := newRest) hRecTrue
                  have hEq : newFront = tok :: newRest := by
                    simpa [consumeFlagFront, hParse, hRec] using h.symm
                  simpa [hEq, List.length_cons, hLen, Nat.add_comm, Nat.add_left_comm,
                    Nat.add_assoc]
      | some pair =>
          rcases pair with ⟨found, value?⟩
          by_cases hFound : found = name
          · subst hFound
            cases value? with
            | some _ =>
                simp [consumeFlagFront, hParse] at h
            | none =>
                have hEq : newFront = rest := by
                  simpa [consumeFlagFront, hParse] using h.symm
                simpa [hEq, List.length_cons, Nat.add_comm, Nat.add_left_comm]
          · cases hRec : consumeFlagFront name rest with
          | error err =>
              simp [hParse, hFound, hRec] at h
            | ok result =>
                rcases result with ⟨present, newRest⟩
                cases present with
                | false =>
                    simp [hParse, hFound, hRec] at h
                | true =>
                    have hRecTrue : consumeFlagFront name rest = .ok (true, newRest) := by
                      simpa using hRec
                    have hLen := ih (newFront := newRest) hRecTrue
                    have hEq : newFront = tok :: newRest := by
                      simpa [consumeFlagFront, hParse, hFound, hRec] using h.symm
                    simpa [consumeFlagFront, hParse, hFound, hRec, hEq,
                      List.length_cons, hLen, Nat.add_comm, Nat.add_left_comm,
                      Nat.add_assoc]

/-- Attempt to consume a flag token from the front of the stream. -/
def consumeFlag [DecidableEq α] [TokenSpec α]
    (name : α) (stream : ArgStream) : Except Error (Bool × ArgStream) :=
  let front := ArgStream.toList stream
  let tail := ArgStream.tailList stream
  match consumeFlagFront name front with
  | .ok (true, newFront) =>
      let newStream := rebuild newFront tail
      .ok (true, newStream)
  | .ok (false, _) => .ok (false, stream)
  | .error err => .error err

theorem consumeFlag_progress [DecidableEq α] [TokenSpec α]
    (name : α) {stream newStream : ArgStream} :
    consumeFlag name stream = .ok (true, newStream) →
      (ArgStream.remaining newStream).length <
        (ArgStream.remaining stream).length := by
  intro h
  dsimp [consumeFlag] at h
  cases hConsume : consumeFlagFront name (ArgStream.toList stream) with
  | error err =>
      simp [hConsume] at h
  | ok result =>
      rcases result with ⟨present, newFront⟩
      cases present with
      | false =>
          simp [hConsume] at h
      | true =>
          let tail := ArgStream.tailList stream
          have hConsumeTrue :
              consumeFlagFront name (ArgStream.toList stream) =
                .ok (true, newFront) := by
            simpa [hConsume]
          have hPair : (true, rebuild newFront tail) = (true, newStream) := by
            simpa [tail, hConsume] using h
          have hStream : newStream = rebuild newFront tail :=
            (congrArg Prod.snd hPair).symm
          subst hStream
          have hFrontLen :=
            consumeFlagFront_ok_length (name := name)
              (front := ArgStream.toList stream) (newFront := newFront) hConsumeTrue
          have hLtFront : newFront.length < (ArgStream.toList stream).length := by
            have hNat : newFront.length < newFront.length + 1 :=
              Nat.lt_succ_self _
            exact Nat.lt_of_lt_of_eq hNat hFrontLen
          have hNewLen :=
            ArgStream.remaining_length (stream := rebuild newFront tail)
          have hOldLen := ArgStream.remaining_length (stream := stream)
          have hNewLen' :
              (ArgStream.remaining (rebuild newFront tail)).length =
                newFront.length + (if tail = [] then 0 else tail.length + 1) := by
            simpa [rebuild, ArgStream.toList_ofFrontTail,
              ArgStream.tailList_ofFrontTail] using hNewLen
          have hOldLen' :
              (ArgStream.remaining stream).length =
                (ArgStream.toList stream).length +
                  (if tail = [] then 0 else tail.length + 1) := by
            simpa using hOldLen
          have hIneq :=
            Nat.add_lt_add_right hLtFront (if tail = [] then 0 else tail.length + 1)
          simpa [hNewLen'.symm, hOldLen'.symm] using hIneq

/-- Locate the option named `name`, returning its value and rebuilt stream. -/
private def consumeValueLoop [DecidableEq α] [TokenSpec α]
    (name : α) (revSkipped : List String)
    : ArgStream → Except Error (Option (String × ArgStream))
  | .step tok rest =>
      match TokenSpec.parse (α := α) tok with
      | some (found, some v) =>
          if found = name then
            let newStream := restoreFront revSkipped rest
            .ok (some (v, newStream))
          else
            consumeValueLoop name (tok :: revSkipped) rest
      | some (found, none) =>
          if found = name then
            match rest with
            | .step next restTail =>
                let newStream := restoreFront revSkipped restTail
                .ok (some (next, newStream))
            | .tail [] =>
                .error <| missingValueError name
            | .tail (next :: tailRest) =>
                let newStream := restoreFront revSkipped (ArgStream.tail tailRest)
                .ok (some (next, newStream))
          else
            consumeValueLoop name (tok :: revSkipped) rest
      | none => consumeValueLoop name (tok :: revSkipped) rest
  | .tail _ => .ok none

/-- Successful value consumption strictly decreases `ArgStream.remaining` length. -/
theorem consumeValueLoop_progress [DecidableEq α] [TokenSpec α]
    (name : α) :
    ∀ (stream : ArgStream) (revSkipped : List String)
      {value : String} {newStream : ArgStream},
      consumeValueLoop (name := name) revSkipped stream = .ok (some (value, newStream)) →
        (ArgStream.remaining newStream).length <
          (ArgStream.remaining (restoreFront revSkipped stream)).length := by
  classical
  intro stream
  induction stream with
  | tail tailTokens =>
      intro revSkipped value newStream h
      simp [consumeValueLoop] at h
  | step tok rest ih =>
      intro revSkipped value newStream h
      dsimp [consumeValueLoop] at h
      cases hParse : TokenSpec.parse (α := α) tok with
      | none =>
          have hHyp : consumeValueLoop (name := name) (tok :: revSkipped) rest =
              .ok (some (value, newStream)) := by
            simpa [consumeValueLoop, hParse] using h
          have hRec := ih (revSkipped := tok :: revSkipped)
            (value := value) (newStream := newStream) hHyp
          simpa [restoreFront_cons] using hRec
      | some parsed =>
          rcases parsed with ⟨found, value?⟩
          cases value? with
          | some v =>
              by_cases hFound : found = name
              · subst hFound
                simp [consumeValueLoop, hParse] at h
                rcases h with ⟨hValue, hStream⟩
                subst hValue
                subst hStream
                have hNext : ArgStream.next? (ArgStream.step tok rest) = some (tok, rest) := by
                  simp [ArgStream.next?]
                have hLt := ArgStream.next?_remaining_length_lt
                    (stream := ArgStream.step tok rest)
                    (tok := tok) (rest := rest) hNext
                have hAdd := Nat.add_lt_add_left hLt revSkipped.length
                have hGoal :
                    (ArgStream.remaining (restoreFront revSkipped rest)).length <
                      (ArgStream.remaining (restoreFront revSkipped (ArgStream.step tok rest))).length := by
                  simpa [remaining_length_restoreFront] using hAdd
                simpa using hGoal
              ·
                have hHyp : consumeValueLoop (name := name) (tok :: revSkipped) rest =
                    .ok (some (value, newStream)) := by
                  simpa [consumeValueLoop, hParse, hFound] using h
                have hRec := ih (revSkipped := tok :: revSkipped)
                  (value := value) (newStream := newStream) hHyp
                simpa [restoreFront_cons] using hRec
          | none =>
              by_cases hFound : found = name
              · subst hFound
                cases rest with
                | tail tailTokens =>
                    cases tailTokens with
                    | nil =>
                        simp [consumeValueLoop, hParse] at h
                    | cons next tailRest =>
                        simp [consumeValueLoop, hParse] at h
                        rcases h with ⟨hValue, hStream⟩
                        subst hValue
                        subst hStream
                        have hNextStream : ArgStream.next? (ArgStream.step tok (.tail (next :: tailRest))) =
                            some (tok, ArgStream.tail (next :: tailRest)) := by
                          simp [ArgStream.next?]
                        have hLtStream := ArgStream.next?_remaining_length_lt
                            (stream := ArgStream.step tok (.tail (next :: tailRest)))
                            (tok := tok)
                            (rest := ArgStream.tail (next :: tailRest))
                            hNextStream
                        have hNextTail : ArgStream.next? (.tail (next :: tailRest)) =
                            some (next, ArgStream.tail tailRest) := by
                          simp [ArgStream.next?]
                        have hLtTail := ArgStream.next?_remaining_length_lt
                            (stream := ArgStream.tail (next :: tailRest))
                            (tok := next)
                            (rest := ArgStream.tail tailRest)
                            hNextTail
                        have hChain := Nat.lt_trans hLtTail hLtStream
                        have hAdd := Nat.add_lt_add_left hChain revSkipped.length
                        have hGoal :
                            (ArgStream.remaining (restoreFront revSkipped (ArgStream.tail tailRest))).length <
                              (ArgStream.remaining (restoreFront revSkipped (ArgStream.step tok (.tail (next :: tailRest))))).length := by
                          simpa [remaining_length_restoreFront] using hAdd
                        simpa using hGoal
                | step next restTail =>
                    simp [consumeValueLoop, hParse] at h
                    rcases h with ⟨hValue, hStream⟩
                    subst hValue
                    subst hStream
                    have hNextStream : ArgStream.next? (ArgStream.step tok (.step next restTail)) =
                        some (tok, ArgStream.step next restTail) := by
                      simp [ArgStream.next?]
                    have hLtStream := ArgStream.next?_remaining_length_lt
                        (stream := ArgStream.step tok (.step next restTail))
                        (tok := tok)
                        (rest := ArgStream.step next restTail)
                        hNextStream
                    have hNextRest : ArgStream.next? (ArgStream.step next restTail) =
                        some (next, restTail) := by
                      simp [ArgStream.next?]
                    have hLtRest := ArgStream.next?_remaining_length_lt
                        (stream := ArgStream.step next restTail)
                        (tok := next)
                        (rest := restTail)
                        hNextRest
                    have hChain := Nat.lt_trans hLtRest hLtStream
                    have hAdd := Nat.add_lt_add_left hChain revSkipped.length
                    have hGoal :
                        (ArgStream.remaining (restoreFront revSkipped restTail)).length <
                          (ArgStream.remaining (restoreFront revSkipped (ArgStream.step tok (.step next restTail)))).length := by
                      simpa [remaining_length_restoreFront] using hAdd
                    simpa using hGoal
              · have hHyp : consumeValueLoop (name := name) (tok :: revSkipped) rest =
                      .ok (some (value, newStream)) := by
                    simpa [consumeValueLoop, hParse, hFound] using h
                have hRec := ih (revSkipped := tok :: revSkipped)
                  (value := value) (newStream := newStream) hHyp
                simpa [restoreFront_cons] using hRec

/-- Attempt to consume an option value token from the front of the stream. -/
def consumeValue [DecidableEq α] [TokenSpec α]
    (name : α) (stream : ArgStream) : Except Error (Option String × ArgStream) :=
  match consumeValueLoop (name := name) [] stream with
  | .ok (some (value, newStream)) => .ok (some value, newStream)
  | .ok none => .ok (none, stream)
  | .error err => .error err

theorem consumeValue_progress [DecidableEq α] [TokenSpec α]
    (name : α) {stream newStream : ArgStream} {value : String} :
    consumeValue name stream = .ok (some value, newStream) →
      (ArgStream.remaining newStream).length <
        (ArgStream.remaining stream).length := by
  intro h
  dsimp [consumeValue] at h
  cases hLoop : consumeValueLoop (name := name) [] stream with
  | error err =>
      simp [hLoop] at h
  | ok result =>
      cases result with
      | none =>
          simp [hLoop] at h
      | some pair =>
          rcases pair with ⟨value', newStream'⟩
          simp [hLoop] at h
          rcases h with ⟨hValue, hStream⟩
          subst hValue
          subst hStream
          have hProgress := consumeValueLoop_progress (name := name)
              (stream := stream) (revSkipped := [])
              (value := value') (newStream := newStream') hLoop
          simpa [restoreFront_nil] using hProgress

def consumeLongFlag := consumeFlag (α := String)

def consumeShortFlag := consumeFlag (α := Char)

def consumeLongValue := consumeValue (α := String)

def consumeShortValue := consumeValue (α := Char)

end Consumer
end Native
end Argparse

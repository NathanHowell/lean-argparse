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

/-- Measure contribution from the tail section to `remaining` length. -/
private def tailMeasure (tail : List String) : Nat :=
  if tail = [] then 0 else tail.length + 1

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

/-- Attempt to consume an option value token from the front of the stream. -/
def consumeValue [DecidableEq α] [TokenSpec α]
    (name : α) (stream : ArgStream) : Except Error (Option String × ArgStream) :=
  let front := ArgStream.toList stream
  let tail := ArgStream.tailList stream
  let rec loop (processed : List String)
      : List String → Except Error (Option (Option String × List String × List String))
    | [] => .ok none
    | tok :: rest =>
        match TokenSpec.parse tok with
        | some (found, value?) =>
            if found = name then
              match value? with
              | some v =>
                  let newFront := processed.reverse ++ rest
                  .ok (some (some v, newFront, tail))
              | none =>
                  match rest with
                  | next :: restTail =>
                      let newFront := processed.reverse ++ restTail
                      .ok (some (some next, newFront, tail))
                  | [] =>
                      match tail with
                      | next :: tailRest =>
                          let newFront := processed.reverse
                          .ok (some (some next, newFront, tailRest))
                      | [] =>
                          .error <| missingValueError name
            else
              loop (tok :: processed) rest
        | none => loop (tok :: processed) rest
  match loop [] front with
  | .ok (some (value, newFront, newTail)) =>
      let newStream := rebuild newFront newTail
      .ok (value, newStream)
  | .ok none => .ok (none, stream)
  | .error err => .error err

def consumeLongFlag := consumeFlag (α := String)

def consumeShortFlag := consumeFlag (α := Char)

def consumeLongValue := consumeValue (α := String)

def consumeShortValue := consumeValue (α := Char)

end Consumer
end Native
end Argparse

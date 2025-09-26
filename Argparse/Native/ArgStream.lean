import Std
import Argparse.Basic.ParseState

namespace Argparse

/--
`ArgStream` encodes the command-line arguments split at the first `--` sentinel.
Tokens that precede the sentinel are represented structurally via `step`, while
any tokens after the sentinel remain in a list captured by `tail`. This mirrors
`ParseState`, making it easier to perform structural recursion on "front" tokens
without losing the original CLI sequence.
-/
inductive ArgStream where
  /-- Tokens that appear after an explicit `--` separator. -/
  | tail : List String → ArgStream
  /-- A token preceding the `--` separator. -/
  | step : String → ArgStream → ArgStream
  deriving Repr

namespace ArgStream

/-- Extract the structurally tracked "front" tokens. -/
def toList : ArgStream → List String
  | .tail _ => []
  | .step tok rest => tok :: toList rest

/-- Extract the post-`--` tokens. -/
def tailList : ArgStream → List String
  | .tail tailTokens => tailTokens
  | .step _ rest => tailList rest

/-- Reconstruct the command-line arguments, re-inserting `--` when needed. -/
def remaining (stream : ArgStream) : List String :=
  match tailList stream with
    | [] => toList stream
    | tailTokens => toList stream ++ "--" :: tailTokens

/- Return the next token (front or tail) if one is available. -/
def next? : ArgStream → Option (String × ArgStream)
  | .step tok rest => some (tok, rest)
  | .tail [] => none
  | .tail (tok :: tailTokens) => some (tok, ArgStream.tail tailTokens)

@[simp] theorem tailList_step (tok : String) (rest : ArgStream) :
    tailList (.step tok rest) = tailList rest := rfl

@[simp] theorem tailList_tail (tailTokens : List String) :
    tailList (.tail tailTokens) = tailTokens := rfl

@[simp] theorem toList_tail (tailTokens : List String) :
    toList (.tail tailTokens) = [] := rfl

@[simp] theorem toList_step (tok : String) (rest : ArgStream) :
    toList (.step tok rest) = tok :: toList rest := rfl

@[simp] theorem remaining_tail_nil :
    remaining (.tail ([] : List String)) = [] := by
  simp [remaining]

@[simp] theorem remaining_tail_cons (tok : String) (tailTokens : List String) :
    remaining (.tail (tok :: tailTokens)) = "--" :: tok :: tailTokens := by
  simp [remaining]

@[simp] theorem remaining_step (tok : String) (rest : ArgStream) :
    remaining (.step tok rest) =
      match tailList rest with
      | [] => tok :: toList rest
      | tailTokens => (tok :: toList rest) ++ "--" :: tailTokens := by
  simp [remaining]

/-- Build an `ArgStream` from a raw list of CLI tokens. -/
def ofList : List String → ArgStream
  | [] => .tail []
  | "--" :: rest => .tail rest
  | tok :: rest => .step tok (ofList rest)

private def foldFront (front tailTokens : List String) : ArgStream :=
  front.foldr (fun tok acc => ArgStream.step tok acc) (ArgStream.tail tailTokens)

/-- Translate the existing `ParseState` into `ArgStream`. -/
def ofParseState (state : ParseState) : ArgStream :=
  foldFront state.front state.tail

@[simp] theorem toList_foldFront (front tailTokens : List String) :
    toList (foldFront front tailTokens) = front := by
  unfold foldFront
  induction front with
  | nil => simp
  | cons tok rest ih =>
      simp [List.foldr, ih]

@[simp] theorem toList_ofParseState (state : ParseState) :
    toList (ofParseState state) = state.front :=
  toList_foldFront _ _

@[simp] theorem tailList_foldFront (front tailTokens : List String) :
    tailList (foldFront front tailTokens) = tailTokens := by
  unfold foldFront
  induction front with
  | nil => simp
  | cons _ rest ih =>
      simp [List.foldr, ih]

@[simp] theorem tailList_ofParseState (state : ParseState) :
    tailList (ofParseState state) = state.tail :=
  tailList_foldFront _ _

@[simp] theorem remaining_ofParseState (state : ParseState) :
    remaining (ofParseState state) = ParseState.remaining state := by
  cases h : state.tail with
  | nil =>
      simp [remaining, toList_ofParseState, tailList_ofParseState, ParseState.remaining, h]
  | cons tok tailTokens =>
      simp [remaining, toList_ofParseState, tailList_ofParseState, ParseState.remaining, h]

end ArgStream

end Argparse

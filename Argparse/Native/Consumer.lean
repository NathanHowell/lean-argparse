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

/-- Remove the next positional argument, skipping option-like tokens in the front section. -/
def takePositional? (stream : ArgStream) : Option (String × ArgStream) :=
  let rec loop (revSkipped : List String) : ArgStream → Option (String × ArgStream)
    | .step tok rest =>
        if isOptionLike tok then
          loop (tok :: revSkipped) rest
        else
          let stream' := restoreFront revSkipped rest
          some (tok, stream')
    | .tail [] => none
    | .tail (tok :: tailTokens) =>
        let stream' := restoreFront revSkipped (ArgStream.tail tailTokens)
        some (tok, stream')
  loop [] stream

private def describe [TokenSpec α] (name : α) : String :=
  TokenSpec.describe name

private def mismatchError [TokenSpec α]
    (name : α) (detail : String) : Error :=
  { code := .invalid, subject? := some (describe name), detail? := some detail }

private def missingValueError [TokenSpec α] (name : α) : Error :=
  { code := .missing, subject? := some (describe name) }

private def rebuild (front tail : List String) : ArgStream :=
  ArgStream.ofFrontTail front tail

/-- Attempt to consume a flag token from the front of the stream. -/
def consumeFlag [DecidableEq α] [TokenSpec α]
    (name : α) (stream : ArgStream) : Except Error (Bool × ArgStream) :=
  let front := ArgStream.toList stream
  let tail := ArgStream.tailList stream
  let rec loop (processed : List String) : List String → Except Error (Option (List String))
    | [] => .ok none
    | tok :: rest =>
        match TokenSpec.parse tok with
        | some (found, value?) =>
            if found = name then
              match value? with
              | some _ => .error <| mismatchError name s!"Flag {describe name} does not accept a value"
              | none =>
                  let newFront := processed.reverse ++ rest
                  .ok (some newFront)
            else
              loop (tok :: processed) rest
        | none => loop (tok :: processed) rest
  match loop [] front with
  | .ok (some newFront) =>
      let newStream := rebuild newFront tail
      .ok (true, newStream)
  | .ok none => .ok (false, stream)
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

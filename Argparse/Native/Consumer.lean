import Argparse.Native.ArgStream
import Argparse.Native.Token

namespace Argparse
namespace Native
namespace Consumer

open Token

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

end Consumer
end Native
end Argparse

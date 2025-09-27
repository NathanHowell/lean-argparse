import Std
import Argparse.Native.Error
import Argparse.Native.ParsedToken

namespace Argparse
namespace Native

/--
An option handler mutates a partial parse when it recognises a matching
`ParsedOption`. Returning `none` indicates the handler did not consume the
option; `some` returns the updated partial state.
-/
structure OptionHandler (σ : Type) where
  apply : ParsedOption → σ → Except Error (Option σ)

namespace OptionHandler

@[inline] def const {σ : Type} (f : ParsedOption → σ → Except Error (Option σ)) :
    OptionHandler σ :=
  { apply := fun opt state => f opt state }

end OptionHandler

/--
A positional handler updates the partial state when fed the next positional
argument. Returning `none` signals that the handler chose not to consume the
argument so that later handlers get a chance.
-/
structure PositionalHandler (σ : Type) where
  apply : String → σ → Except Error (Option σ)

/-- Apply a list of handlers to a single option token. -/
partial def dispatchOption {σ : Type}
    (handlers : List (OptionHandler σ))
    (opt : ParsedOption) (state : σ) : Except Error σ :=
  match handlers with
  | [] =>
      throw {
        code := .unexpected
        , subject? := some opt.original
        , detail? := some "no parser field accepted this option"
      }
  | handler :: rest => do
      match ← handler.apply opt state with
      | some state' => pure state'
      | none => dispatchOption rest opt state

def foldOptions {σ : Type}
    (handlers : List (OptionHandler σ))
    (opts : Array ParsedOption) (state : σ) : Except Error σ :=
  opts.foldlM (fun s opt => dispatchOption handlers opt s) state

/-- Consume positional arguments from left to right using the supplied handler queue. -/
partial def dispatchPositional {σ : Type}
    (handlers : List (PositionalHandler σ))
    (arg : String) (state : σ) : Except Error (List (PositionalHandler σ) × σ) :=
  match handlers with
  | [] =>
      throw {
        code := .unexpected
        , subject? := some arg
        , detail? := some "too many positional arguments"
      }
  | handler :: rest => do
      match ← handler.apply arg state with
      | some state' => pure (rest, state')
      | none => do
          let (handlers', state') ← dispatchPositional rest arg state
          pure (handler :: handlers', state')

def foldPositionals {σ : Type}
    (handlers : List (PositionalHandler σ))
    (args : Array String) (state : σ) : Except Error σ :=
  let rec loop
      (pending : List (PositionalHandler σ))
      (idx : Nat) (curr : σ) : Except Error σ := do
    if _h : idx < args.size then
      let arg := args[idx]!
      let (pending', curr') ← dispatchPositional pending arg curr
      loop pending' (idx + 1) curr'
    else
      pure curr
  loop handlers 0 state

end Native
end Argparse

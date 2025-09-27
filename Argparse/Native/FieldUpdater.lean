import Std
import Argparse.Native.Error
import Argparse.Native.ParsedToken
import Argparse.Native.TokenCursor

namespace Argparse
namespace Native

/--
An option handler mutates a partial parse when it recognises a matching
`ParsedOption`. Returning `none` indicates the handler did not consume the
option; `some` returns the updated partial state.
-/
structure OptionHandler (σ : Type) where
  apply : ParsedOption → σ → Array String → Except Error (Option (σ × Array String))

namespace OptionHandler

@[inline] def const {σ : Type}
    (f : ParsedOption → σ → Array String → Except Error (Option (σ × Array String))) :
    OptionHandler σ :=
  { apply := fun opt state positionals => f opt state positionals }

/-- Lift an option handler so it only updates the left component of a product state. -/
@[inline] def mapLeft {σ τ : Type} (handler : OptionHandler σ) : OptionHandler (σ × τ) :=
  const fun opt (state : σ × τ) positionals => do
    let (left, right) := state
    match ← handler.apply opt left positionals with
    | some (left', remaining) => pure (some ((left', right), remaining))
    | none => pure none

/-- Lift an option handler so it only updates the right component of a product state. -/
@[inline] def mapRight {σ τ : Type} (handler : OptionHandler τ) : OptionHandler (σ × τ) :=
  const fun opt (state : σ × τ) positionals => do
    let (left, right) := state
    match ← handler.apply opt right positionals with
    | some (right', remaining) => pure (some ((left, right'), remaining))
    | none => pure none

end OptionHandler

/--
A positional handler updates the partial state when fed the next positional
argument. Returning `none` signals that the handler chose not to consume the
argument so that later handlers get a chance.
-/
structure PositionalHandler (σ : Type) where
  apply : String → σ → Except Error (Option σ)

namespace PositionalHandler

/-- Lift a positional handler so it updates the left component of a product state. -/
@[inline] def mapLeft {σ τ : Type} (handler : PositionalHandler σ) : PositionalHandler (σ × τ) :=
  {
    apply := fun arg (state : σ × τ) => do
      let (left, right) := state
      match ← handler.apply arg left with
      | some left' => pure (some (left', right))
      | none => pure none
  }

/-- Lift a positional handler so it updates the right component of a product state. -/
@[inline] def mapRight {σ τ : Type} (handler : PositionalHandler τ) : PositionalHandler (σ × τ) :=
  {
    apply := fun arg (state : σ × τ) => do
      let (left, right) := state
      match ← handler.apply arg right with
      | some right' => pure (some (left, right'))
      | none => pure none
  }

end PositionalHandler

/--
A bundle of handlers the interpreter can fold over the classified token
cursor. Options are processed to completion before positionals so that
option-derived updates always take precedence.
-/
structure HandlerBundle (σ : Type) where
  optionHandlers : List (OptionHandler σ)
  positionalHandlers : List (PositionalHandler σ)

namespace HandlerBundle

/--
Lift two handler bundles onto a product state by forwarding options and
positionals to both sides. Handlers from the left bundle keep their relative
priority by appearing earlier in the combined list.
-/
@[simp] def product {σ τ : Type}
    (left : HandlerBundle σ) (right : HandlerBundle τ) : HandlerBundle (σ × τ) :=
  { optionHandlers :=
      left.optionHandlers.map OptionHandler.mapLeft ++
      right.optionHandlers.map OptionHandler.mapRight
    , positionalHandlers :=
      left.positionalHandlers.map PositionalHandler.mapLeft ++
      right.positionalHandlers.map PositionalHandler.mapRight }

end HandlerBundle

/--
Specification for constructing and finalising a partial parse. The interpreter
maintains a state `σ` while folding handlers; `complete` either produces the
final value or reports a structured error.
-/
structure PartialSpec (σ α : Type) where
  init : σ
  complete : σ → Except Error α

/-- Apply a list of handlers to a single option token. -/
partial def dispatchOption {σ : Type}
    (handlers : List (OptionHandler σ))
    (opt : ParsedOption) (state : σ) (positionals : Array String)
    : Except Error (σ × Array String) :=
  match handlers with
  | [] =>
      throw {
        code := .unexpected
        , subject? := some opt.original
        , detail? := some "no parser field accepted this option"
      }
  | handler :: rest => do
      match ← handler.apply opt state positionals with
      | some (state', remaining) => pure (state', remaining)
      | none => dispatchOption rest opt state positionals

def foldOptions {σ : Type}
    (handlers : List (OptionHandler σ))
    (opts : Array ParsedOption) (state : σ) (positionals : Array String)
    : Except Error (σ × Array String) :=
  opts.foldlM
    (fun acc opt =>
      let (state, remaining) := acc
      dispatchOption handlers opt state remaining)
    (state, positionals)

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

/--
Fold the classified token cursor using the supplied handler bundle.
-/
def HandlerBundle.apply {σ : Type}
    (bundle : HandlerBundle σ) (cursor : TokenCursor) (state : σ) : Except Error σ := do
  let (state, remaining) ←
    foldOptions bundle.optionHandlers cursor.options state cursor.positionals
  foldPositionals bundle.positionalHandlers remaining state

/--
Fold all tokens and immediately complete the partial state using the supplied
specification.
-/
def HandlerBundle.run {σ α : Type}
    (bundle : HandlerBundle σ) (spec : PartialSpec σ α)
    (cursor : TokenCursor) : Except Error α := do
  let state ← bundle.apply cursor spec.init
  spec.complete state

end Native
end Argparse

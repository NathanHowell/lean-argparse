import Std
import Argparse.Basic.Docs
import Argparse.Native.Error
import Argparse.Native.FieldUpdater
import Argparse.Native.Token
import Argparse.Native.Partial
import Argparse.Native.TokenCursor

namespace Argparse
namespace Native

open Token

/-- Result type returned by the native interpreter. -/
abbrev Result (α : Type) := Except Error α

/-- Metadata-only grammar description. -/
structure Grammar (α : Type) where
  usage : Usage

namespace Grammar

/-- Embed a pure value (no usage contribution). -/
def pure {α : Type} (_ : α) : Grammar α :=
  { usage := Usage.empty }

/-- Record a failure branch (no usage contribution). -/
def fail {α : Type} : Grammar α :=
  { usage := Usage.empty }

/-- Map over a grammar without changing usage. -/
def map {α β : Type} (_ : α → β) (g : Grammar α) : Grammar β :=
  { usage := g.usage }

/-- Sequential application combines usage from left to right. -/
def seq {α β : Type} (gf : Grammar (α → β)) (ga : Grammar α) : Grammar β :=
  { usage := Usage.append gf.usage ga.usage }

/-- Primitive positional argument metadata. -/
def positional (doc : PositionalDoc) : Grammar String :=
  { usage := Usage.mergePositional doc Usage.empty }

/-- Flag metadata contributes the provided option doc. -/
def flag (doc : OptionDoc) : Grammar Bool :=
  { usage := Usage.mergeOption doc Usage.empty }

/-- Option metadata contributes the provided option doc. -/
def option (doc : OptionDoc) : Grammar (Option String) :=
  { usage := Usage.mergeOption doc Usage.empty }

/-- Grammar that always fails. Mirrors `Alternative.failure`. -/
def failure {α : Type} : Grammar α := fail

/-- Grammar-level alternative combining usage information for both branches. -/
def orElse {α : Type} (ga : Grammar α) (gb : Unit → Grammar α) : Grammar α :=
  let gb' := gb ()
  { usage := Usage.optional (Usage.append ga.usage gb'.usage) }

/-- Strict helper for `orElse`. -/
def orElse' {α : Type} (ga gb : Grammar α) : Grammar α :=
  orElse ga (fun _ => gb)

end Grammar

instance : Functor Grammar where
  map := Grammar.map

instance : Pure Grammar := ⟨Grammar.pure⟩

instance : Seq Grammar where
  seq gf ga := Grammar.seq gf (ga ())

instance : Applicative Grammar where
  pure := Grammar.pure
  seq := Seq.seq
  map := Functor.map

instance : Alternative Grammar where
  failure := Grammar.failure
  orElse := Grammar.orElse

private def describe {α : Type} [TokenSpec α] (name : α) : String :=
  TokenSpec.describe name

private def missingValueError {α : Type} [TokenSpec α] (name : α) : Error :=
  { code := .missing
    , subject? := some (describe name) }

private def invalidInlineValue (subject : String) : Error :=
  { code := .invalid
    , subject? := some subject
    , detail? := some s!"Flag {subject} does not accept a value" }

private def popFront (args : Array String) : Option (String × Array String) :=
  match args[0]? with
  | none => none
  | some value =>
      let rest := args.extract 1 args.size
      some (value, rest)

/-- Interpreter pairs metadata with a handler bundle and completion spec. -/
structure Interpreter (α : Type) where
  grammar : Grammar α
  state : Type
  bundle : HandlerBundle state
  spec : PartialSpec state α

namespace Interpreter

@[inline] def eval {α : Type} (i : Interpreter α) (cursor : TokenCursor) : Result α :=
  HandlerBundle.run i.bundle i.spec cursor

/-- Functorial map over interpreter results. -/
def map {α β : Type} (i : Interpreter α) (f : α → β) : Interpreter β :=
  { grammar := Grammar.map f i.grammar
    , state := i.state
    , bundle := i.bundle
    , spec :=
        { init := i.spec.init
          , complete := fun state => do
              let value ← i.spec.complete state
              Except.ok (f value) } }

/-- Remaining machinery combines handler bundles and specs across products. -/
private def bundleProduct {σ τ : Type}
    (lhs : HandlerBundle σ) (rhs : HandlerBundle τ) : HandlerBundle (σ × τ) :=
  HandlerBundle.product lhs rhs

private def specProduct {σ τ α β : Type}
    (specF : PartialSpec σ (α → β)) (specA : PartialSpec τ α)
    : PartialSpec (σ × τ) β :=
  { init := (specF.init, specA.init)
    , complete := fun state => do
        let (left, right) := state
        let fn ← specF.complete left
        let arg ← specA.complete right
        Except.ok (fn arg) }

/-- Pure value interpreter. -/
def pure {α : Type} (value : α) : Interpreter α :=
  { grammar := Grammar.pure value
    , state := Unit
    , bundle := { optionHandlers := [], positionalHandlers := [] }
    , spec := { init := (), complete := fun _ => Except.ok value } }

/-- Interpreter that immediately fails with `err`. -/
def fail {α : Type} (err : Error) : Interpreter α :=
  { grammar := Grammar.fail
    , state := Unit
    , bundle := { optionHandlers := [], positionalHandlers := [] }
    , spec := { init := (), complete := fun _ => Except.error err } }

/-- Sequential application of interpreters with thunked right branch. -/
def seq {α β : Type} (ifn : Interpreter (α → β)) (ival : Unit → Interpreter α) : Interpreter β :=
  let ival' := ival ()
  { grammar := Grammar.seq ifn.grammar ival'.grammar
    , state := ifn.state × ival'.state
    , bundle := bundleProduct ifn.bundle ival'.bundle
    , spec := specProduct ifn.spec ival'.spec }

/-- Strict helper for `seq`. -/
def seqApply {α β : Type} (ifn : Interpreter (α → β)) (ival : Interpreter α) : Interpreter β :=
  seq ifn (fun _ => ival)

/-- Primitive positional argument. -/
def positional (doc : PositionalDoc) : Interpreter String :=
  let handler : PositionalHandler (Assigned String) :=
    { apply := fun arg _ => Except.ok (some (Assigned.value arg)) }
  { grammar := Grammar.positional doc
    , state := Assigned String
    , bundle := { optionHandlers := [], positionalHandlers := [handler] }
    , spec :=
        { init := Assigned.unset
          , complete := fun state =>
              Assigned.require
                { code := .missing
                  , subject? := some doc.metavar }
                state } }

/-- Boolean flag that reports presence of the given token. -/
def flag {α : Type} [TokenSpec α] [TokenCursor.ToParsedName α]
    (doc : OptionDoc) (name : α) : Interpreter Bool :=
  let target := TokenCursor.ToParsedName.toParsedName name
  let subject := describe name
  let handler : OptionHandler (Assigned Bool) :=
    OptionHandler.const fun opt _ positionals =>
      if opt.name = target then
        if opt.inlineValue?.isSome then
          Except.error (invalidInlineValue subject)
        else
          Except.ok (some (Assigned.value true, positionals))
      else
        Except.ok none
  { grammar := Grammar.flag doc
    , state := Assigned Bool
    , bundle := { optionHandlers := [handler], positionalHandlers := [] }
    , spec :=
        { init := Assigned.value false
          , complete := fun state =>
              Except.ok (Assigned.getD state false) } }

/-- Option parser returning the associated value when present. -/
def option {α : Type} [TokenSpec α] [TokenCursor.ToParsedName α]
    (doc : OptionDoc) (name : α) : Interpreter (Option String) :=
  let target := TokenCursor.ToParsedName.toParsedName name
  let handler : OptionHandler (Assigned String) :=
    OptionHandler.const fun opt _ positionals =>
      if opt.name = target then
        match opt.inlineValue? with
        | some value => Except.ok (some (Assigned.value value, positionals))
        | none =>
            match popFront positionals with
            | some (value, rest) =>
                Except.ok (some (Assigned.value value, rest))
            | none => Except.error (missingValueError name)
      else
        Except.ok none
  { grammar := Grammar.option doc
    , state := Assigned String
    , bundle := { optionHandlers := [handler], positionalHandlers := [] }
    , spec :=
        { init := Assigned.unset
          , complete := fun state =>
              Except.ok (Assigned.toOption state) } }

/-- Produce an optional result, returning `none` on missing errors. -/
def optional {α : Type} (p : Interpreter α) : Interpreter (Option α) :=
  { grammar := { usage := Usage.optional p.grammar.usage }
    , state := p.state
    , bundle := p.bundle
    , spec :=
        { init := p.spec.init
          , complete := fun state =>
              match p.spec.complete state with
              | Except.ok value => Except.ok (Option.some value)
              | Except.error err =>
                  if err.code = .missing then
                    Except.ok Option.none
                  else
                    Except.error err } }

/-- Supply a constant default when a parser reports a missing error. -/
def withDefault {α : Type} (p : Interpreter α) (value : α) : Interpreter α :=
  map (optional p) (fun opt : Option α => opt.getD value)

/-- Long flag convenience helper. -/
def longFlag (name : String)
    (doc : OptionDoc := { long? := some name, required := false }) : Interpreter Bool :=
  flag doc name

/-- Short flag convenience helper. -/
def shortFlag (name : Char)
    (doc : OptionDoc := { short? := some name, required := false }) : Interpreter Bool :=
  flag doc name

/-- Long option convenience helper. -/
def longOption (name : String)
    (doc : OptionDoc := { long? := some name, required := false }) : Interpreter (Option String) :=
  option doc name

/-- Short option convenience helper. -/
def shortOption (name : Char)
    (doc : OptionDoc := { short? := some name, required := false }) : Interpreter (Option String) :=
  option doc name

instance : Functor Interpreter where
  map f p := map p f

instance : Pure Interpreter := ⟨pure⟩

instance : Seq Interpreter where
  seq pf pa := seq pf pa

instance : Applicative Interpreter where
  pure := pure
  map f p := map p f
  seq := Seq.seq

end Interpreter

end Native
end Argparse

import ArgParse.Builder
import Lean

/-!
# ArgParse.Deriving

Layer 7: the single-declaration front end.

```lean
structure GreetConfig where
  /-- Name to greet. -/
  name : String
  /-- Shout the greeting. -/
  loud : Bool := false
  deriving ArgParse.Parseable
```

generates a `P GreetConfig`: field names become long options, doc-strings become
help text, and structure defaults become parser defaults. There is one
declaration, and it is the one the application already had to write.

This is sugar over the design, not part of it. The handler emits *calls to Layer
3* and nothing else, so everything it generates inherits the Layer 6
correspondence theorems unchanged — there is no second path into the builders
for the proofs to miss.

The handler does no type reflection. Which builder a field needs is decided by
`FieldParser` instance resolution on the field's type, which means adding
support for a new shape of field is an instance, not a change to the macro.
-/

namespace ArgParse

open ArgParse.Builder

/-- A type that knows how to parse itself from the command line. -/
class Parseable (α : Type) where
  /-- The generated parser, paired with its documentation as always. -/
  parser : P α

/-! ### Presentation a field type can carry

A field declaration has nowhere to say "this one is `-v`" or "this one is
positional": the name and the type are all there is. `Arg` puts it in the type,
where a phantom parameter can hold it.

The trick is that `Arg α o` stays *opaque to instance resolution* -- it is a
structure, not an abbreviation -- so `FieldParser` can dispatch on it and read
`o` while the payload type `α` selects which builder to reach for. Nothing about
the handler changes; support arrives entirely as instances.

The cost is a wrapper: the field is `Arg Bool _`, not `Bool`, so use sites read
`cfg.verbose.val`. The instances below pass `Repr`, `DecidableEq`, `ToString`,
and a coercion through, which covers most of it. -/

/-- Command-line presentation for a field, carried in its type. -/
structure Opts where
  /-- Short form to accept in addition to the long one. -/
  short?    : Option Char := none
  /-- Long form, overriding the one derived from the field name. -/
  long?     : Option String := none
  /-- Metavar, overriding the one the `FromArg` instance suggests. -/
  metavar?  : Option String := none
  /-- Whether the field is a positional argument rather than an option. -/
  positional : Bool := false
deriving DecidableEq, Repr, Inhabited

/-- A field of type `α` presented according to `o`.

```lean
structure GreetConfig where
  /-- Enable verbose output. -/
  verbose : Short Bool 'v' := ⟨false⟩
  /-- Name to greet. -/
  name : Positional String
```
-/
structure Arg (α : Type) (o : Opts) where
  /-- The parsed value. -/
  val : α

namespace Arg

instance {α : Type} {o : Opts} [Repr α] : Repr (Arg α o) :=
  ⟨fun a n => reprPrec a.val n⟩

instance {α : Type} {o : Opts} [DecidableEq α] : DecidableEq (Arg α o) :=
  fun a b =>
    if h : a.val = b.val then .isTrue (by cases a; cases b; simp_all)
    else .isFalse (by intro hEq; exact h (congrArg Arg.val hEq))

instance {α : Type} {o : Opts} [ToString α] : ToString (Arg α o) :=
  ⟨fun a => toString a.val⟩

instance {α : Type} {o : Opts} [Inhabited α] : Inhabited (Arg α o) := ⟨⟨default⟩⟩

/-- Read the value out where one is expected. -/
instance {α : Type} {o : Opts} : CoeOut (Arg α o) α := ⟨Arg.val⟩

end Arg

/-- A field with a short form: `verbose : Short Bool 'v'`. -/
abbrev Short (α : Type) (c : Char) := Arg α { short? := some c }

/-- A positional field: `name : Positional String`. -/
abbrev Positional (α : Type) := Arg α { positional := true }

/-- A field with a short form and an explicit long form:
`outDir : Named String 'o' "out"`. -/
abbrev Named (α : Type) (c : Char) (long : String) :=
  Arg α { short? := some c, long? := some long }

/-- How a structure field of this type becomes an item.

Resolution on the field type is what selects the builder: `Bool` becomes a flag,
`Option α` an optional option, `List α` a repeatable one, and anything with a
`FromArg` instance a plain option. Wrapping any of those in `Arg` keeps the
choice and adds the presentation the wrapper carries. -/
class FieldParser (α : Type) where
  /-- Build the item for a field with this long name, help text, and structure
  default (when it has one). -/
  field (long : String) (help : String) (default? : Option α) : P α

/-- A `Bool` field is a flag: absent is `false`, present is `true`. -/
instance (priority := high) : FieldParser Bool where
  field long help _ := flag long none help

/-- An `Option α` field is an option that may be absent. -/
instance {α : Type} [FromArg α] : FieldParser (Option α) where
  field long help _ := optionOpt α long none none help

/-- A `List α` field is a repeatable option. -/
instance {α : Type} [FromArg α] : FieldParser (List α) where
  field long help _ := options α long none none help

/-- Any other decodable field is an option, required unless the structure gave
it a default. -/
instance {α : Type} [FromArg α] [ToString α] : FieldParser α where
  field long help
    | some d => optionD long d none none help
    | none => option α long none none help

/-! ### Fields carrying their own presentation

One instance per payload shape, mirroring the four above. Each reads the long
name, short form, and metavar out of the phantom `Opts` and otherwise reaches for
the same builder the unwrapped type would have. -/

/-- The long name to use: the type's override, else the field name. -/
@[inline] private def Opts.longName (o : Opts) (fieldName : String) : String :=
  o.long?.getD fieldName

/-- A wrapped `Bool` is still a flag, now with a short form. -/
instance {o : Opts} : FieldParser (Arg Bool o) where
  field long help _ := Arg.mk <$> flag (o.longName long) o.short? help

/-- A wrapped `Option α` is still an optional option. -/
instance {α : Type} {o : Opts} [FromArg α] : FieldParser (Arg (Option α) o) where
  field long help _ :=
    Arg.mk <$> optionOpt α (o.longName long) o.short? o.metavar? help

/-- A wrapped `List α` is still repeatable. -/
instance {α : Type} {o : Opts} [FromArg α] : FieldParser (Arg (List α) o) where
  field long help _ :=
    if o.positional then
      Arg.mk <$> args α (o.longName long) (some (o.metavar?.getD (o.longName long).toUpper)) help
    else
      Arg.mk <$> options α (o.longName long) o.short? o.metavar? help

/-- Anything else wrapped is an option, or a positional when the type says so.

A positional with a structure default reads as "optional, falling back" -- there
is no such thing as a required argument that may be omitted. -/
instance {α : Type} {o : Opts} [FromArg α] [ToString α] : FieldParser (Arg α o) where
  field long help default? :=
    let name := o.longName long
    Arg.mk <$>
      (if o.positional then
        let mv := some (o.metavar?.getD name.toUpper)
        match default? with
        | some d => (fun v => v.getD d.val) <$> argOpt α name mv help
        | none => arg α name mv help
      else
        match default? with
        | some d => optionD name d.val o.short? o.metavar? help
        | none => option α name o.short? o.metavar? help)

namespace Deriving

/-- Convert a Lean field name to its command-line spelling: `dryRun` becomes
`dry-run`, which is what a user expects to type. -/
def kebabCase (s : String) : String :=
  s.toList.foldl
    (fun acc c =>
      if c.isUpper then acc ++ "-" ++ (Char.toLower c).toString else acc ++ c.toString)
    ""

end Deriving

end ArgParse

namespace ArgParse.Deriving

open Lean Elab Command Meta PrettyPrinter

/-- Build the `Parseable` instance for one structure. -/
def mkParseableInstance (declName : Name) : CommandElabM Bool := do
  let env ← getEnv
  unless isStructure env declName do
    throwError "deriving Parseable: {declName} is not a structure"
  let some info := env.find? declName
    | throwError "deriving Parseable: {declName} is not declared"
  unless info.type.isSort do
    throwError
      "deriving Parseable: {declName} takes parameters, which the handler does not support"
  let fields := getStructureFields env declName
  if fields.isEmpty then
    throwError "deriving Parseable: {declName} has no fields to parse"
  let ctor := getStructureCtor env declName
  let mut terms : Array Term := #[]
  for f in fields do
    let doc := ((← liftCoreM (findDocString? env (declName ++ f))).getD "").trim
    let helpStx := Syntax.mkStrLit doc
    let longStx := Syntax.mkStrLit (kebabCase f.toString)
    let defaultStx ← match getDefaultFnForField? env declName f with
      | none => `(none)
      | some dfn => do
          let some dinfo := env.find? dfn
            | throwError "deriving Parseable: cannot see the default for {declName}.{f}"
          if dinfo.type.isForall then
            throwError
              "deriving Parseable: the default for {declName}.{f} depends on another \
               field, which a command-line default cannot; give it a literal default \
               or drop it"
          let some value := dinfo.value?
            | throwError "deriving Parseable: cannot see the default for {declName}.{f}"
          -- Structure defaults are stored wrapped (`id true`, not `true`), so the
          -- check has to reduce before it can recognise one. `Arg Bool _` has to
          -- be peeled as well, or a `Short Bool 'v' := ⟨true⟩` would slip past the
          -- guard that plain `Bool := true` trips.
          let defaultsToTrue ← liftTermElabM do
            let value ← whnf value
            if dinfo.type.isConstOf ``Bool then
              return value.isConstOf ``Bool.true
            if dinfo.type.isAppOfArity ``ArgParse.Arg 2
                && dinfo.type.appFn!.appArg!.isConstOf ``Bool then
              if value.isAppOfArity ``ArgParse.Arg.mk 3 then
                return (← whnf value.appArg!).isConstOf ``Bool.true
            return false
          if defaultsToTrue then
            throwError
              "deriving Parseable: {declName}.{f} is a Bool defaulting to true, which no \
               flag can express -- a flag that is absent means false. Model it as the \
               negated field, or as an Option Bool option"
          -- The default's *value*, delaborated, rather than a reference to the
          -- `_default` constant: naming that constant in compiled code trips a
          -- backend crash in the toolchain, and the value is what we mean anyway.
          let valueStx ← liftTermElabM (PrettyPrinter.delab value)
          `(some $valueStx)
    terms := terms.push (← `(ArgParse.FieldParser.field $longStx $helpStx $defaultStx))
  let ctorId := mkIdent ctor.name
  let head ← `($ctorId <$> $(terms[0]!))
  let body ← terms[1:].foldlM (fun acc t => `($acc <*> $t)) head
  let declId := mkIdent declName
  elabCommand (← `(instance : ArgParse.Parseable $declId where parser := $body))
  return true

/-- `deriving Parseable` handler. -/
def parseableHandler (declNames : Array Name) : CommandElabM Bool := do
  for declName in declNames do
    unless ← mkParseableInstance declName do
      return false
  return true

initialize registerDerivingHandler ``ArgParse.Parseable parseableHandler

end ArgParse.Deriving

namespace ArgParse

/-- The generated parser for a type that derives `Parseable`. -/
@[inline] def parserFor (α : Type) [Parseable α] : P α := Parseable.parser

end ArgParse

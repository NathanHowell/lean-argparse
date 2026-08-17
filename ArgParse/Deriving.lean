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

/-- How a structure field of this type becomes an item.

Resolution on the field type is what selects the builder: `Bool` becomes a flag,
`Option α` an optional option, `List α` a repeatable one, and anything with a
`FromArg` instance a plain option. -/
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
          -- check has to reduce before it can recognise one.
          let reduced ← liftTermElabM (whnf value)
          if dinfo.type.isConstOf ``Bool && reduced.isConstOf ``Bool.true then
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

import ArgParse.P
import ArgParse.Core.Scan

/-!
# ArgParse.Builder

The only place a `Doc` and a `Parser` are zipped together.

Every builder takes one set of arguments and produces both sides from it: the
typed runtime spec Layer 1's scanner consumes, and the erased `ItemSpec` the
renderers read. Divergence between what help promises and what the parser
accepts is expressible in this file and nowhere else, which is what makes it
something to prove away (Layer 6) rather than police at call sites.

Haskell's `Mod` monoid exists because Haskell has no keyword arguments. Lean
does, so the modifiers are named parameters with defaults.
-/

namespace ArgParse.Builder

open ArgParse
open ArgParse.Spec

/-! ### Shared construction

`short?` is the delicate one. `Spec.Short` carries a proof that the character is
neither `-` nor outside ASCII, so a builder taking a plain `Char` has to decide
what an inadmissible one means. It means "no short form" — and crucially, the
`ItemSpec` below reads its short form back off the constructed `Short`, not off
the caller's `Char`. A rejected character therefore disappears from the help text
and from the scanner together, instead of being advertised but unparseable. -/

/-- Promote a character to a short-flag name, rejecting `-` and non-ASCII. -/
def mkShort? (c? : Option Char) : Option Short :=
  match c? with
  | .none => .none
  | .some c =>
      if h : c ≠ '-' ∧ c.toNat < 128 then .some ⟨c, h⟩ else .none

/-- Metadata shared by every builder. -/
@[inline] private def mkMeta (name : String) (help : String) (metavar? : Option String)
    (default? : Option String) : Meta :=
  { name := name
  , help? := if help.isEmpty then none else some help
  , metavar? := metavar?
  , default? := default? }

/-! ### Flags -/

/-- The runtime spec and the erased item a flag is built from.

Exposed rather than inlined so Layer 6 can state agreement between the two
halves, and so a proof about the scanner can be transported to a statement about
the document. -/
def flagParts (long : String) (short : Option Char) (help : String)
    (hidden : Bool) : FlagSpec × ItemSpec :=
  let short? := mkShort? short
  let spec : FlagSpec :=
    { short? := short?
    , long? := some long
    , «meta» := mkMeta long help none none
    , hidden? := hidden }
  let item : ItemSpec :=
    { kind := .flag
    , name := long
    , short? := short?.map (·.c)
    , long? := some long
    , help? := if help.isEmpty then none else some help
    , arity := .zero
    , required := false
    , hidden := hidden }
  (spec, item)

/-- A boolean flag: `true` when present anywhere in the stream, `false` otherwise.

```
flag "verbose" (short := 'v') (help := "Enable verbose output.")
```
-/
def flag (long : String) (short : Option Char := none) (help : String := "")
    (hidden : Bool := false) : P Bool :=
  let parts := flagParts long short help hidden
  { doc := .item parts.snd, run := Core.flagScan parts.fst }

/-! ### Options

Defaults are typed (`default? : Option α`) rather than pre-rendered strings, and
the help text is produced from the same value with `ToString`. A `defaultText`
parameter alongside a typed default would be a second declaration to keep in
sync -- precisely the drift this layer exists to remove -- so the `ToString`
constraint is the price of the guarantee. -/

/-- Build the runtime spec and erased item for an option from shared arguments. -/
def optParts (α : Type) [FromArg α]
    (long : String) (short : Option Char) (metavar? : Option String)
    (help : String) (defaultText? : Option String) (arity : Arity) (required : Bool)
    (hidden : Bool) : OptSpec α × ItemSpec :=
  let short? := mkShort? short
  let metavar := metavar?.getD (FromArg.metavar (α := α))
  let spec : OptSpec α :=
    { short? := short?
    , long? := some long
    , «meta» := mkMeta long help (some metavar) defaultText?
    , arity := arity
    , hidden? := hidden }
  let item : ItemSpec :=
    { kind := .option
    , name := long
    , short? := short?.map (·.c)
    , long? := some long
    , metavar? := some metavar
    , help? := if help.isEmpty then none else some help
    , arity := arity
    , choices? := FromArg.choices (α := α)
    , default? := defaultText?
    , required := required
    , hidden := hidden }
  (spec, item)

/-- Read every value supplied for an option, in order.

Shared by all four option builders, which differ only in how they read this
list: `optionOpt` takes the last as an `Option`, `optionD` falls back to a
default, `option` errors when it is empty, `options` returns it whole. That is
why the behavioural correspondence proofs establish acceptance once here and
derive the four builders as corollaries. -/
@[inline] def optionValues {α : Type} [FromArg α] (spec : OptSpec α) :
    Parser (List α) := fun st =>
  match Core.collectOptionScanValues spec st with
  | .ok (values, _, st') => .ok values st'
  | .error err => .err err

/-- An option that may be absent, yielding `none`.

```
optionOpt (α := Nat) "count" (short := 'n') (help := "How many times.")
```
-/
def optionOpt (α : Type) [FromArg α] (long : String) (short : Option Char := none)
    (metavar : Option String := none) (help : String := "")
    (hidden : Bool := false) : P (Option α) :=
  let (spec, item) := optParts α long short metavar help none .one false hidden
  { doc := .optionalItem item
  , run := fun st =>
      match optionValues spec st with
      | .ok values st' => .ok values.getLast? st'
      | .err err => .err err }

/-- An option with a default, which is therefore never missing.

The rendered default in help comes from the same value the parser falls back to. -/
def optionD {α : Type} [FromArg α] [ToString α] (long : String) (default : α)
    (short : Option Char := none) (metavar : Option String := none)
    (help : String := "") (hidden : Bool := false) : P α :=
  let (spec, item) := optParts α long short metavar help (some (toString default)) .one false hidden
  { doc := .optionalItem item
  , run := fun st =>
      match optionValues spec st with
      | .ok values st' => .ok (values.getLast?.getD default) st'
      | .err err => .err err }

/-- A required option: absence is a `missingValue` error. -/
def option (α : Type) [FromArg α] (long : String) (short : Option Char := none)
    (metavar : Option String := none) (help : String := "")
    (hidden : Bool := false) : P α :=
  let (spec, item) := optParts α long short metavar help none .one true hidden
  { doc := .item item
  , run := fun st =>
      match optionValues spec st with
      | .ok values st' =>
          match values.getLast? with
          | Option.some value => .ok value st'
          | Option.none =>
              .err { kind := .missingValue, context := [], expect := [.optionVal long] }
      | .err err => .err err }

/-- A repeatable option collecting every value supplied. -/
def options (α : Type) [FromArg α] (long : String) (short : Option Char := none)
    (metavar : Option String := none) (help : String := "")
    (hidden : Bool := false) : P (List α) :=
  let (spec, item) := optParts α long short metavar help none .many false hidden
  { doc := .repeatedItem item false, run := optionValues spec }

/-- A required `String` option. -/
@[inline] def strOption (long : String) (short : Option Char := none)
    (metavar : Option String := none) (help : String := "")
    (hidden : Bool := false) : P String :=
  option String long short metavar help hidden

/-! ### Positionals -/

/-- Build the runtime spec and erased item for a positional. -/
def posParts (α : Type) [FromArg α]
    (name : String) (metavar? : Option String) (help : String) (arity : Arity)
    (required : Bool) (hidden : Bool) : PosSpec α × ItemSpec :=
  let metavar := metavar?.getD name
  let spec : PosSpec α :=
    { «meta» := mkMeta name help (some metavar) none
    , arity := arity
    , hidden? := hidden }
  let item : ItemSpec :=
    { kind := .positional
    , name := name
    , metavar? := some metavar
    , help? := if help.isEmpty then none else some help
    , arity := arity
    , choices? := FromArg.choices (α := α)
    , required := required
    , hidden := hidden }
  (spec, item)

/-- A required positional argument. -/
def arg (α : Type) [FromArg α] (name : String) (metavar : Option String := none)
    (help : String := "") (hidden : Bool := false) : P α :=
  let (spec, item) := posParts α name metavar help .one true hidden
  { doc := .item item
  , run := fun st =>
      match Core.takePositionalValue? spec st with
      | .ok (Option.some (value, _), st') => .ok value st'
      | .ok (Option.none, _) =>
          -- The metavar, not the field name: it is what usage shows and what the
          -- user has to type.
          .err { kind := .missingValue, context := [], expect := [.positional item.metavar] }
      | .error err => .err err }

/-- An optional positional argument. -/
def argOpt (α : Type) [FromArg α] (name : String) (metavar : Option String := none)
    (help : String := "") (hidden : Bool := false) : P (Option α) :=
  let (spec, item) := posParts α name metavar help .one false hidden
  { doc := .optionalItem item
  , run := fun st =>
      match Core.takePositionalValue? spec st with
      | .ok (value?, st') => .ok (value?.map Prod.fst) st'
      | .error err => .err err }

/-- Every remaining positional argument. -/
def args (α : Type) [FromArg α] (name : String) (metavar : Option String := none)
    (help : String := "") (hidden : Bool := false) : P (List α) :=
  let (spec, item) := posParts α name metavar help .many false hidden
  { doc := .repeatedItem item false
  , run := fun st =>
      match Core.collectPositionalValues spec st with
      | .ok (values, _, st') => .ok values st'
      | .error err => .err err }

/-- A required `String` positional. -/
@[inline] def positional (name : String) (metavar : Option String := none)
    (help : String := "") (hidden : Bool := false) : P String :=
  arg String name metavar help hidden

end ArgParse.Builder

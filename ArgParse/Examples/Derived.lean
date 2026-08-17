import ArgParse.Exec
import ArgParse.Deriving

/-!
# ArgParse.Examples.Derived

The Layer 7 front end, end to end.

One declaration per configuration, with the doc-strings doing double duty as help
text. What a field name cannot say -- the short form, that a field is positional,
a metavar the `FromArg` instance would not guess -- travels in the field's type
instead, so it stays one declaration.

Plain and wrapped fields mix freely, as do derived and hand-written commands
inside one `Cmd`.
-/

namespace ArgParse.Examples

open ArgParse

/-- Options for a `build` verb. -/
structure BuildConfig where
  /-- Directory to build into. -/
  outDir : Arg String { short? := some 'o', metavar? := some "DIR" } := ⟨"build"⟩
  /-- Compile without optimisations. -/
  debug : Short Bool 'd' := ⟨false⟩
  /-- Number of parallel jobs. -/
  jobs : Nat := 1
  /-- Targets to build; repeatable. -/
  target : Short (List String) 't' := ⟨[]⟩
  /-- Override the toolchain to use. -/
  toolchain : Option String := none
  deriving Repr, DecidableEq, ArgParse.Parseable

/-- Options for a `clean` verb, taking the directory positionally. -/
structure CleanConfig where
  /-- Directory to clean. -/
  outDir : Positional String := ⟨"build"⟩
  /-- Remove the directory itself as well. -/
  purge : Short Bool 'p' := ⟨false⟩
  deriving Repr, DecidableEq, ArgParse.Parseable

/-- What the example CLI can be asked to do. -/
inductive Verb where
  /-- Build with the given configuration. -/
  | build (cfg : BuildConfig)
  /-- Clean with the given configuration. -/
  | clean (cfg : CleanConfig)
  deriving Repr, DecidableEq

/-- A two-verb CLI whose every item came from a structure field. -/
def derivedApp : Cmd Verb :=
  .node "builder" { name := "builder", help? := some "Toy build tool." }
    (pure id)
    [ .leaf "build" { name := "build", help? := some "Compile the project." }
        (Verb.build <$> parserFor BuildConfig)
    , .leaf "clean" { name := "clean", help? := some "Remove build output." }
        (Verb.clean <$> parserFor CleanConfig) ]

end ArgParse.Examples

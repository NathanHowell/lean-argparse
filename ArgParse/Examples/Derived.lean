import ArgParse.Exec
import ArgParse.Deriving

/-!
# ArgParse.Examples.Derived

The Layer 7 front end, end to end.

Compare with `Main.lean`, which writes the same shape of CLI through Layer 3 by
hand. The derived form is one declaration instead of two, and the doc-strings do
double duty as help text. What it gives up is anything a structure field has
nowhere to say: short forms, positionals, and per-item metavars. Those remain
available by writing the builders directly, which is what the handler emits
anyway.
-/

namespace ArgParse.Examples

open ArgParse

/-- Options for a `build` verb. -/
structure BuildConfig where
  /-- Directory to build into. -/
  outDir : String := "build"
  /-- Compile without optimisations. -/
  debug : Bool := false
  /-- Number of parallel jobs. -/
  jobs : Nat := 1
  /-- Targets to build; repeatable. -/
  target : List String := []
  /-- Override the toolchain to use. -/
  toolchain : Option String := none
  deriving Repr, DecidableEq, ArgParse.Parseable

/-- Options for a `clean` verb. -/
structure CleanConfig where
  /-- Directory to clean. -/
  outDir : String := "build"
  /-- Remove the directory itself as well. -/
  purge : Bool := false
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

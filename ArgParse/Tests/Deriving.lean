import ArgParse.Examples.Derived

/-!
# ArgParse.Tests.Deriving

Checks for the Layer 7 handler: that field names, doc-strings, and defaults all
arrive where they should, and that the generated parser behaves like the
hand-written builders it is made of.
-/

namespace ArgParse.Tests

open ArgParse ArgParse.Examples ArgParse.Spec

private def expectTrue (cond : Bool) (msg : String) : Except String Unit :=
  if cond then .ok () else .error msg

/-- Field names reach the command line in kebab-case. -/
private def checkKebabCase : Except String Unit := do
  expectTrue (Deriving.kebabCase "outDir" == "out-dir")
    s!"expected out-dir, got {Deriving.kebabCase "outDir"}"
  expectTrue (Deriving.kebabCase "jobs" == "jobs")
    s!"expected jobs, got {Deriving.kebabCase "jobs"}"

/-- Every field becomes exactly one item, named after itself. -/
private def checkItems : Except String Unit := do
  let items := (parserFor BuildConfig).items
  expectTrue (items.length == 5) s!"expected five items, got {items.length}"
  let names := items.map (·.name)
  expectTrue (names == ["out-dir", "debug", "jobs", "target", "toolchain"])
    s!"unexpected item names: {names}"

/-- A short form declared in the field's type reaches the item. -/
private def checkShortForms : Except String Unit := do
  let items := (parserFor BuildConfig).items
  let shortOf (name : String) : Option (Option Char) :=
    (items.find? (fun i => i.name == name)).map (·.short?)
  expectTrue (shortOf "out-dir" == some (some 'o')) s!"out-dir: {repr (shortOf "out-dir")}"
  expectTrue (shortOf "debug" == some (some 'd')) s!"debug: {repr (shortOf "debug")}"
  expectTrue (shortOf "target" == some (some 't')) s!"target: {repr (shortOf "target")}"
  expectTrue (shortOf "jobs" == some none) s!"an unwrapped field has no short form: {repr (shortOf "jobs")}"

/-- A metavar declared in the field's type overrides the one the `FromArg`
instance suggests. -/
private def checkMetavarOverride : Except String Unit :=
  match ((parserFor BuildConfig).items).find? (fun i => i.name == "out-dir") with
  | some item =>
      expectTrue (item.metavar? == some "DIR") s!"expected DIR, got {repr item.metavar?}"
  | none => .error "no item for the outDir field"

/-- A `Positional` field becomes a positional with an upper-cased metavar. -/
private def checkPositional : Except String Unit :=
  match ((parserFor CleanConfig).items).find? (fun i => i.name == "out-dir") with
  | some item => do
      expectTrue (item.kind == .positional) s!"expected a positional, got {repr item.kind}"
      expectTrue (item.metavar? == some "OUT-DIR") s!"expected OUT-DIR, got {repr item.metavar?}"
  | none => .error "no item for the outDir field"

/-- Short forms and positionals parse the way their types promise. -/
private def checkWrappedParses : Except String Unit := do
  match Exec.exec derivedApp ["build", "-o", "dist", "-d", "-t", "a"] with
  | .ok (.build cfg) =>
      expectTrue (cfg.outDir.val == "dist" && cfg.debug.val && cfg.target.val == ["a"])
        s!"unexpected build payload: {repr cfg}"
  | other => .error s!"expected a build payload, got {repr other}"
  match Exec.exec derivedApp ["clean", "tmp", "-p"] with
  | .ok (.clean cfg) =>
      expectTrue (cfg.outDir.val == "tmp" && cfg.purge.val)
        s!"unexpected clean payload: {repr cfg}"
  | other => .error s!"expected a clean payload, got {repr other}"

/-- Doc-strings become help text, and structure defaults become rendered
defaults. -/
private def checkHelpMetadata : Except String Unit := do
  let items := (parserFor BuildConfig).items
  match items.find? (fun i => i.name == "jobs") with
  | some item =>
      expectTrue (item.help? == some "Number of parallel jobs.")
        s!"expected the doc-string as help, got {repr item.help?}"
      expectTrue (item.default? == some "1")
        s!"expected the structure default to be rendered, got {repr item.default?}"
      expectTrue (!item.required) "a field with a default is not required"
  | none => .error "no item for the jobs field"

/-- Field types select the builder: Bool is a flag, List repeats, Option is
optional, everything else is an option. -/
private def checkKinds : Except String Unit := do
  let items := (parserFor BuildConfig).items
  let kindOf (name : String) : Option (ItemKind × Arity) :=
    (items.find? (fun i => i.name == name)).map (fun i => (i.kind, i.arity))
  expectTrue (kindOf "debug" == some (.flag, .zero)) s!"debug: {repr (kindOf "debug")}"
  expectTrue (kindOf "target" == some (.option, .many)) s!"target: {repr (kindOf "target")}"
  expectTrue (kindOf "toolchain" == some (.option, .one)) s!"toolchain: {repr (kindOf "toolchain")}"
  expectTrue (kindOf "jobs" == some (.option, .one)) s!"jobs: {repr (kindOf "jobs")}"

/-- The generated parser parses. -/
private def checkParses : Except String Unit := do
  match Exec.exec derivedApp
      ["build", "--out-dir", "dist", "--debug", "--jobs=4", "--target", "a", "--target", "b"] with
  | .ok (.build cfg) =>
      expectTrue (cfg == { outDir := ⟨"dist"⟩, debug := ⟨true⟩, jobs := 4
                         , target := ⟨["a", "b"]⟩, toolchain := none })
        s!"unexpected payload: {repr cfg}"
  | other => .error s!"expected a build payload, got {repr other}"

/-- Omitted fields fall back to the structure's own defaults. -/
private def checkDefaultsApplied : Except String Unit := do
  match Exec.exec derivedApp ["build"] with
  | .ok (.build cfg) =>
      expectTrue (cfg == { outDir := ⟨"build"⟩, debug := ⟨false⟩, jobs := 1
                         , target := ⟨[]⟩, toolchain := none })
        s!"expected the structure defaults, got {repr cfg}"
  | other => .error s!"expected a build payload, got {repr other}"

/-- Help for a derived command lists every field. -/
private def checkHelp : Except String Unit :=
  match Exec.exec derivedApp ["build", "--help"] with
  | .output text =>
      ((parserFor BuildConfig).items).foldl
        (fun acc item =>
          acc.bind fun _ =>
            expectTrue ((text.splitOn ("--" ++ item.name)).length ≥ 2)
              s!"help omits --{item.name}")
        (Except.ok ())
  | other => .error s!"expected help output, got {repr other}"

/-- Deriving checks executed by `lake test`. -/
def derivingChecks : List (String × Except String Unit) :=
  [ ("kebab-case field names", checkKebabCase)
  , ("one item per field", checkItems)
  , ("short forms from field types", checkShortForms)
  , ("metavar override from field types", checkMetavarOverride)
  , ("positional from field types", checkPositional)
  , ("wrapped fields parse", checkWrappedParses)
  , ("doc-strings and defaults reach help", checkHelpMetadata)
  , ("field types select builders", checkKinds)
  , ("generated parser parses", checkParses)
  , ("structure defaults applied", checkDefaultsApplied)
  , ("help lists every field", checkHelp)
  ]

end ArgParse.Tests

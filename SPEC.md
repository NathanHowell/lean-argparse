Lean4 argparse — library specification

0) Goals & constraints
	•	Pure Lean 4, no external deps beyond Lean/Std.
	•	Applicative-first parser combinators for subcommands, flags, options, and positionals; instances for Functor, Applicative, Alternative (and optional Monad only where ergonomic).
	•	POSIX-ish semantics with -- sentinel to end option parsing; support short flags including numeric ones (e.g. -0 like xargs).
	•	Single source of truth (spec/grammar) used to:
	•	parse argv (deterministic, total);
	•	generate --help (Clap-style sections & formatting);
	•	generate manpages (mdoc(7) or simple man(7) roff);
	•	generate completion scripts (bash/zsh/fish).
	•	Correctness proofs over simple data structures; proofs shipped alongside the code.
	•	Incremental build with tests and docstrings at every step.

⸻

1) Module layout

ArgParse/
  Core/
    Types.lean            -- core types (State, Result, Error, Doc, Shell, etc.)
    Parser.lean           -- Parser α & instances; stepping semantics
    Value.lean            -- FromArg α class & instances (String, Int, Nat, Bool, Enum…)
    Combinators.lean      -- flags/options/positionals/subcommands DSL
    Normalize.lean        -- argv normalization & `--` sentinel partition
  Spec/
    AST.lean              -- command/spec tree (the single source of truth)
    Elab.lean             -- build Parser from AST (Applicative)
    Describe.lean         -- derive docs/completions/manpage from AST
  Doc/
    Help.lean             -- Clap-style help rendering (String)
    Man.lean              -- roff mdoc generator (String)
    Completion.lean       -- bash/zsh/fish script emitters (String)
  Proofs/
    Laws.lean             -- Functor/Applicative/Alternative law lemmas
    Totality.lean         -- totality/termination
    Determinism.lean      -- uniqueness of parse result (when spec is unambiguous)
    Sentinel.lean         -- properties of `--` partition
    Soundness.lean        -- parsed value satisfies declarative semantics
  CLI/
    Print.lean            -- stock `--help`, `--man`, `--generate-completions`
  Examples/
    Xargs0.lean           -- demo supporting `-0`
    GitLike.lean          -- subcommands demo (e.g. `init`, `commit`)
  Tests/
    Unit.lean
    Golden.lean


⸻

2) Core types (proof-friendly)

namespace ArgParse

/-- Raw tokens, typically `List String` from `argv`. -/
abbrev Tokens := List String

/-- Input state for the parser. -/
structure State where
  pre    : List String  -- tokens before `--` (if present)
  post   : List String  -- tokens after  `--` (pure positionals)
  cursor : Nat          -- index into the *flattened* stream (pre ++ ["--"?] ++ post)
deriving Repr, DecidableEq

/-- Parser result with remaining state or a structured error. -/
inductive Result (α : Type) where
  | ok  : α → State → Result α
  | err : Error → Result α

/-- Minimal, structured error information (no exceptions). -/
structure Error where
  kind    : ErrorKind
  context : List String   -- nearby tokens (for messages)
  expect  : List Expect   -- expected things for diagnostics
deriving Repr, DecidableEq

/-- Expectations used for helpful errors. -/
inductive Expect
  | flag (short? : Option Char) (long? : Option String)
  | optionVal (name : String)
  | positional (name : String)
  | subcommand (name : String)
  | endOfInput
deriving Repr, DecidableEq

/-- Error kinds (unknown flag, missing value, etc.). -/
inductive ErrorKind
  | unknownShort | unknownLong | missingValue | leftover | conflict | custom
deriving Repr, DecidableEq

/-- The parser type. Total and pure. -/
def Parser (α : Type) := State → Result α

Instances & laws
	•	Provide instance : Functor Parser, Applicative, Alternative.
	•	Prove standard laws (where meaningful) in Proofs/Laws.lean:
	•	map_const, map_id, seq_pure, pure_seq, seq_map_assoc, etc.
	•	Optional Monad for do-notation; avoid relying on it for spec power.

⸻

3) Normalization & -- sentinel
	•	Function normalize : Tokens → State:
	•	Splits on first --.
	•	pre scanned for flags/options/subcommand and positionals are allowed here as specified by the grammar.
	•	post are always positionals, even if they look like options.
	•	Proofs:
	•	Sentinel.post_is_positional: tokens in post are never consumed as options/flags.
	•	Sentinel.stability: inserting/removing -- only affects classification, not the textual token content.

⸻

4) Value parsing typeclass

/-- Convert a single token to a value for options/positionals. -/
class FromArg (α : Type) where
  parse : String → Except String α
  -- Optional hints for completions & docs:
  metavar : String := "VALUE"
  choices : Option (List String) := none

namespace FromArg
-- Instances: String, Substring, Int, Nat, Bool (accepts "true"/"false"/"1"/"0"),
-- bounded integers via simple parsers; enumeration via `DecidableEq` helper.
end FromArg

No dependencies beyond core. Enumeration helper:

def enumFrom (xs : List (String × α)) : FromArg α


⸻

5) Spec AST: single source of truth

/-- A short flag is *any ASCII char except '-'*, including digits (supports `-0`). -/
structure Short where c : Char
  ok : c ≠ '-' ∧ c.toNat < 128

structure Meta where
  name        : String            -- displayed name
  help?       : Option String     -- short help line
  longHelp?   : Option String     -- longer description
  metavar?    : Option String     -- override FromArg.metavar
  env?        : Option String     -- environment variable name to mention
  default?    : Option String     -- human-readable default for docs
deriving Inhabited

inductive Arity
  | zero    -- flag
  | one     -- single value
  | many    -- repeats & collects (List α)
  | some    -- 1+ values (List α)

structure FlagSpec where
  short?     : Option Short
  long?      : Option String
  meta       : Meta
  exclusive? : Bool := false      -- e.g., conflicts with another group
  hidden?    : Bool := false

structure OptSpec (α : Type) [FromArg α] where
  short?     : Option Short
  long?      : Option String
  meta       : Meta
  arity      : Arity := .one
  concatVal? : Bool := true       -- accept `-n5` if `.one`
  eqVal?     : Bool := true       -- accept `--name=value`
  repeatable : Bool := (arity ≠ .one)  -- collect list if many/some

structure PosSpec (α : Type) [FromArg α] where
  meta       : Meta
  arity      : Arity := .one

/-- Subcommands form a tree; payload type may depend on the chosen command. -/
inductive CmdSpec : Type
  | node (name : String) (meta : Meta) (args : List ItemSpec) (subs : List CmdSpec)
-- Items inside a command:
with ItemSpec : Type
  | flag  (s : FlagSpec)
  | opt   {α} [FromArg α] (s : OptSpec α)
  | pos   {α} [FromArg α] (s : PosSpec α)

App-level descriptor

structure AppSpec where
  name        : String
  version?    : Option String := none
  about?      : Option String := none
  epilog?     : Option String := none
  root        : CmdSpec


⸻

6) Building parsers (Applicative DSL)

Ergonomic combinators that declare and return values:

/-- Boolean flag parser. -/
def flag (f : FlagSpec) : Parser Bool

/-- Option with value(s). -/
def opt {α} [FromArg α] (s : OptSpec α) : Parser (match s.arity with
  | .zero => PUnit
  | .one  => α
  | .many => List α
  | .some => List α)

/-- Positional(s). -/
def pos {α} [FromArg α] (s : PosSpec α) : Parser (…same shape…)

/-- Subcommand: returns a dependent pair of the chosen name & payload. -/
def subcommands (cs : List CmdSpec) : Parser (Σ name : String, Payload name)

-- Combinator sugar:
infixl:4  "<|>" => Alternative.orElse
notation:max p "?" => optional p

Design note: the actual Parser is derived from CmdSpec/ItemSpec via Spec/Elab.lean. The user-facing DSL helps construct that AST Applicatively:

/-- Builder DSL, Applicative to accumulate spec & an extractor for results. -/
structure Builder (α : Type) :=
  (spec : List ItemSpec) (parser : Parser α)

-- Provide `flagB : FlagSpec → Builder Bool`, `optB`, `posB`, `mapB`, `<*>`, etc.
-- `commandB : (name : String) → Meta → Builder α → List (Builder …) → CmdSpec × Parser α`

This keeps the single source of truth while giving a friendly Applicative style.

⸻

7) Semantics & determinism
	•	Left-to-right, single pass over pre then post.
	•	Option recognition:
	•	--long=value and --long value if eqVal? / space form allowed.
	•	-xVALUE if concatVal? and arity .one.
	•	Bundles of short flags allowed (-abc ≡ -a -b -c), including digits (so -0v is ok).
	•	Subcommand: first non-option token that matches a declared subcommand name selects it and switches to that command’s parser.
	•	Positionals: consumed in declared order; many/some are greedy within their scope.
	•	Sentinel --: all tokens after move to post and are matched only by positionals.

Determinism theorem: For an unambiguous spec (no duplicate names, non-overlapping short/long flags, single subcommand match), parse spec argv yields at most one .ok result. Proved by induction on token stream and structure of CmdSpec.

⸻

8) Help, manpage, completions (derived)

Clap-style help

Render sections (conditionally present):
	•	Usage: canonical synopsis per command (includes subcommands).
	•	Commands: table (name + short help).
	•	Arguments: (positionals) with metavars & arity.
	•	Options: flags & options with -s, --long <META> columns, default/env notes.
	•	Examples: (if provided in meta/epilog).
	•	Formatting: align columns; wrap to width (simple pure wrapper).

APIs:

def helpFor (app : AppSpec) (cmdPath : List String := []) : String
def synopsis (app : AppSpec) (cmdPath := []) : String

Manpage generation
	•	Emit mdoc(7) by default (safer), minimal subset:
	•	.Dd, .Dt, .Os, .Sh NAME, .Nd, .Sh SYNOPSIS, .Sh DESCRIPTION,
.Sh ARGUMENTS, .Sh OPTIONS, .Sh EXIT STATUS, .Sh SEE ALSO.

def manFor (app : AppSpec) (cmdPath := []) : String

Shell completions
	•	bash: emit a _appname function using COMPREPLY, static words for subcommands, --long, -s, and choices from FromArg.choices.
	•	zsh: generate _arguments spec lines and _values for enums.
	•	fish: emit complete -c app … lines for flags/subcommands and choices.

inductive Shell | bash | zsh | fish
def completions (app : AppSpec) (sh : Shell) : String


⸻

9) Built-ins: --help, --man, --generate-completions
	•	The library can auto-inject reserved long options at the root (configurable):
	•	--help → prints helpFor of the current command context.
	•	--man  → prints manFor.
	•	--generate-completions SHELL → prints script for given shell.
	•	These are modeled as intercept options on CmdSpec with reserved names, to keep the single spec source.

⸻

10) Correctness properties (to prove)
	1.	Totality: For any State, Parser α s returns ok or err (no divergence).
	2.	Progress: If an item parses successfully, the cursor advances by at least the number of consumed tokens unless arity .zero.
	3.	Sentinel correctness: After --, only positionals match (no_flag_after_sentinel).
	4.	Short-digit support: If a FlagSpec.short? = some ⟨'0', …⟩, then token "-0" is recognized as that flag (even inside bundles).
	5.	Soundness: Parsed α satisfies the declarative semantics of the corresponding ItemSpec.
	6.	Determinism (unambiguous spec): At most one successful parse result (no backtracking-induced ambiguity).
	7.	Lawfulness: Functor, Applicative, Alternative lemmas (mapping identity, composition, left/right identity of pure, etc.).
	8.	Documentation reflection: helpFor and manFor enumerate exactly the declared items; adding/removing an item updates output predictably (traceable lemmas).

Each lemma over List String only—no heavy combinatorics or Perm reliance.

⸻

11) Testing strategy (no external libs)
	•	Golden tests for help/man/completions: render, compare with stored strings.
	•	Parser unit tests (Examples/Tests):
	•	flags (-a, -0, -abc bundling); options (-n5, -n 5, --name=foo); positionals (required/optional/many/some); subcommands; sentinel.
	•	negative numbers as positionals (with and without sentinel).
	•	Proof-backed examples: small executable theorems (example blocks) asserting equalities on parse outcomes for concrete token lists.
	•	Round-trip doc tests: for enum options, ensure choices appear in help/completions.

lake test collects:
	•	Tests/Unit.lean: #eval style assertions returning IO Unit.
	•	Tests/Golden.lean: load golden files from Tests/golden/*.txt.

⸻

12) Incremental delivery plan

M0 — Scaffolding
	•	lakefile.lean, namespaces, basic State, Result, Error, Parser with Functor/Applicative/Alternative.
	•	normalize + -- partition; tests for splitting.
	•	Proof: Sentinel.post_is_positional (simple).

M1 — Flags & short-digit
	•	FlagSpec, flag parser, bundles (-abc), numeric short acceptance (-0); unknown-flag errors.
	•	Proofs: -0 recognized if declared; bundle lemma.

M2 — Options
	•	OptSpec arities .one/.many/.some, concatenated/equals forms; FromArg instances.
	•	Proofs: consumption/arity correctness; concatenation correctness.

M3 — Positionals
	•	PosSpec with arities; greedy many/some bounded by remaining items.
	•	Proof: determinism for a linear sequence of positionals.

M4 — Subcommands
	•	CmdSpec tree; subcommands selection; switching context.
	•	Proof: determinism given unique names; progress into subparser.

M5 — Spec/Builder unification
	•	Applicative builder producing both AST and Parser.
	•	Lawfulness lemmas (Functor/Applicative).

M6 — Docs
	•	Clap-style helpFor + tests (golden).
	•	Proof sketch: items in help equal items in spec (counting lemma).

M7 — Manpages
	•	Man.lean minimal mdoc emission + golden tests.

M8 — Completions
	•	Completion.lean for bash/zsh/fish; include enum choices; golden tests.

M9 — Built-ins
	•	Inject --help/--man/--generate-completions; integration tests.

M10 — Polish
	•	Error messages, spacing, wrapping; examples (Xargs0, GitLike).
	•	CI: run lake build && lake test.

Each milestone ships with docstrings & examples; no milestone introduces new dependencies.

⸻

13) Public API sketch (what end-users write)

open ArgParse

/-- Example: `mytool` with `-0`, `-n/--num <N>`, and a `cat` subcommand. -/
def app : AppSpec :=
  let flag0 : FlagSpec := {
    short? := some ⟨'0', by decide⟩,
    long?  := none,
    meta   := { name := "-0", help? := some "Read NUL-delimited input" }
  }
  let num : OptSpec Nat := {
    short? := some ⟨'n', by decide⟩, long? := some "num",
    meta   := { name := "num", help? := some "Repetitions", default? := some "1" }
  }
  let file : PosSpec String := {
    meta := { name := "FILE", help? := some "Input file" }, arity := .some
  }
  let root : CmdSpec :=
    CmdSpec.node "mytool" {name := "mytool"} [
      ItemSpec.flag flag0,
      ItemSpec.opt  num,
      ItemSpec.pos  file
    ] [
      CmdSpec.node "cat" {name := "cat", help? := some "Concatenate files"} [
        ItemSpec.pos { meta := { name := "PATH" }, arity := .many }
      ] []
    ]
  { name := "mytool", version? := some "0.1.0", about? := some "Demo", root := root }

/-- Running: parse argv using the spec-derived parser. -/
def main (argv : List String) : IO UInt32 := do
  let st := ArgParse.normalize argv
  match ArgParse.run app st with
  | .help s => (IO.println s) *> pure 0
  | .man  s => (IO.println s) *> pure 0
  | .completions s => (IO.println s) *> pure 0
  | .ok payload =>
      -- do useful work with payload
      pure 0
  | .err e =>
      IO.eprintln (ArgParse.prettyError e); pure 2

(The run convenience wraps building the Parser from the AppSpec and intercepts built-ins.)

⸻

14) Ergonomic niceties (still dependency-free)
	•	@[doc] docstrings on all public types/defs; short example snippets under each combinator.
	•	FromArg derives for enum-like types via a macro that reads a List (String × α).
	•	Simple wrap : Nat → String → String and column layout utilities contained in Doc/Help.lean.
	•	ArgParse.Assert internal helpers for small proof hints (no tactic deps besides core).

⸻

15) Edge cases & decisions
	•	Negative numbers: may be misread as options; recommend using -- when ambiguous; spec documents this clearly.
	•	Duplicate flags/options: if repeatable=false, later occurrences override earlier ones (documented); otherwise collected in order.
	•	Unknown tokens before subcommand: treated as positionals if declared; else error with expectations list.
	•	Short bundles with valued option: In -n5v, if -n expects a value via concatenation, it consumes 5 and leaves -v for later in the same token; this is specified and tested.
	•	-0: parsed as a normal short flag; bundling works (-0v).

⸻

16) Deliverables checklist
	•	Module tree & scaffolding (M0)
	•	Applicative/Alternative instances + laws (M5/Proofs)
	•	Complete Spec AST & Builder DSL (M5)
	•	Parser elaboration from AST (M2–M4)
	•	Help/Man/Completions from AST (M6–M8)
	•	Built-ins & run entry point (M9)
	•	Examples (Xargs0, GitLike) + golden outputs
	•	Proofs: Totality, Sentinel, Short-digit, Soundness, Determinism
	•	Unit + golden tests under lake test
	•	Comprehensive docstrings and usage examples

⸻

17) Minimal spec for xargs -0 behavior (example)
	•	Declare FlagSpec.short? = '0'.
	•	Provide docstring: “-0 Read items separated by NUL bytes.”
	•	Golden tests:
	•	["-0"] → true
	•	["-0v"] with -v present → true ∧ verbose
	•	["--", "-0"] when no flag declared at this level & a positional exists → positional is "-0".

⸻

This plan keeps data structures simple (List String, small records), sticks to Applicative composition (easy to reason about), and uses a single AST to drive parsing, help/man, and completions—so docs can’t drift from behavior. All core behaviors (including -- and -0) are specified, testable, and provable within pure Lean 4.
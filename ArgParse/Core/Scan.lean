import ArgParse.Core.Combinators

/-!
# ArgParse.Core.Scan

Order-insensitive variants of the flag/option combinators. Where the baseline
combinators only inspect the front of the `pre` stream, the scanning variants
search the whole stream for the first matching token, splice it (and any
detached value) out, and leave every other token in place. Positionals then
consume whatever remains, so `greet --count 2 -v Alice` and
`greet Alice -v --count 2` parse identically.

Scanning never crosses the `--` sentinel: only `pre` is searched, so
post-sentinel tokens stay positional. `scopedPre` additionally limits a scan to
the tokens before the first subcommand name, keeping parent items out of a
child's argument segment.

One ambiguity is inherent to searching rather than consuming: a detached option
value that lexes as a flag this command accepts is claimed by the flag.
`--message -v` gives `-v` to the flag, not to `--message`. There is no way to
resolve it from the token stream alone -- both readings are legal -- so the
`--message=-v` form is the one that forces the value reading.
-/

namespace ArgParse.Core

open ArgParse
open ArgParse.Spec

/-- Remove the first token matching the flag from the stream, rewriting bundled
short flags (`-vx` matched by `-v` leaves `-x` in place). -/
def scanFlagPre (spec : FlagSpec) : List String → Option (List String)
  | [] => none
  | token :: rest =>
      match matchFlagToken spec token with
      | .none => (scanFlagPre spec rest).map (token :: ·)
      | .short => some rest
      | .long => some rest
      | .shortBundled tail => some (("-" ++ tail) :: rest)

/-- Order-insensitive flag parser: matches the flag anywhere in the `pre` stream. -/
def flagScan (spec : FlagSpec) : Parser Bool := fun st =>
  match scanFlagPre spec st.pre with
  | some pre' => .ok true (State.withPre st pre' 1)
  | none => .ok false st

/-- Worker for `takeOptionScanStep?`: try the front-of-stream option step at
each suffix of `pre`, keeping the skipped prefix (`seen`, reversed) intact. -/
def takeOptionScanStepGo
    {α : Type} [ArgParse.FromArg α] (spec : OptSpec α) (st : State) :
    (seen : List String) → (suffix : List String) → Except Error (CollectStep α)
  | _, [] => .ok (CollectStep.stay st)
  | seen, token :: rest =>
      match takeOptionStep? spec { st with pre := token :: rest } with
      | .error err => .error err
      | .ok step =>
          match step.value? with
          | some _ =>
              .ok { step with
                    state := { step.state with pre := seen.reverse ++ step.state.pre } }
          | none => takeOptionScanStepGo spec st (token :: seen) rest

/-- Attempt a single option parsing step anywhere in the `pre` stream. -/
@[inline] def takeOptionScanStep?
    {α : Type} [ArgParse.FromArg α] (spec : OptSpec α) (st : State) :
    Except Error (CollectStep α) :=
  takeOptionScanStepGo spec st [] st.pre

/-- Scanning option steps advance the cursor by the recorded amount. -/
theorem takeOptionScanStepGo_cursor
    {α : Type} [FromArg α] {spec : OptSpec α} {st : State} :
    ∀ (seen suffix : List String) {step : CollectStep α},
      takeOptionScanStepGo spec st seen suffix = .ok step →
      step.state.cursor = st.cursor + step.consumed := by
  classical
  intro seen suffix
  induction suffix generalizing seen with
  | nil =>
      intro step h
      simp [takeOptionScanStepGo] at h
      cases h
      simp [CollectStep.stay]
  | cons token rest ih =>
      intro step h
      unfold takeOptionScanStepGo at h
      cases hStep : takeOptionStep? spec { st with pre := token :: rest } with
      | error err =>
          simp [hStep] at h
      | ok inner =>
          have hCursor :
              inner.state.cursor = st.cursor + inner.consumed := by
            simpa using
              takeOptionStep?_cursor (spec := spec)
                (st := { st with pre := token :: rest }) (step := inner) hStep
          cases hVal : inner.value? with
          | some value =>
              simp [hStep, hVal] at h
              cases h
              simpa using hCursor
          | none =>
              simp [hStep, hVal] at h
              exact ih (token :: seen) h

/-- Successful scanning option steps advance the cursor by the recorded amount. -/
@[simp] theorem takeOptionScanStep?_cursor
    {α : Type} [FromArg α] {spec : OptSpec α} {st : State} {step : CollectStep α}
    (h : takeOptionScanStep? spec st = .ok step) :
    step.state.cursor = st.cursor + step.consumed :=
  takeOptionScanStepGo_cursor [] st.pre h

/-- Collect option values by repeatedly scanning the `pre` stream. -/
@[inline] def collectOptionScanSteps
    {α : Type} [ArgParse.FromArg α] (spec : OptSpec α) (st : State) :
    Except Error (CollectResult α) :=
  let fuel := st.pre.length + st.post.length + 1
  collectStepsLoop (takeOptionScanStep? spec) fuel [] [] 0 st

/-- Collect scanned option values alongside the updated parser state. -/
@[inline] def collectOptionScanValues
    {α : Type} [ArgParse.FromArg α] (spec : OptSpec α) (st : State) :
    Except Error (List α × List String × State) := do
  let result ← collectOptionScanSteps spec st
  return (result.values, result.raws, result.state)

/-- Order-insensitive option parser supporting `.one`/`.many`/`.some` arities. -/
def optionScan {α : Type} [ArgParse.FromArg α] (spec : OptSpec α) :
    Parser (match spec.arity with
      | .zero => PUnit
      | .one  => Option α
      | .many => List α
      | .some => List α) := fun st =>
  match spec.arity with
  | .zero => .ok PUnit.unit st
  | .one =>
      match collectOptionScanValues spec st with
      | .error err => .err err
      | .ok (values, _, st') => .ok (values.getLast?) st'
  | .many =>
      match collectOptionScanValues spec st with
      | .error err => .err err
      | .ok (values, _, st') => .ok values st'
  | .some =>
      match collectOptionScanValues spec st with
      | .error err => .err err
      | .ok (values, _, st') =>
          match values with
          | [] =>
              .err { kind := .missingValue
                     , context := []
                     , expect := [.optionVal spec.«meta».name] }
          | _ => .ok values st'

/-! ### Syntactic token classification

`optionToken?` decides, by inspecting a token alone, whether the option would
claim it. It mirrors the branch structure of `takeOptionStep?` exactly, which
is what lets the proofs in `Proofs/Scan.lean` turn a syntactic statement about
argv into a statement about parser behaviour. -/

/-- Whether the token is claimed by the option's short form. -/
def optionTokenShort? {α : Type} [ArgParse.FromArg α]
    (spec : OptSpec α) (token : String) : Bool :=
  match spec.short? with
  | some short =>
      token == shortLexeme short ||
        (spec.concatVal? && token.startsWith (shortLexeme short))
  | none => false

/-- Whether the option would claim this token in any of its accepted forms
(`--name`, `--name=value`, `-x`, `-xVALUE`). -/
def optionToken? {α : Type} [ArgParse.FromArg α]
    (spec : OptSpec α) (token : String) : Bool :=
  match spec.long? with
  | some name =>
      (spec.eqVal? && token.startsWith (longLexeme name ++ "=")) ||
        token == longLexeme name ||
        optionTokenShort? spec token
  | none => optionTokenShort? spec token

/-! ### Bundle expansion

`-n5v` parses and `-vn5` does not, because the option scan runs first and
`-vn5` does not begin with `-n`. Swapping the two scans only moves the problem:
then `-n5v` breaks instead. Neither pass can fix it alone, because deciding
where a bundle ends needs to know which characters are flags and which take
values — and that is exactly what a command's item list says.

So the split happens before either scan, driven by the items. It is deliberately
conservative: every character has to name a short this command accepts, and the
walk stops at the first one that takes a value, which keeps the rest of the
token as that value. Anything else is left byte-for-byte alone, so `-n5v` still
reaches the concatenation path and an unknown short can never be turned into
tokens the user did not type. -/

/-- Short forms of the items presenting a given surface syntax. -/
def shortsOfKind (kind : ItemKind) (items : List ItemSpec) : List Char :=
  items.filterMap (fun i => if i.kind = kind then i.short? else none)

/-- Walk a bundle's characters, emitting one token per flag short until an
option short is reached, which takes the rest of the token with it.

`none` means "do not touch this token": one of the characters names nothing
this command accepts, and guessing at it could invent a token the user never
typed. Running out of characters is fine -- that is a bundle of pure flags. -/
def splitBundle (flagShorts optShorts : List Char) :
    List String → List Char → Option (List String)
  | acc, [] => if acc.isEmpty then Option.none else Option.some acc.reverse
  | acc, ch :: tail =>
      if optShorts.contains ch then
        Option.some (acc.reverse ++ [String.ofList ('-' :: ch :: tail)])
      else if flagShorts.contains ch then
        splitBundle flagShorts optShorts (String.ofList ['-', ch] :: acc) tail
      else
        Option.none

/-- Split one token, or leave it exactly as it was.

A single-token result means nothing was gained -- the token was already one
short -- so the original is returned rather than a re-spelled copy. -/
def expandBundleToken (flagShorts optShorts : List Char) (token : String) : List String :=
  match token.toList with
  | '-' :: c :: rest =>
      if c = '-' then [token]
      else
        match splitBundle flagShorts optShorts [] (c :: rest) with
        | Option.some out => if out.length ≥ 2 then out else [token]
        | Option.none => [token]
  | _ => [token]

/-- Expand short bundles in the `pre` stream using the items legal here.

`post` is untouched: past the `--` sentinel nothing is an option. -/
def expandBundles (items : List ItemSpec) (st : State) : State :=
  let flagShorts := shortsOfKind .flag items
  let optShorts := shortsOfKind .option items
  { st with pre := st.pre.flatMap (expandBundleToken flagShorts optShorts) }

/-- A command with no short forms cannot bundle, so expansion is the identity. -/
@[simp] theorem expandBundles_nil_shorts (items : List ItemSpec) (st : State)
    (hflag : shortsOfKind .flag items = [])
    (hopt : shortsOfKind .option items = []) :
    expandBundles items st = st := by
  simp only [expandBundles, hflag, hopt]
  have hid : ∀ tokens : List String,
      tokens.flatMap (expandBundleToken [] []) = tokens := by
    intro tokens
    induction tokens with
    | nil => rfl
    | cons t rest ih =>
        simp only [List.flatMap_cons, ih]
        have : expandBundleToken [] [] t = [t] := by
          simp only [expandBundleToken]
          split
          · split
            · rfl
            · simp [splitBundle]
          · rfl
        simp [this]
  rw [hid]

/-! ### Keeping positionals off other items' tokens

Scanning is order-insensitive; positionals are not. A positional takes the front
of `pre`, so it takes whatever is there — including a flag, or another option's
value — if it happens to run before the scan that wanted it. `clean -p out` used
to fail for exactly that reason: the positional was sequenced first and ate `-p`.

Sequencing positionals last fixes it, but that is a rule the library cannot
enforce: the applicative order is the caller's, and `P` is opaque. What the
library *can* do is make the rule unnecessary, by moving the tokens this
command's flags and options would claim behind the ones they would not. Scanning
finds them wherever they are, and a positional now sees only tokens nothing else
wanted. -/

/-- Whether a non-positional lexeme would claim this token, in any of its
spellings: exact, `--name=value`, or `-nvalue`. -/
def lexemeClaims (lex : String) (token : String) : Bool :=
  if lex.startsWith "--" then token == lex || token.startsWith (lex ++ "=")
  else token == lex || token.startsWith lex

/-- Stable partition of a segment into the tokens left for positionals and the
tokens this command's flags and options would claim.

A detached value travels with the lexeme that takes it, so the two stay adjacent
and a positional cannot pick up the value on its own. -/
def partitionClaimed (lexemes valueLex : List String) :
    List String → List String × List String
  | [] => ([], [])
  | tok :: rest =>
      if lexemes.any (lexemeClaims · tok) then
        if valueLex.contains tok then
          match rest with
          | [] => ([], [tok])
          | v :: rest' =>
              let (free, taken) := partitionClaimed lexemes valueLex rest'
              (free, tok :: v :: taken)
        else
          let (free, taken) := partitionClaimed lexemes valueLex rest
          (free, tok :: taken)
      else
        let (free, taken) := partitionClaimed lexemes valueLex rest
        (tok :: free, taken)

/-- Move the tokens this command's flags and options would claim behind the rest,
so its positionals see only what nothing else wanted.

Relative order is preserved within each group, so repeated options still
accumulate in the order they were written and positionals still arrive in the
order they were typed. -/
def hoistPositionals (items : List ItemSpec) (st : State) : State :=
  let switches := items.filter (fun i => i.kind != .positional)
  let lexemes := switches.flatMap (·.lexemes)
  let valueLex := valueLexemes switches
  let (free, taken) := partitionClaimed lexemes valueLex st.pre
  { st with pre := free ++ taken }

/-- The whole pre-pass a command runs over the segment it owns: split bundles,
then keep positionals off the tokens its other items want. -/
def prepare (items : List ItemSpec) (st : State) : State :=
  hoistPositionals items (expandBundles items st)

/-- A command with no flags or options leaves the stream alone. -/
@[simp] theorem hoistPositionals_no_switches (items : List ItemSpec) (st : State)
    (h : items.filter (fun i => i.kind != .positional) = []) :
    hoistPositionals items st = st := by
  have hid : ∀ tokens : List String,
      partitionClaimed [] [] tokens = (tokens, []) := by
    intro tokens
    induction tokens with
    | nil => rfl
    | cons t rest ih =>
        rw [partitionClaimed.eq_def]
        simp [ih]
  simp only [hoistPositionals, h, List.flatMap_nil, valueLexemes, List.filter_nil, hid]
  simp

/-- Split a token list at the first occurrence of any of the given names. -/
def splitAtFirst (names : List String) : List String → List String × List String
  | [] => ([], [])
  | tok :: rest =>
      if names.contains tok then
        ([], tok :: rest)
      else
        let (seg, remainder) := splitAtFirst names rest
        (tok :: seg, remainder)

/-- Splitting factors the stream: segment and remainder rejoin to the input. -/
theorem splitAtFirst_append (names : List String) :
    ∀ tokens : List String,
      (splitAtFirst names tokens).fst ++ (splitAtFirst names tokens).snd = tokens
  | [] => rfl
  | tok :: rest => by
      by_cases hHit : tok ∈ names
      · simp [splitAtFirst, hHit]
      · simp [splitAtFirst, hHit, splitAtFirst_append names rest]

/-- Run `p` with the `pre` stream restricted to the tokens before the first
occurrence of any of `names`, reattaching the remainder afterwards. Used to
keep a parent command's scanning items away from a subcommand's segment. -/
def scopedPre (names : List String) (p : Parser α) : Parser α := fun st =>
  let split := splitAtFirst names st.pre
  match p { st with pre := split.fst } with
  | .ok a st' => .ok a { st' with pre := st'.pre ++ split.snd }
  | .err e => .err e

end ArgParse.Core

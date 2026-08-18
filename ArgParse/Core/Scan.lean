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

Searching rather than consuming creates one ambiguity: a detached option value
that lexes as a flag this command accepts could go to either. `--message -v` is
resolved the way every mainstream parser resolves it -- the option takes it --
by fusing the pair into `--message=-v` in the pre-pass, before any scan runs.
The `=` form was always the unambiguous spelling; the pre-pass just stops the
user from having to reach for it.
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

Which of `-vn5` and `-n5v` a scan can read depends on which scan runs first, and
that is the caller's applicative order rather than anything the library chooses.
Neither scan can fix it alone, because deciding where a bundle ends needs to know
which characters are flags and which take values — and that is exactly what a
command's item list says.

So the split happens before either scan, driven by the items. It is deliberately
conservative: every character has to name a short this command accepts, and an
unknown one leaves the token byte-for-byte alone, so the pass can never invent a
token the user did not type.

Where the walk reaches an option short, how much of the rest is its value is a
question only that value's decoder can answer -- `5v` is `5` and a flag for a
`Nat`, and the whole tail for a `String`. `ItemSpec.concatFit` is the part of
that answer the decoder can state as data, which is what lets this pass split
`-n5v` as well as `-vn5`. An item that says `anything` keeps the whole tail, and
the token reaches the concatenation path in the option's own scan exactly as it
used to. -/

/-- Short forms of the items presenting a given surface syntax. -/
def shortsOfKind (kind : ItemKind) (items : List ItemSpec) : List Char :=
  items.filterMap (fun i => if i.kind = kind then i.short? else none)

/-- Option shorts of this command paired with how far their values reach. -/
def optionShortFits (items : List ItemSpec) : List (Char × ConcatFit) :=
  items.filterMap fun i =>
    if i.kind = .option then i.short?.map (fun c => (c, i.concatFit)) else Option.none

/-- Split a concatenated tail into the value and what follows it, for an option
whose values are a digit run. `none` means there is nothing to split: no leading
digit, or nothing left over once the digits are taken.

This agrees with what `findConcatSplit?` would decide at scan time, which is
what makes splitting up front a rewrite rather than a second opinion: the
longest decodable prefix of a `Nat` tail is exactly its leading digits.

The residue is returned already split into its first character and the rest,
because there is no split to speak of without one. -/
def digitValueSplit (tail : List Char) : Option (List Char × Char × List Char) :=
  match tail.takeWhile Char.isDigit, tail.dropWhile Char.isDigit with
  | [], _ => Option.none
  | _, [] => Option.none
  | value, r :: rs => Option.some (value, r, rs)

/-- The split reassembles: the value and the residue are exactly the tail, in
order. This is what keeps the pass from inventing characters -- the two tokens
it emits are the one the user typed, cut in half and re-dashed. The scan-time
split has the same guarantee in `findConcatSplit?_split`. -/
theorem digitValueSplit_concat {tail value : List Char} {r : Char} {rs : List Char}
    (h : digitValueSplit tail = Option.some (value, r, rs)) :
    value ++ r :: rs = tail := by
  unfold digitValueSplit at h
  have hsplit := tail.takeWhile_append_dropWhile (p := Char.isDigit)
  split at h <;> simp_all

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

/-- Split a token that *leads* with an option short whose value shape is known,
into that option with its value and whatever follows.

`none` leaves the decision to the caller: either the leading short is not such
an option, or there is no value-and-residue to separate, or the residue is not
something this command's shorts account for. -/
def splitLeadingOption (flagShorts optShorts : List Char)
    (fits : List (Char × ConcatFit)) (c : Char) (rest : List Char) :
    Option (List String) :=
  match fits.find? (fun p => p.fst = c) with
  | Option.some (_, .digits) =>
      match digitValueSplit rest with
      | Option.some (value, r, rs) =>
          match splitBundle flagShorts optShorts [] (r :: rs) with
          | Option.some out => Option.some (String.ofList ('-' :: c :: value) :: out)
          | Option.none => Option.none
      | Option.none => Option.none
  | _ => Option.none

/-- Split one token, or leave it exactly as it was.

A single-token result means nothing was gained -- the token was already one
short -- so the original is returned rather than a re-spelled copy. -/
def expandBundleToken (flagShorts optShorts : List Char)
    (fits : List (Char × ConcatFit)) (token : String) : List String :=
  match token.toList with
  | '-' :: c :: rest =>
      if c = '-' then [token]
      else
        match splitLeadingOption flagShorts optShorts fits c rest with
        | Option.some out => out
        | Option.none =>
            match splitBundle flagShorts optShorts [] (c :: rest) with
            | Option.some out => if out.length ≥ 2 then out else [token]
            | Option.none => [token]
  | _ => [token]

/-- Expand short bundles in the `pre` stream using the items legal here.

The pass runs twice. One sweep splits a bundle that leads with flags, handing
the trailing option short the rest of the token; a second lets that piece be
split in turn, so `-vn5f` reaches `-v -n5 -f`. Expansion is idempotent, so a
third sweep would do nothing.

`post` is untouched: past the `--` sentinel nothing is an option. -/
def expandBundles (items : List ItemSpec) (st : State) : State :=
  let flagShorts := shortsOfKind .flag items
  let optShorts := shortsOfKind .option items
  let fits := optionShortFits items
  let step (tokens : List String) := tokens.flatMap (expandBundleToken flagShorts optShorts fits)
  { st with pre := step (step st.pre) }

/-- A command with no short forms cannot bundle, so expansion is the identity. -/
@[simp] theorem expandBundles_nil_shorts (items : List ItemSpec) (st : State)
    (hflag : shortsOfKind .flag items = [])
    (hopt : shortsOfKind .option items = []) :
    expandBundles items st = st := by
  simp only [expandBundles, hflag, hopt]
  have hlead : ∀ (fits : List (Char × ConcatFit)) (c : Char) (rest : List Char),
      splitLeadingOption [] [] fits c rest = Option.none := by
    intro fits c rest
    unfold splitLeadingOption
    split
    · split
      · simp [splitBundle]
      · rfl
    · rfl
  have hid : ∀ (fits : List (Char × ConcatFit)) (tokens : List String),
      tokens.flatMap (expandBundleToken [] [] fits) = tokens := by
    intro fits tokens
    induction tokens with
    | nil => rfl
    | cons t rest ih =>
        simp only [List.flatMap_cons, ih]
        have : expandBundleToken [] [] fits t = [t] := by
          simp only [expandBundleToken, hlead]
          split
          · split
            · rfl
            · simp [splitBundle]
          · rfl
        simp [this]
  rw [hid, hid]

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

/-! ### Detached values that look like flags

A value that lexes as one of this command's own flags is claimed by whichever
scan reaches it first, and the scans run in the caller's applicative order. So
`--message -v` parsed as a `-v` flag and a `--message` with nothing to take,
unless the user reached for `--message=-v`.

`partitionClaimed` already takes the other side of that question: it keeps a
detached value adjacent to its lexeme so a positional cannot pick it up. The
scans just never asked. Fusing the pair into the `=` spelling before anything
scans settles it once, in the direction the rest of the library already assumes
and every mainstream parser takes -- an option that takes a value takes the
token after it.

The pass fires only where the ambiguity is real: the second token has to be one
this command's own items would claim. An ordinary value is left as the user
typed it. -/

/-- Write a detached pair as the unambiguous concatenated spelling. -/
def fuseValueToken (lex value : String) : String :=
  if lex.startsWith "--" then lex ++ "=" ++ value else lex ++ value

/-- Fuse each detached value onto the lexeme that takes it, where leaving them
apart would let another item claim the value first. -/
def fuseDetachedValues (lexemes valueLex : List String) :
    List String → List String
  | [] => []
  | [tok] => [tok]
  | tok :: v :: rest =>
      if valueLex.contains tok && lexemes.any (lexemeClaims · v) then
        fuseValueToken tok v :: fuseDetachedValues lexemes valueLex rest
      else
        tok :: fuseDetachedValues lexemes valueLex (v :: rest)

/-- Run the fuse over a command's own segment, using its own items. -/
def fuseValues (items : List ItemSpec) (st : State) : State :=
  let switches := items.filter (fun i => i.kind != .positional)
  let lexemes := switches.flatMap (·.lexemes)
  { st with pre := fuseDetachedValues lexemes (valueLexemes switches) st.pre }

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
fuse detached values onto the lexemes that take them, then keep positionals off
the tokens its other items want.

Order matters. Bundles are split first so `-vn5` has become `-v -n5` before
anything looks for a detached value; hoisting runs last so it sees the fused
tokens as the single claimed tokens they now are. -/
def prepare (items : List ItemSpec) (st : State) : State :=
  hoistPositionals items (fuseValues items (expandBundles items st))

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

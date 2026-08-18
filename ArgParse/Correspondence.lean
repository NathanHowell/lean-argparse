import ArgParse.Exec
import ArgParse.Core.Scan
import ArgParse.Proofs.Scan

/-!
# ArgParse.Correspondence

What was a sync guard becomes a lemma.

Because `doc` and `run` are constructed together in Layer 3, the agreement
between what help says and what the parser accepts is provable per builder and
lifted by induction over `Cmd`. These sit on top of the Layer-1 suite (progress,
scan/front-of-stream agreement) and do not disturb it.

A note on what several of these proofs look like: they are short, and a few are
`rfl`. That is the result being reported, not a shortcut being taken. In a
design where help reads a separately-maintained declaration the corresponding
statements are false, and no proof of any length would establish them. The
statements are also written to name both halves explicitly, so an edit that
changes one and not the other stops compiling here.
-/

namespace ArgParse.Correspondence

open ArgParse ArgParse.Spec ArgParse.Builder

/-! ### Item agreement

Each builder produces one item, and the runtime spec handed to the scanner
advertises exactly the surface syntax that item does. -/

/-- A flag's document is the item its parts describe. -/
theorem flag_doc (long : String) (short : Option Char) (help : String) (hidden : Bool) :
    (Builder.flag long short help hidden).doc =
      .item (flagParts long short help hidden).snd := rfl

/-- A flag's parser is the scanner for the spec its parts describe. -/
theorem flag_run (long : String) (short : Option Char) (help : String) (hidden : Bool) :
    (Builder.flag long short help hidden).run =
      Core.flagScan (flagParts long short help hidden).fst := rfl

/-- **Item agreement for flags.** The lexemes the scanner matches on and the
lexemes the item advertises are the same lexemes. -/
theorem flag_surface_agree (long : String) (short : Option Char) (help : String)
    (hidden : Bool) :
    let parts := flagParts long short help hidden
    parts.fst.long? = parts.snd.long? ∧
      parts.fst.short?.map (·.c) = parts.snd.short? := ⟨rfl, rfl⟩

/-- **Item agreement for options.** Likewise for the option builders, which all
route through `optParts`. -/
theorem opt_surface_agree (α : Type) [FromArg α] (long : String) (short : Option Char)
    (metavar : Option String) (help : String) (defaultText? : Option String)
    (arity : Arity) (required hidden : Bool) :
    let parts := optParts α long short metavar help defaultText? arity required hidden
    parts.fst.long? = parts.snd.long? ∧
      parts.fst.short?.map (·.c) = parts.snd.short? ∧
      parts.fst.arity = parts.snd.arity := ⟨rfl, rfl, rfl⟩

/-- **Item agreement for positionals.** -/
theorem pos_surface_agree (α : Type) [FromArg α] (name : String)
    (metavar : Option String) (help : String) (arity : Arity) (required hidden : Bool) :
    let parts := posParts α name metavar help arity required hidden
    parts.fst.arity = parts.snd.arity ∧
      parts.fst.«meta».name = parts.snd.name := ⟨rfl, rfl⟩

/-- A rejected short character is absent from the item, not merely unparseable.

This is the property that keeps help from advertising a form the scanner will
never match: the item reads its short form off the constructed `Short`, so a
character the constructor refuses disappears from both halves at once. -/
theorem short_rejected_absent (long : String) (help : String) (hidden : Bool) :
    (flagParts long (some '-') help hidden).snd.short? = none ∧
      (flagParts long (some '-') help hidden).fst.short? = none := ⟨rfl, rfl⟩

/-- Each builder contributes exactly one item to its document. -/
theorem flag_items_length (long : String) (short : Option Char) (help : String)
    (hidden : Bool) : ((Builder.flag long short help hidden).items).length = 1 := rfl

/-- An optional option contributes one item, wrapped in the alternation that
renders as `[…]`. -/
theorem optionOpt_items_length (α : Type) [FromArg α] (long : String)
    (short : Option Char) (metavar : Option String) (help : String) (hidden : Bool) :
    ((Builder.optionOpt α long short metavar help hidden).items).length = 1 := rfl

/-- A required option is advertised as required. -/
theorem option_required (α : Type) [FromArg α] (long : String) (short : Option Char)
    (metavar : Option String) (help : String) (hidden : Bool) :
    ∀ item ∈ (Builder.option α long short metavar help hidden).items, item.required := by
  intro item h
  simp [P.items, Builder.option, Doc.items] at h
  subst h
  rfl

/-- An option with a default is advertised as optional, because the default is
exactly what makes it so. -/
theorem optionD_not_required (α : Type) [FromArg α] [ToString α] (long : String)
    (default : α) (short : Option Char) (metavar : Option String) (help : String)
    (hidden : Bool) :
    ∀ item ∈ (Builder.optionD long default short metavar help hidden).items,
      item.required = false := by
  intro item h
  simp [P.items, Builder.optionD, Doc.items, Doc.itemsList] at h
  subst h
  rfl

/-- A default reaches help as the same value the parser falls back to. -/
theorem optionD_default_rendered (α : Type) [FromArg α] [ToString α] (long : String)
    (default : α) (short : Option Char) (metavar : Option String) (help : String)
    (hidden : Bool) :
    ∀ item ∈ (Builder.optionD long default short metavar help hidden).items,
      item.default? = some (toString default) := by
  intro item h
  simp [P.items, Builder.optionD, Doc.items, Doc.itemsList] at h
  subst h
  rfl

/-! ### Behavioural acceptance

The statements above relate two pieces of data. These relate the document to
what the parser does with tokens. -/

/-- A flag accepts its own long lexeme. -/
theorem flag_accepts_long (long : String) (short : Option Char) (help : String)
    (hidden : Bool) (h : long ≠ "") :
    ∃ st, (Builder.flag long short help hidden).run
      (Core.normalize ["--" ++ long]) = .ok true st := by
  have hne : ("--" ++ long) ≠ "--" := by
    intro hEq
    apply h
    have hlen := congrArg String.length hEq
    simp at hlen
    exact hlen
  refine ⟨Core.State.withPre (Core.normalize ["--" ++ long]) [] 1, ?_⟩
  simp [Builder.flag, flagParts, Core.flagScan, Core.normalize,
        Core.split_cons_token hne, Core.scanFlagPre, Core.matchFlagToken,
        Core.longLexeme]

/-- A flag accepts its own short lexeme, when the character was admissible. -/
theorem flag_accepts_short (long : String) (c : Char) (help : String) (hidden : Bool)
    (hc : c ≠ '-' ∧ c.toNat < 128) :
    ∃ st, (Builder.flag long (some c) help hidden).run
      (Core.normalize ["-" ++ String.singleton c]) = .ok true st := by
  have hne : ("-" ++ String.singleton c) ≠ "--" := by
    intro hEq
    apply hc.1
    have hlist := congrArg (fun s => s.toList) hEq
    simp at hlist
    exact hlist
  have hsplit : (Core.split ["-" ++ String.singleton c]).pre = ["-" ++ String.singleton c] := by
    rw [Core.split_cons_token hne]
    simp
  have hs : mkShort? (some c) = some ⟨c, hc⟩ := by simp [mkShort?, hc.1, hc.2]
  refine ⟨Core.State.withPre (Core.normalize ["-" ++ String.singleton c]) [] 1, ?_⟩
  simp only [Builder.flag, flagParts, Core.flagScan, Core.normalize, hsplit, hs,
             Core.scanFlagPre, Core.matchFlagToken, Core.longLexeme, Core.shortLexeme]
  by_cases hcollide : ("-" ++ String.singleton c) = "--" ++ long
  · rw [if_pos hcollide]
  · rw [if_neg hcollide, if_pos trivial]

/-- A token the scanner classifies as no match leaves the flag unset and the
stream untouched.

The hypothesis is stated over the very spec the builder passes to the scanner,
which is what makes this the bridge from Layer 1's `Proofs.Scan` results to a
statement about a `P`. -/
theorem flag_ignores_unmatched (long : String) (short : Option Char) (help : String)
    (hidden : Bool) (token : String) (hne : token ≠ "--")
    (hmiss : Core.matchFlagToken (flagParts long short help hidden).fst token
               = Core.FlagMatch.none) :
    (Builder.flag long short help hidden).run (Core.normalize [token])
      = .ok false (Core.normalize [token]) := by
  have hsplit : (Core.split [token]).pre = [token] := by
    rw [Core.split_cons_token hne]
    simp
  simp only [Builder.flag, Core.flagScan, Core.normalize, hsplit,
             Core.scanFlagPre, hmiss]
  rfl

/-! ### Behavioural acceptance: options

The four option builders share one runtime core, `Builder.optionValues`, and
differ only in how they read the list it returns. So acceptance is proved once
against that core and the builders follow as corollaries — which is also the
statement that they cannot drift apart.

Two directions are covered, matching the flag lemmas above: a stream carrying
the option's own detached long form yields its value, and a stream carrying
nothing the option claims leaves it empty. -/

/-- A string never starts with a strict extension of itself.

`takeOptionLongToken?` tests the `--name=value` form *before* the detached
`--name value` form, so reaching the detached branch means discharging this.
The proof crosses out of `String`: `simp` rewrites `startsWith` to a list-prefix
claim, and a prefix cannot be longer than what it prefixes. -/
theorem startsWith_append_eq_false (s t : String) (ht : t ≠ "") :
    (s.startsWith (s ++ t)) = false := by
  simp
  intro h
  have hl := h.length_le
  simp at hl
  apply ht
  have h0 : t.toList = [] := by
    have : t.toList.length = 0 := by omega
    exact List.eq_nil_of_length_eq_zero this
  simpa using congrArg String.ofList h0

/-- A non-empty long name gives a lexeme distinct from the sentinel. -/
theorem long_lexeme_ne_sentinel {long : String} (h : long ≠ "") :
    ("--" ++ long) ≠ "--" := by
  intro hEq
  apply h
  have hlen := congrArg String.length hEq
  simp at hlen
  exact hlen

/-- Two non-sentinel tokens normalize to a two-token `pre` stream. -/
theorem normalize_pre_pair {a b : String} (ha : a ≠ "--") (hb : b ≠ "--") :
    (Core.normalize [a, b]).pre = a :: b :: [] := by
  simp [Core.normalize, Core.split_cons_token ha, Core.split_cons_token hb]

/-- The collector loop over one matching step followed by an exhausted stream. -/
theorem collectStepsLoop_single {α : Type}
    (takeStep : State → Except Error (Core.CollectStep α))
    (fuel : Nat) (hfuel : 2 ≤ fuel)
    (st : State) (step : Core.CollectStep α) (value : α) (raw : String)
    (h1 : takeStep st = .ok step)
    (hv : step.value? = some value) (hr : step.raw? = some raw)
    (h2 : takeStep step.state = .ok (Core.CollectStep.stay step.state)) :
    Core.collectStepsLoop takeStep fuel [] [] 0 st
      = .ok { values := [value], raws := [raw]
            , state := step.state, consumed := step.consumed } := by
  obtain ⟨f, rfl⟩ : ∃ f, fuel = f + 2 := ⟨fuel - 2, by omega⟩
  simp only [Core.collectStepsLoop, h1, hv, hr]
  simp only [h2, Core.CollectStep.stay]
  simp

/-- The collector loop over a stream that matches nothing. -/
theorem collectStepsLoop_stay {α : Type}
    (takeStep : State → Except Error (Core.CollectStep α))
    (fuel : Nat) (hfuel : 1 ≤ fuel) (st : State)
    (h : takeStep st = .ok (Core.CollectStep.stay st)) :
    Core.collectStepsLoop takeStep fuel [] [] 0 st
      = .ok { values := [], raws := [], state := st, consumed := 0 } := by
  obtain ⟨f, rfl⟩ : ∃ f, fuel = f + 1 := ⟨fuel - 1, by omega⟩
  simp [Core.collectStepsLoop, h, Core.CollectStep.stay]

/-- An exhausted stream offers the option scanner nothing. -/
theorem takeOptionScanStep?_stay_nil {α : Type} [FromArg α] (spec : OptSpec α)
    (st : State) (h : st.pre = []) :
    Core.takeOptionScanStep? spec st = .ok (Core.CollectStep.stay st) := by
  simp [Core.takeOptionScanStep?, h, Core.takeOptionScanStepGo]

/-- Every option builder's spec carries the long name it was given. -/
theorem optParts_long? (α : Type) [FromArg α] (long : String) (short : Option Char)
    (metavar : Option String) (help : String) (defaultText : Option String)
    (arity : Arity) (required hidden : Bool) :
    (optParts α long short metavar help defaultText arity required hidden).fst.long?
      = some long := rfl

/-- The front-of-stream step claims its own long lexeme and the token after it. -/
theorem takeOptionStep?_detached_long {α : Type} [FromArg α] (spec : OptSpec α)
    (long v : String) (value : α) (st : State) (rest : List String)
    (hlong : spec.long? = some long)
    (hpre : st.pre = ("--" ++ long) :: v :: rest)
    (hrun : FromArg.run v = .ok value) :
    Core.takeOptionStep? spec st
      = .ok (Core.CollectStep.ofPre st rest 2 (some value) (some v)) := by
  simp only [Core.takeOptionStep?, hpre, hlong, Core.takeOptionLongToken?]
  by_cases heq : spec.eqVal? = true
  · rw [if_pos heq, Core.longLexeme,
      startsWith_append_eq_false ("--" ++ long) "=" (by simp)]
    simp [Core.takeOptionDetachedValue?, hrun]
  · rw [if_neg heq, Core.longLexeme]
    simp [Core.takeOptionDetachedValue?, hrun]

/-- The scanning collector reads exactly one value off `--name value`, leaving
a tail that claims nothing where it found it. -/
theorem collectOptionScanValues_detached_long {α : Type} [FromArg α]
    (spec : OptSpec α) (long v : String) (value : α) (st : State) (rest : List String)
    (hlong : spec.long? = some long)
    (hpre : st.pre = ("--" ++ long) :: v :: rest)
    (hrun : FromArg.run v = .ok value)
    (hrest : ∀ tok ∈ rest, Core.optionToken? spec tok = false) :
    Core.collectOptionScanValues spec st
      = .ok ([value], [v], Core.State.withPre st rest 2) := by
  have hstep := takeOptionStep?_detached_long spec long v value st rest hlong hpre hrun
  have hscan := Proofs.Scan.takeOptionScanStep?_eq_of_head hstep
    (by simp [Core.CollectStep.ofPre])
  have hstay : Core.takeOptionScanStep? spec
      (Core.CollectStep.ofPre st rest 2 (some value) (some v) : Core.CollectStep α).state
      = .ok (Core.CollectStep.stay
          (Core.CollectStep.ofPre st rest 2 (some value) (some v)
            : Core.CollectStep α).state) := by
    refine Proofs.Scan.takeOptionScanStep?_stay_of_no_match ?_
    simpa [Core.CollectStep.ofPre, Core.State.withPre] using hrest
  simp only [Core.collectOptionScanValues, Core.collectOptionScanSteps]
  rw [collectStepsLoop_single _ _ (by simp [hpre]; omega) st _ value v hscan
    (by simp [Core.CollectStep.ofPre]) (by simp [Core.CollectStep.ofPre]) hstay]
  rfl

/-- **The shared option core accepts its detached long form.** Every option
builder is a reading of this list. -/
theorem optionValues_accepts_detached_long {α : Type} [FromArg α]
    (spec : OptSpec α) (long v : String) (value : α) (st : State) (rest : List String)
    (hlong : spec.long? = some long)
    (hpre : st.pre = ("--" ++ long) :: v :: rest)
    (hrun : FromArg.run v = .ok value)
    (hrest : ∀ tok ∈ rest, Core.optionToken? spec tok = false) :
    optionValues spec st = .ok [value] (Core.State.withPre st rest 2) := by
  simp [optionValues,
    collectOptionScanValues_detached_long spec long v value st rest hlong hpre hrun hrest]

/-- A stream with no token the option claims collects nothing and moves nothing.

The hypothesis is stated over `Core.optionToken?`, the classifier the scanner
itself consults, which is what makes this the option-side counterpart of
`flag_ignores_unmatched`. -/
theorem collectOptionScanValues_no_match {α : Type} [FromArg α]
    (spec : OptSpec α) (st : State)
    (h : ∀ tok ∈ st.pre, Core.optionToken? spec tok = false) :
    Core.collectOptionScanValues spec st = .ok ([], [], st) := by
  simp only [Core.collectOptionScanValues, Core.collectOptionScanSteps]
  rw [collectStepsLoop_stay _ _ (by omega) st
    (Proofs.Scan.takeOptionScanStep?_stay_of_no_match h)]
  rfl

/-- **The shared option core declines what it does not claim.** -/
theorem optionValues_no_match {α : Type} [FromArg α]
    (spec : OptSpec α) (st : State)
    (h : ∀ tok ∈ st.pre, Core.optionToken? spec tok = false) :
    optionValues spec st = .ok [] st := by
  simp [optionValues, collectOptionScanValues_no_match spec st h]

/-- A required option accepts `--name value`. -/
theorem option_accepts_detached_long (α : Type) [FromArg α]
    (long v : String) (value : α) (short : Option Char) (metavar : Option String)
    (help : String) (hidden : Bool)
    (hlong : long ≠ "") (hv : v ≠ "--") (hrun : FromArg.run v = .ok value) :
    (Builder.option α long short metavar help hidden).run
        (Core.normalize ["--" ++ long, v])
      = .ok value (Core.State.withPre (Core.normalize ["--" ++ long, v]) [] 2) := by
  have hpre := normalize_pre_pair (long_lexeme_ne_sentinel hlong) hv
  have h := optionValues_accepts_detached_long _ long v value _ []
    (optParts_long? α long short metavar help none .one true hidden) hpre hrun (by simp)
  simp [Builder.option, h]

/-- An optional option accepts `--name value`. -/
theorem optionOpt_accepts_detached_long (α : Type) [FromArg α]
    (long v : String) (value : α) (short : Option Char) (metavar : Option String)
    (help : String) (hidden : Bool)
    (hlong : long ≠ "") (hv : v ≠ "--") (hrun : FromArg.run v = .ok value) :
    (Builder.optionOpt α long short metavar help hidden).run
        (Core.normalize ["--" ++ long, v])
      = .ok (some value)
          (Core.State.withPre (Core.normalize ["--" ++ long, v]) [] 2) := by
  have hpre := normalize_pre_pair (long_lexeme_ne_sentinel hlong) hv
  have h := optionValues_accepts_detached_long _ long v value _ []
    (optParts_long? α long short metavar help none .one false hidden) hpre hrun (by simp)
  simp [Builder.optionOpt, h]

/-- A defaulted option accepts `--name value`, and the supplied value wins. -/
theorem optionD_accepts_detached_long {α : Type} [FromArg α] [ToString α]
    (long v : String) (value default : α) (short : Option Char)
    (metavar : Option String) (help : String) (hidden : Bool)
    (hlong : long ≠ "") (hv : v ≠ "--") (hrun : FromArg.run v = .ok value) :
    (Builder.optionD long default short metavar help hidden).run
        (Core.normalize ["--" ++ long, v])
      = .ok value (Core.State.withPre (Core.normalize ["--" ++ long, v]) [] 2) := by
  have hpre := normalize_pre_pair (long_lexeme_ne_sentinel hlong) hv
  have h := optionValues_accepts_detached_long _ long v value _ []
    (optParts_long? α long short metavar help (some (toString default)) .one false hidden)
    hpre hrun (by simp)
  simp [Builder.optionD, h]

/-- A repeatable option accepts `--name value`, collecting a one-element list. -/
theorem options_accepts_detached_long (α : Type) [FromArg α]
    (long v : String) (value : α) (short : Option Char) (metavar : Option String)
    (help : String) (hidden : Bool)
    (hlong : long ≠ "") (hv : v ≠ "--") (hrun : FromArg.run v = .ok value) :
    (Builder.options α long short metavar help hidden).run
        (Core.normalize ["--" ++ long, v])
      = .ok [value] (Core.State.withPre (Core.normalize ["--" ++ long, v]) [] 2) := by
  have hpre := normalize_pre_pair (long_lexeme_ne_sentinel hlong) hv
  have h := optionValues_accepts_detached_long _ long v value _ []
    (optParts_long? α long short metavar help none .many false hidden) hpre hrun (by simp)
  simp [Builder.options, h]

/-- An optional option is `none` when the stream claims nothing for it. -/
theorem optionOpt_ignores_unclaimed (α : Type) [FromArg α]
    (long : String) (short : Option Char) (metavar : Option String)
    (help : String) (hidden : Bool) (st : State)
    (h : ∀ tok ∈ st.pre,
      Core.optionToken? (optParts α long short metavar help none .one false hidden).fst
        tok = false) :
    (Builder.optionOpt α long short metavar help hidden).run st = .ok none st := by
  simp [Builder.optionOpt, optionValues_no_match _ st h]

/-- A defaulted option falls back exactly when the stream claims nothing for it. -/
theorem optionD_falls_back {α : Type} [FromArg α] [ToString α]
    (long : String) (default : α) (short : Option Char) (metavar : Option String)
    (help : String) (hidden : Bool) (st : State)
    (h : ∀ tok ∈ st.pre,
      Core.optionToken?
        (optParts α long short metavar help (some (toString default)) .one false hidden).fst
        tok = false) :
    (Builder.optionD long default short metavar help hidden).run st
      = .ok default st := by
  simp [Builder.optionD, optionValues_no_match _ st h]

/-- A repeatable option collects the empty list when nothing is claimed. -/
theorem options_ignores_unclaimed (α : Type) [FromArg α]
    (long : String) (short : Option Char) (metavar : Option String)
    (help : String) (hidden : Bool) (st : State)
    (h : ∀ tok ∈ st.pre,
      Core.optionToken? (optParts α long short metavar help none .many false hidden).fst
        tok = false) :
    (Builder.options α long short metavar help hidden).run st = .ok [] st := by
  simp [Builder.options, optionValues_no_match _ st h]

/-- A required option reports `missingValue` exactly when nothing is claimed. -/
theorem option_missing_when_unclaimed (α : Type) [FromArg α]
    (long : String) (short : Option Char) (metavar : Option String)
    (help : String) (hidden : Bool) (st : State)
    (h : ∀ tok ∈ st.pre,
      Core.optionToken? (optParts α long short metavar help none .one true hidden).fst
        tok = false) :
    (Builder.option α long short metavar help hidden).run st
      = .err { kind := .missingValue, context := [], expect := [.optionVal long] } := by
  simp [Builder.option, optionValues_no_match _ st h]

/-! ### Behavioural acceptance: positionals

Positionals read the front of the stream rather than scanning it, so these are
shorter: one lemma for a readable head token, one for an exhausted stream, and
the three builders read off them. -/

/-- A positional claims the head token when it parses, and when the token is not
one a flag or option was meant to claim. -/
theorem takePositionalStep?_head {α : Type} [FromArg α] (spec : PosSpec α)
    (st : State) (tok : String) (rest : List String) (value : α)
    (hpre : st.pre = tok :: rest) (hopt : Core.optionLike tok = false)
    (hrun : FromArg.run tok = .ok value) :
    Core.takePositionalStep? spec st
      = .ok { value? := some value, raw? := some tok
            , state := Core.State.withPre st rest 1, consumed := 1 } := by
  simp [Core.takePositionalStep?, Core.State.consumePre?, hpre, hopt, hrun]

/-- A positional claims nothing once both streams are exhausted. -/
theorem takePositionalStep?_stay {α : Type} [FromArg α] (spec : PosSpec α)
    (st : State) (hpre : st.pre = []) (hpost : st.post = []) :
    Core.takePositionalStep? spec st = .ok (Core.CollectStep.stay st) := by
  simp [Core.takePositionalStep?, Core.takePositionalFromPost, Core.State.consumePre?,
    Core.State.consumePost?, hpre, hpost, Core.CollectStep.stay]

/-- A required positional takes the head token. -/
theorem arg_accepts_head (α : Type) [FromArg α]
    (name : String) (metavar : Option String) (help : String) (hidden : Bool)
    (st : State) (tok : String) (rest : List String) (value : α)
    (hpre : st.pre = tok :: rest) (hopt : Core.optionLike tok = false)
    (hrun : FromArg.run tok = .ok value) :
    (Builder.arg α name metavar help hidden).run st
      = .ok value (Core.State.withPre st rest 1) := by
  simp [Builder.arg, Core.takePositionalValue?,
    takePositionalStep?_head _ st tok rest value hpre hopt hrun]

/-- An optional positional takes the head token. -/
theorem argOpt_accepts_head (α : Type) [FromArg α]
    (name : String) (metavar : Option String) (help : String) (hidden : Bool)
    (st : State) (tok : String) (rest : List String) (value : α)
    (hpre : st.pre = tok :: rest) (hopt : Core.optionLike tok = false)
    (hrun : FromArg.run tok = .ok value) :
    (Builder.argOpt α name metavar help hidden).run st
      = .ok (some value) (Core.State.withPre st rest 1) := by
  simp [Builder.argOpt, Core.takePositionalValue?,
    takePositionalStep?_head _ st tok rest value hpre hopt hrun]

/-- An optional positional is `none` on an exhausted stream. -/
theorem argOpt_none_when_exhausted (α : Type) [FromArg α]
    (name : String) (metavar : Option String) (help : String) (hidden : Bool)
    (st : State) (hpre : st.pre = []) (hpost : st.post = []) :
    (Builder.argOpt α name metavar help hidden).run st = .ok none st := by
  simp [Builder.argOpt, Core.takePositionalValue?,
    takePositionalStep?_stay _ st hpre hpost, Core.CollectStep.stay]

/-- A required positional reports `missingValue`, naming the metavar the user
had to type rather than the field name. -/
theorem arg_missing_when_exhausted (α : Type) [FromArg α]
    (name : String) (metavar : Option String) (help : String) (hidden : Bool)
    (st : State) (hpre : st.pre = []) (hpost : st.post = []) :
    (Builder.arg α name metavar help hidden).run st
      = .err { kind := .missingValue, context := []
             , expect := [.positional (metavar.getD name)] } := by
  simp [Builder.arg, Core.takePositionalValue?,
    takePositionalStep?_stay _ st hpre hpost, Core.CollectStep.stay,
    posParts, ItemSpec.metavar]

/-- A repeatable positional collects the head token. -/
theorem args_collects_head (α : Type) [FromArg α]
    (name : String) (metavar : Option String) (help : String) (hidden : Bool)
    (st : State) (tok : String) (value : α)
    (hpre : st.pre = [tok]) (hpost : st.post = [])
    (hopt : Core.optionLike tok = false)
    (hrun : FromArg.run tok = .ok value) :
    (Builder.args α name metavar help hidden).run st
      = .ok [value] (Core.State.withPre st [] 1) := by
  have hhead := takePositionalStep?_head
    (posParts α name metavar help .many false hidden).fst st tok [] value hpre hopt hrun
  have hnilPre : (Core.State.withPre st [] 1).pre = [] := by simp [Core.State.withPre]
  have hnilPost : (Core.State.withPre st [] 1).post = [] := by
    simp [Core.State.withPre, hpost]
  have hstay := takePositionalStep?_stay
    (posParts α name metavar help .many false hidden).fst _ hnilPre hnilPost
  simp only [Builder.args, Core.collectPositionalValues, Core.collectPositionalSteps]
  rw [collectStepsLoop_single _ _ (by simp [hpre]) st _ value tok hhead rfl rfl hstay]
  rfl

/-! ### Verb agreement

`toCmdSpec` lists exactly the names `toParser` dispatches on, at every depth.
Both walk the same `subs` list, so the induction has nothing to check at each
step -- which is the design working, not the proof being weak. -/

/-- Erasing the payload keeps the name. -/
@[simp] theorem toCmdSpec_name (c : Cmd α) : (c.toCmdSpec).name = c.name := by
  cases c <;> rfl

/-- The dispatch table is the pointwise image of the command list: entry `i`
takes both its name and its parser from command `i`.

This is the statement the name-only lemmas below do not make. A
`toSubcommands` that paired the first name with the second parser would satisfy
every theorem stated in terms of `List.map … name`, and dispatch would silently
run the wrong command. -/
theorem toSubcommands_eq_map (subs : List (Cmd α)) :
    Cmd.toSubcommands subs
      = subs.map (fun c => { name := c.name, parser := c.toParser }) := by
  induction subs with
  | nil => rfl
  | cons c rest ih => simp [Cmd.toSubcommands, ih]

/-- Dispatch entries carry the names of the commands they were built from. -/
theorem toSubcommands_names (subs : List (Cmd α)) :
    (Cmd.toSubcommands subs).map Core.Subcommand.name = subs.map Cmd.name := by
  simp [toSubcommands_eq_map]

/-- The render model lists the names of the commands it was built from. -/
theorem toCmdSpecs_names (subs : List (Cmd α)) :
    (Cmd.toCmdSpecs subs).map CmdSpec.name = subs.map Cmd.name := by
  induction subs with
  | nil => rfl
  | cons c rest ih => simp [Cmd.toCmdSpecs, ih]

/-- **Verb agreement.** The names help prints are the names dispatch accepts. -/
theorem verb_agreement (c : Cmd α) :
    (c.toCmdSpec).subs.map CmdSpec.name = c.subNames := by
  cases c with
  | leaf => rfl
  | node n m g subs => exact toCmdSpecs_names subs

/-- The two interpreters agree on the dispatch table itself, not merely on the
names taken separately. -/
theorem dispatch_agreement (c : Cmd α) :
    (c.toCmdSpec).subs.map CmdSpec.name =
      (Cmd.toSubcommands (match c with | .leaf _ _ _ => [] | .node _ _ _ subs => subs)).map
        Core.Subcommand.name := by
  cases c with
  | leaf => rfl
  | node n m g subs => rw [toSubcommands_names]; exact toCmdSpecs_names subs

/-- Walking the dispatch table for a token reaches the parser of the command
that token names.

`Core.subcommand`'s loop takes the first entry whose name matches, so the
command it reaches is the one `List.find?` picks out of the tree. -/
theorem loop_dispatch (subs : List (Cmd α)) (c : Cmd α)
    (expects : List Expect) (st : State)
    (token : String) (rest : List String)
    (h : subs.find? (fun s => s.name == token) = some c) :
    Core.subcommand.loop expects st token rest (Cmd.toSubcommands subs)
      = c.toParser (Core.State.withPre st rest 1) := by
  induction subs with
  | nil => simp at h
  | cons s tail ih =>
      rw [List.find?_cons] at h
      simp only [Cmd.toSubcommands, Core.subcommand.loop]
      by_cases hn : s.name = token
      · simp [hn] at h
        subst h
        simp [hn]
      · have hb : (s.name == token) = false := by simpa using hn
        rw [hb] at h
        rw [if_neg (Ne.symm hn)]
        exact ih h

/-- **Dispatch agreement, pointwise.** The entry named `foo` runs `foo`'s
parser.

`dispatch_agreement` above compares two lists of names, which is silent about
which parser sits behind each one. This says what the runner actually does: on
a leading token naming a command in the tree, dispatch runs *that* command's
parser, on the state with the verb consumed. -/
theorem subcommand_toSubcommands (subs : List (Cmd α)) (c : Cmd α)
    (st : State) (token : String) (rest : List String)
    (hpre : st.pre = token :: rest)
    (hfind : subs.find? (fun s => s.name == token) = some c) :
    Core.subcommand (Cmd.toSubcommands subs) st
      = c.toParser (Core.State.withPre st rest 1) := by
  cases subs with
  | nil => simp at hfind
  | cons s tail =>
      simp only [Core.subcommand, Cmd.toSubcommands, hpre]
      exact loop_dispatch (s :: tail) c _ st token rest hfind

/-- Verb agreement holds at every depth, not only at the root: it holds of
whatever command the runner descends to. -/
theorem verb_agreement_deep (c : Cmd α) (tokens : List String) :
    ((c.descend tokens).snd.toCmdSpec).subs.map CmdSpec.name =
      (c.descend tokens).snd.subNames :=
  verb_agreement _

/-- A command's render model carries exactly the items its parser was paired
with. -/
theorem toCmdSpec_args (c : Cmd α) : (c.toCmdSpec).args = c.items := by
  cases c <;> rfl

/-! ### Help coverage

Rendering is total on every constructible `Cmd`: nothing in `Doc`, `Spec`, or
`Exec` is declared `partial`, and Lean admits no non-terminating definition, so
totality holds by construction rather than by theorem. Stating it as one would
be a tautology dressed as a result. What is worth proving is that rendering is
not merely total but complete -- that every item the parser accepts reaches the
page. -/

/-- Every item gets its own row. -/
theorem mem_itemRows {item : ItemSpec} {items : List ItemSpec} (h : item ∈ items) :
    Doc.entryRow (Doc.itemLabel item)
        ((item.help?.getD "") ++ Doc.itemNotes item) ∈ Doc.itemRows items := by
  simpa [Doc.itemRows] using List.mem_map_of_mem h

/-- Rows are produced one per item: nothing is dropped and nothing is invented. -/
theorem itemRows_length (items : List ItemSpec) :
    (Doc.itemRows items).length = items.length := by
  simp [Doc.itemRows]

/-- **Help coverage.** Every visible item of a command is rendered. -/
theorem help_covers_visible {item : ItemSpec} {cmd : CmdSpec}
    (h : item ∈ visibleItems cmd.args) :
    Doc.entryRow (Doc.itemLabel item)
        ((item.help?.getD "") ++ Doc.itemNotes item) ∈ Doc.itemRows (visibleItems cmd.args) :=
  mem_itemRows h

/-- Coverage transported to a command tree: every visible item a command's
parser was paired with is rendered on that command's page. -/
theorem help_covers_cmd_items {item : ItemSpec} {c : Cmd α}
    (h : item ∈ visibleItems c.items) :
    Doc.entryRow (Doc.itemLabel item)
        ((item.help?.getD "") ++ Doc.itemNotes item)
      ∈ Doc.itemRows (visibleItems (c.toCmdSpec).args) := by
  rw [toCmdSpec_args]
  exact mem_itemRows h

/-- Every subcommand gets a row in the commands table. -/
theorem mem_subRows {sub : CmdSpec} {subs : List CmdSpec} (h : sub ∈ subs) :
    Doc.entryRow sub.name (sub.«meta».help?.getD "") ∈ Doc.subRows subs := by
  simpa [Doc.subRows] using List.mem_map_of_mem h

/-! ### Completion agreement

A verb that completes is a verb that parses, for the same reason verb agreement
holds: both read the same list. -/

/-- Completion offers every verb the command dispatches on. -/
theorem candidates_contain_verbs {cmd : CmdSpec} {name : String}
    (h : name ∈ cmd.subs.map CmdSpec.name) :
    name ∈ Doc.candidatesFor cmd := by
  simp only [Doc.candidatesFor, List.mem_eraseDups, List.mem_append]
  exact Or.inl (Or.inl h)

/-- Completion at a command tree's root offers exactly its dispatchable verbs. -/
theorem candidates_contain_cmd_verbs {c : Cmd α} {name : String}
    (h : name ∈ c.subNames) : name ∈ Doc.candidatesFor c.toCmdSpec := by
  apply candidates_contain_verbs
  rw [verb_agreement]
  exact h

/-! ### Diagnostics are sound

`Exec.unknownLong?` rewrites a parse failure into "unrecognised `--foo`". That
is a better message when it is right and a confusing one when it is wrong, so
what needs proving is that it never fires on something the command accepts. -/

/-- **`unknownLong?` never names something it was told about.** -/
theorem unknownLong?_sound {known tokens : List String} {name : String}
    (h : Exec.unknownLong? known tokens = some name) : name ∉ known := by
  obtain ⟨token, -, hf⟩ := List.exists_of_findSome?_eq_some h
  dsimp only at hf
  split at hf
  · split at hf
    · exact absurd hf (by simp)
    · rename_i hk
      simp only [Option.some.injEq] at hf
      subst hf
      simpa using hk
  · exact absurd hf (by simp)

/-- What it names is a long lexeme actually present in the stream, with any
`=value` suffix removed — not something invented. -/
theorem unknownLong?_provenance {known tokens : List String} {name : String}
    (h : Exec.unknownLong? known tokens = some name) :
    ∃ token ∈ tokens, token.startsWith "--" = true ∧ token ≠ "--"
      ∧ name = (token.splitOn "=").headD token := by
  obtain ⟨token, hmem, hf⟩ := List.exists_of_findSome?_eq_some h
  dsimp only at hf
  split at hf
  · rename_i hstart
    split at hf
    · exact absurd hf (by simp)
    · simp only [Option.some.injEq] at hf
      simp only [Bool.and_eq_true, bne_iff_ne, ne_eq] at hstart
      exact ⟨token, hmem, hstart.1, hstart.2, hf.symm⟩
  · exact absurd hf (by simp)

/-- Soundness in the shape `Exec.exec` uses it: the `legal` list it passes is the
lexemes of the items on the path plus the runner's own, so no lexeme of an item
the command accepts can be reported as unrecognised. -/
theorem unknownLong?_not_item_lexeme {items : List ItemSpec}
    {extra tokens : List String} {name : String} {item : ItemSpec}
    (h : Exec.unknownLong? (items.flatMap (·.lexemes) ++ extra) tokens = some name)
    (hitem : item ∈ items) : name ∉ item.lexemes := by
  intro hname
  exact unknownLong?_sound h
    (List.mem_append_left _ (List.mem_flatMap.mpr ⟨item, hitem, hname⟩))

/-- **`unknownShort?` never names a short the command accepts**, and never a
digit -- so a negative number standing in as a value is not mistaken for a
lexeme. What it names is a single dash and one character. -/
theorem unknownShort?_sound {shorts : List Char} {tokens : List String} {lexeme : String}
    (h : Exec.unknownShort? shorts tokens = some lexeme) :
    ∃ c, lexeme = String.ofList ['-', c] ∧ c ∉ shorts ∧ c.isDigit = false := by
  obtain ⟨token, -, hf⟩ := List.exists_of_findSome?_eq_some h
  split at hf
  · split at hf
    · exact absurd hf (by simp)
    · split at hf
      · exact absurd hf (by simp)
      · split at hf
        · exact absurd hf (by simp)
        · rename_i c _ _ _ hnotShort hnotDigit
          simp only [Option.some.injEq] at hf
          exact ⟨c, hf.symm, by simpa using hnotShort, by simpa using hnotDigit⟩
  · exact absurd hf (by simp)

/-- The same for the runner's builtins: `--help` is never reported as
unrecognised. -/
theorem unknownLong?_not_runner_lexeme {items : List ItemSpec}
    {extra tokens : List String} {name : String}
    (h : Exec.unknownLong? (items.flatMap (·.lexemes) ++ extra) tokens = some name) :
    name ∉ extra :=
  fun hname => unknownLong?_sound h (List.mem_append_right _ hname)

end ArgParse.Correspondence

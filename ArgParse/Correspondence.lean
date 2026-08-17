import ArgParse.Exec
import ArgParse.Core.Scan

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

end ArgParse.Correspondence

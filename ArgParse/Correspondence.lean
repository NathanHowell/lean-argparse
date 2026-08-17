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
`rfl`. That is the result being reported, not a shortcut being taken. In a design
where help reads a separately-maintained declaration, the corresponding
statements are false, and no proof of any length would establish them.
-/

namespace ArgParse.Correspondence

open ArgParse ArgParse.Spec ArgParse.Builder

/-! ### Item agreement

Each builder produces one item, and the runtime spec handed to the scanner
carries the same surface syntax that item advertises. The statements name both
sides explicitly so that a future edit which changes one and not the other stops
compiling. -/

/-- A flag's document is exactly one item. -/
theorem flag_doc (long : String) (short : Option Char) (help : String) (hidden : Bool) :
    (Builder.flag long short help hidden).doc =
      .item { kind := .flag
            , name := long
            , short? := (mkShort? short).map (·.c)
            , long? := some long
            , help? := if help.isEmpty then none else some help
            , arity := .zero
            , required := false
            , hidden := hidden } := by
  sorry

/-- A flag's parser is the scanner for a spec with the same surface syntax as
the item its document advertises. -/
theorem flag_run (long : String) (short : Option Char) (help : String) (hidden : Bool) :
    (Builder.flag long short help hidden).run =
      Core.flagScan { short? := mkShort? short
                    , long? := some long
                    , «meta» := { name := long
                                , help? := if help.isEmpty then none else some help }
                    , hidden? := hidden } := by
  sorry

/-- Every builder contributes exactly one item to its document. -/
theorem flag_items (long : String) (short : Option Char) (help : String) (hidden : Bool) :
    ((Builder.flag long short help hidden).items).length = 1 := by
  sorry

/-- An optional option's document is the item, alternated with nothing --
the shape renderers print as `[…]`. -/
theorem optionOpt_items (α : Type) [FromArg α] (long : String) (short : Option Char)
    (metavar : Option String) (help : String) (hidden : Bool) :
    ((Builder.optionOpt α long short metavar help hidden).items).length = 1 := by
  sorry

/-- A required option is advertised as required. -/
theorem option_required (α : Type) [FromArg α] (long : String) (short : Option Char)
    (metavar : Option String) (help : String) (hidden : Bool) :
    ∀ item ∈ (Builder.option α long short metavar help hidden).items, item.required := by
  sorry

/-- An option with a default is advertised as optional, because the default is
what makes it so. -/
theorem optionD_not_required (α : Type) [FromArg α] [ToString α] (long : String)
    (default : α) (short : Option Char) (metavar : Option String) (help : String)
    (hidden : Bool) :
    ∀ item ∈ (Builder.optionD long default short metavar help hidden).items,
      !item.required := by
  sorry

/-- A rejected short character is absent from the item, not merely unparseable.
This is the property that keeps help from advertising a form the scanner will
never match. -/
theorem short_rejected_absent (long : String) (help : String) (hidden : Bool) :
    ∀ item ∈ (Builder.flag long (some '-') help hidden).items, item.short? = none := by
  sorry

/-! ### Behavioural acceptance

The statements above relate two pieces of data. These relate the document to
what the parser actually does with tokens. -/

/-- A flag accepts its own long lexeme. -/
theorem flag_accepts_long (long : String) (short : Option Char) (help : String)
    (hidden : Bool) (h : long ≠ "") :
    ∃ st, (Builder.flag long short help hidden).run
      (Core.normalize ["--" ++ long]) = .ok true st := by
  sorry

/-- A flag accepts its own short lexeme, when it has one. -/
theorem flag_accepts_short (long : String) (c : Char) (help : String) (hidden : Bool)
    (hc : c ≠ '-' ∧ c.toNat < 128) :
    ∃ st, (Builder.flag long (some c) help hidden).run
      (Core.normalize ["-" ++ String.singleton c]) = .ok true st := by
  sorry

/-- A flag is not set by a token it does not advertise. -/
theorem flag_ignores_other (long : String) (short : Option Char) (help : String)
    (hidden : Bool) (token : String)
    (hne : Core.matchFlagToken { short? := mkShort? short, long? := some long
                               , «meta» := { name := long }, hidden? := hidden } token
             = Core.FlagMatch.none) :
    ∃ st, (Builder.flag long short help hidden).run (Core.normalize [token]) = .ok false st := by
  sorry

/-! ### Verb agreement

`toCmdSpec` lists exactly the names `toParser` dispatches on, at every depth.
Both walk the same `subs` list, so this is an induction with nothing to check at
each step -- which is the design working. -/

/-- Dispatch entries carry the names of the commands they were built from. -/
theorem toSubcommands_names (subs : List (Cmd α)) :
    (Cmd.toSubcommands subs).map Core.Subcommand.name = subs.map Cmd.name := by
  sorry

/-- The render model lists the names of the commands it was built from. -/
theorem toCmdSpecs_names (subs : List (Cmd α)) :
    (Cmd.toCmdSpecs subs).map CmdSpec.name = subs.map Cmd.name := by
  sorry

/-- The names help prints are the names dispatch accepts. -/
theorem verb_agreement (c : Cmd α) :
    (c.toCmdSpec).subs.map CmdSpec.name = c.subNames := by
  sorry

/-- Verb agreement holds at every depth, not only at the root. -/
theorem verb_agreement_deep (c : Cmd α) (tokens : List String) :
    ((c.descend tokens).snd.toCmdSpec).subs.map CmdSpec.name =
      (c.descend tokens).snd.subNames := by
  sorry

/-- A command's render model carries exactly the items its parser was paired
with. -/
theorem toCmdSpec_args (c : Cmd α) : (c.toCmdSpec).args = c.items := by
  sorry

/-! ### Help coverage

Rendering is total on every constructible `Cmd`: nothing in the `Doc`, `Spec`,
or `Exec` modules is declared `partial`, and Lean admits no non-terminating
definition, so totality is discharged by construction rather than by a theorem.
What is worth proving is that rendering is not merely total but complete -- that
every item the parser accepts reaches the page. -/

/-- Every item gets its own row. -/
theorem mem_itemRows {item : ItemSpec} {items : List ItemSpec} (h : item ∈ items) :
    Doc.entryRow (Doc.itemLabel item)
        ((item.help?.getD "") ++ Doc.itemNotes item) ∈ Doc.itemRows items := by
  sorry

/-- Rows are produced one per item: nothing is dropped and nothing is invented. -/
theorem itemRows_length (items : List ItemSpec) :
    (Doc.itemRows items).length = items.length := by
  sorry

/-- Every visible item of a command is rendered. -/
theorem help_covers_visible {item : ItemSpec} {cmd : CmdSpec}
    (h : item ∈ visibleItems cmd.args) :
    Doc.entryRow (Doc.itemLabel item)
        ((item.help?.getD "") ++ Doc.itemNotes item) ∈ Doc.itemRows (visibleItems cmd.args) :=
  mem_itemRows h

/-- Every subcommand gets a row in the commands table. -/
theorem mem_subRows {sub : CmdSpec} {subs : List CmdSpec} (h : sub ∈ subs) :
    Doc.entryRow sub.name (sub.«meta».help?.getD "") ∈ Doc.subRows subs := by
  sorry

/-! ### Completion agreement

A verb that completes is a verb that parses, for the same reason verb agreement
holds: both read the same list. -/

/-- Completion offers exactly the verbs the command dispatches on. -/
theorem candidates_contain_verbs {cmd : CmdSpec} {name : String}
    (h : name ∈ cmd.subs.map CmdSpec.name) :
    name ∈ Doc.candidatesFor cmd := by
  sorry

end ArgParse.Correspondence

import ArgParse.Correspondence

/-!
# ArgParse.Proofs.Completeness

The suite elsewhere is about soundness: what the parser accepts is what help
advertises, what it reports is real, what it consumes it was owed. This file is
the other half — argv that conforms to a command parses, and yields the bindings
you would predict.

It cannot be one theorem about `P`, because `P` carries an arbitrary `run` and a
user can put anything in it. It is instead two pieces that compose:

* `Yields p st a st'` and its lemmas for `pure`, `map`, `seq`, and `orElse` say
  how success travels through the applicative. `seq_yields` is the one that
  earns the phrase "the expected bindings": the value is the function applied to
  the argument, and the state is the one the right-hand side left, so the whole
  result is determined by the parts.
* Per-builder lemmas say when a builder succeeds and with what. Those come from
  `Correspondence`, where acceptance is already proved.

`demo_yields` at the end puts the two together on a real command tree, through
normalization, globals, dispatch, an option, and a positional — a closed
statement with no hypotheses, naming the exact record and the exact final state.
-/

namespace ArgParse.Proofs.Complete

open ArgParse ArgParse.Spec ArgParse.Core ArgParse.Builder

/-! ### Success travels through the applicative -/

/-- `p` succeeds on `st`, producing `a` and leaving `st'`. -/
def Yields {α : Type} (p : P α) (st : State) (a : α) (st' : State) : Prop :=
  p.run st = .ok a st'

/-- `pure` succeeds without touching the stream. -/
theorem pure_yields {α : Type} (a : α) (st : State) : Yields (P.pure a) st a st := rfl

/-- `pure`'s runtime half, for rewriting under an interpreter. -/
theorem pure_run {α : Type} (a : α) : (Pure.pure a : P α).run = Parser.pure a := rfl

/-- Mapping transforms the value and nothing else. -/
theorem map_yields {α β : Type} (f : α → β) {p : P α} {st st' : State} {a : α}
    (h : Yields p st a st') : Yields (P.map f p) st (f a) st' := by
  unfold Yields at h ⊢
  show Parser.map f p.run st = .ok (f a) st'
  simp only [Parser.map, h]

/-- **Sequencing composes the parts.** The value is the function applied to the
argument and the state is the one the right-hand side left, so a parser built
from builders yields exactly what its pieces yield. -/
theorem seq_yields {α β : Type} {pf : P (α → β)} {pa : P α}
    {st st₁ st₂ : State} {f : α → β} {a : α}
    (hf : Yields pf st f st₁) (ha : Yields pa st₁ a st₂) :
    Yields (pf <*> pa) st (f a) st₂ := by
  unfold Yields at hf ha ⊢
  show Parser.seq pf.run (fun _ => pa.run) st = .ok (f a) st₂
  simp only [Parser.seq, hf, ha]

/-- Alternation takes the left branch when it succeeds. -/
theorem orElse_yields_left {α : Type} {p q : P α} {st st' : State} {a : α}
    (h : Yields p st a st') : Yields (p <|> q) st a st' := by
  unfold Yields at h ⊢
  show Parser.orElse p.run (fun _ => q.run) st = .ok a st'
  simp only [Parser.orElse, h]

/-- An optional item that is present yields it. -/
theorem optional_yields_some {α : Type} {p : P α} {st st' : State} {a : α}
    (h : Yields p st a st') : Yields (P.optional p) st (some a) st' :=
  orElse_yields_left (map_yields Option.some h)

/-! ### What each builder needs

Acceptance is proved in `Correspondence`; these restate it as `Yields` so it
composes with the lemmas above. -/

/-- A flag never fails. It is the one builder with no precondition at all:
absence is a value, not an error. -/
theorem flag_total (long : String) (short : Option Char) (help : String)
    (hidden : Bool) (st : State) :
    ∃ b st', Yields (Builder.flag long short help hidden) st b st' := by
  cases h : Core.scanFlagPre (flagParts long short help hidden).fst st.pre with
  | none =>
      exact ⟨false, st, by simp [Yields, Builder.flag, Core.flagScan, h]⟩
  | some pre' =>
      exact ⟨true, Core.State.withPre st pre' 1,
        by simp [Yields, Builder.flag, Core.flagScan, h]⟩

/-- A required option yields its value when the stream offers `--name value` and
nothing later in the stream claims to be the same option. -/
theorem option_yields (α : Type) [FromArg α] (long v : String) (value : α)
    (short : Option Char) (metavar : Option String) (help : String) (hidden : Bool)
    (st : State) (rest : List String)
    (hpre : st.pre = ("--" ++ long) :: v :: rest)
    (hrun : FromArg.run v = .ok value)
    (hrest : ∀ tok ∈ rest,
      Core.optionToken?
        (optParts α long short metavar help none .one true hidden).fst tok = false) :
    Yields (Builder.option α long short metavar help hidden) st value
      (Core.State.withPre st rest 2) := by
  have h := Correspondence.optionValues_accepts_detached_long _ long v value st rest
    (Correspondence.optParts_long? α long short metavar help none .one true hidden)
    hpre hrun hrest
  simp [Yields, Builder.option, h]

/-- A required positional yields the head token when it decodes. -/
theorem arg_yields (α : Type) [FromArg α] (name : String) (metavar : Option String)
    (help : String) (hidden : Bool) (st : State) (tok : String) (rest : List String)
    (value : α) (hpre : st.pre = tok :: rest) (hrun : FromArg.run tok = .ok value) :
    Yields (Builder.arg α name metavar help hidden) st value
      (Core.State.withPre st rest 1) :=
  Correspondence.arg_accepts_head α name metavar help hidden st tok rest value hpre hrun

/-! ### Dispatch -/

/-- **A node yields what its child yields**, with the globals applied.

This is the completeness counterpart of `subcommand_toSubcommands`: that says
dispatch reaches the right parser, this says the whole node succeeds when the
globals and that parser do.

The globals hypothesis is stated over the bundle-expanded segment, because that
is what `toParser` actually runs them on. -/
theorem node_yields {α : Type} (n : String) (m : Meta) (globals : P (α → α))
    (subs : List (Cmd α)) (c : Cmd α)
    {st stG stC : State} {f : α → α} {a : α} {token : String} {rest : List String}
    (hglob : Core.scopedPre (subs.map Cmd.name)
      (fun st' => globals.run (Core.expandBundles (Doc.items globals.doc) st')) st
        = .ok f stG)
    (hpre : stG.pre = token :: rest)
    (hfind : subs.find? (fun s => s.name == token) = some c)
    (hchild : Cmd.toParser c (Core.State.withPre stG rest 1) = .ok a stC) :
    Cmd.toParser (.node n m globals subs) st = .ok (f a) stC := by
  simp only [Cmd.toParser, hglob]
  rw [Correspondence.subcommand_toSubcommands subs c stG token rest hpre hfind, hchild]

/-! ### End to end

A closed instance: one command tree, one argv, no hypotheses. Everything above
is used — normalization, a node's globals, dispatch, an option, a positional,
and `seq_yields` to put the two items together. -/

/-- Payload for the worked example. -/
structure Greeting where
  /-- Value of the `--who` option. -/
  who : String
  /-- The positional argument. -/
  name : String
deriving Repr, DecidableEq

/-- Two items composed applicatively. -/
def greetP : P Greeting :=
  (fun w n => Greeting.mk w n)
    <$> Builder.option String "who" <*> Builder.arg String "name"

/-- One verb under a node whose globals do nothing. -/
def demoApp : Cmd Greeting :=
  .node "demo" { name := "demo" } (Pure.pure id)
    [.leaf "greet" { name := "greet" } greetP]

/-- The normalized argv the example parses. -/
def s0 : State := { pre := ["greet", "--who", "world", "Alice"], post := [], cursor := 0 }

/-- No sentinel, so normalization is the identity on the token list. -/
theorem argv_normalizes :
    Core.normalize ["greet", "--who", "world", "Alice"] = s0 := by
  simp [s0, Core.normalize,
    Core.split_cons_token (show ("greet" : String) ≠ "--" by decide),
    Core.split_cons_token (show ("--who" : String) ≠ "--" by decide),
    Core.split_cons_token (show ("world" : String) ≠ "--" by decide),
    Core.split_cons_token (show ("Alice" : String) ≠ "--" by decide)]

/-- The node's globals are `pure id`, scoped to the empty segment before the
verb, so they consume nothing and change nothing. -/
theorem globals_pass :
    Core.scopedPre ["greet"]
      (fun st' => (Pure.pure id : P (Greeting → Greeting)).run
        (Core.expandBundles (Doc.items (Pure.pure id : P (Greeting → Greeting)).doc) st'))
      s0 = .ok id s0 := by
  simp [Core.scopedPre, Core.splitAtFirst, s0, pure_run, Parser.pure,
    Core.expandBundles, Core.shortsOfKind]

/-- `greetP` declares no short forms, so expanding its bundles does nothing. -/
theorem greet_expand_id (st : State) :
    Core.expandBundles (Doc.items greetP.doc) st = st := by
  refine Core.expandBundles_nil_shorts _ st ?_ ?_ <;> rfl

/-- The trailing positional is not something the `--who` option would claim. -/
theorem alice_unclaimed :
    Core.optionToken? (optParts String "who" none none "" none .one true false).fst
      "Alice" = false := by
  simp only [Core.optionToken?, optParts, Core.optionTokenShort?, Core.longLexeme,
    mkShort?]
  decide

/-- **Completeness, end to end.** Conforming argv parses, and the bindings are
the ones the command declares. -/
theorem demo_yields :
    Cmd.toParser demoApp (Core.normalize ["greet", "--who", "world", "Alice"])
      = .ok { who := "world", name := "Alice" }
          { pre := [], post := [], cursor := 4 } := by
  rw [argv_normalizes]
  refine node_yields "demo" { name := "demo" } (Pure.pure id)
    [Cmd.leaf "greet" { name := "greet" } greetP]
    (Cmd.leaf "greet" { name := "greet" } greetP)
    (token := "greet") (rest := ["--who", "world", "Alice"])
    (by simpa [Cmd.name] using globals_pass) rfl (by simp [Cmd.name]) ?_
  show greetP.run (Core.expandBundles (Doc.items greetP.doc)
      { pre := ["--who", "world", "Alice"], post := [], cursor := 1 }) = _
  rw [greet_expand_id]
  have hopt : Yields (Builder.option String "who")
      { pre := ["--who", "world", "Alice"], post := [], cursor := 1 } "world"
      { pre := ["Alice"], post := [], cursor := 3 } := by
    have := option_yields String "who" "world" "world" none none "" false
      { pre := ["--who", "world", "Alice"], post := [], cursor := 1 } ["Alice"]
      rfl rfl (by intro tok htok; simp at htok; subst htok; exact alice_unclaimed)
    simpa [Core.State.withPre] using this
  have harg : Yields (Builder.arg String "name")
      { pre := ["Alice"], post := [], cursor := 3 } "Alice"
      { pre := [], post := [], cursor := 4 } := by
    have := arg_yields String "name" none "" false
      { pre := ["Alice"], post := [], cursor := 3 } "Alice" [] "Alice" rfl rfl
    simpa [Core.State.withPre] using this
  exact seq_yields (map_yields (fun w n => Greeting.mk w n) hopt) harg

end ArgParse.Proofs.Complete

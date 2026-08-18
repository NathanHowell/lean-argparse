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

/-- A required positional yields the head token when it decodes and is not
something a flag or option was meant to claim. -/
theorem arg_yields (α : Type) [FromArg α] (name : String) (metavar : Option String)
    (help : String) (hidden : Bool) (st : State) (tok : String) (rest : List String)
    (value : α) (hpre : st.pre = tok :: rest) (hopt : Core.optionLike tok = false)
    (hrun : FromArg.run tok = .ok value) :
    Yields (Builder.arg α name metavar help hidden) st value
      (Core.State.withPre st rest 1) :=
  Correspondence.arg_accepts_head α name metavar help hidden st tok rest value
    hpre hopt hrun

/-! ### Dispatch -/

/-- **A node yields what its child yields**, with the globals applied.

This is the completeness counterpart of `subcommand_toSubcommands`: that says
dispatch reaches the right parser, this says the whole node succeeds when the
globals and that parser do.

The globals hypothesis is stated over the prepared segment -- bundles split and
positional tokens hoisted -- because that is what `toParser` runs them on. -/
theorem node_yields {α : Type} (n : String) (m : Meta) (globals : P (α → α))
    (subs : List (Cmd α)) (c : Cmd α)
    {st stG stC : State} {f : α → α} {a : α} {token : String} {rest : List String}
    (hglob : Core.scopedPre (subs.map Cmd.name)
      (fun st' => globals.run (Core.prepare (globals.items) st')) st
        = .ok f stG)
    (hpre : stG.pre = token :: rest)
    (hfind : subs.find? (fun s => s.name == token) = some c)
    (hchild : Cmd.toParser c (Core.State.withPre stG rest 1) = .ok a stC) :
    Cmd.toParser (.node n m globals subs) st = .ok (f a) stC := by
  simp only [Cmd.toParser, hglob]
  rw [Correspondence.subcommand_toSubcommands subs c stG token rest hpre hfind, hchild]

/-! ### End to end

A closed instance: one command tree, one argv, no hypotheses. Everything above
is used — normalization, a node's globals, dispatch, a positional, an option,
and `seq_yields` to put the two items together.

The positional is sequenced *first*, which used to be a way to write a broken
parser: it would take the front of the stream whatever was there. `Core.prepare`
is what makes it safe, and this is the shape that exercises it. -/

/-- Payload for the worked example. -/
structure Greeting where
  /-- The positional argument. -/
  name : String
  /-- Value of the `--who` option. -/
  who : String
deriving Repr, DecidableEq

/-- Two items composed applicatively, positional first. -/
def greetP : P Greeting :=
  (fun n w => Greeting.mk n w)
    <$> Builder.arg String "name" <*> Builder.option String "who"

/-- One verb under a node whose globals do nothing. -/
def demoApp : Cmd Greeting :=
  .node "demo" { name := "demo" } (Pure.pure id)
    [.leaf "greet" { name := "greet" } greetP]

/-- The normalized argv the example parses. -/
def s0 : State := { pre := ["greet", "Alice", "--who", "world"], post := [], cursor := 0 }

/-- The leaf's segment, after dispatch has consumed the verb. -/
def s1 : State := { pre := ["Alice", "--who", "world"], post := [], cursor := 1 }

/-- No sentinel, so normalization is the identity on the token list. -/
theorem argv_normalizes :
    Core.normalize ["greet", "Alice", "--who", "world"] = s0 := by
  simp [s0, Core.normalize,
    Core.split_cons_token (show ("greet" : String) ≠ "--" by decide),
    Core.split_cons_token (show ("Alice" : String) ≠ "--" by decide),
    Core.split_cons_token (show ("--who" : String) ≠ "--" by decide),
    Core.split_cons_token (show ("world" : String) ≠ "--" by decide)]

/-- The node's globals are `pure id`, scoped to the empty segment before the
verb, so they consume nothing and change nothing. -/
theorem globals_pass :
    Core.scopedPre ["greet"]
      (fun st' => (Pure.pure id : P (Greeting → Greeting)).run
        (Core.prepare ((Pure.pure id : P (Greeting → Greeting)).items) st'))
      s0 = .ok id s0 := by
  simp [Core.scopedPre, Core.splitAtFirst, s0, pure_run, Parser.pure,
    Core.prepare, Core.expandBundles, Core.shortsOfKind, Core.hoistPositionals,
    Spec.valueLexemes, Core.partitionClaimed]

/-- The only lexeme the leaf's non-positional items answer to. -/
theorem greet_switch_lexemes :
    ((greetP.items).filter (fun i => i.kind != .positional)).flatMap (·.lexemes)
      = ["--who"] := rfl

/-- And it takes a value. -/
theorem greet_value_lexemes :
    Spec.valueLexemes ((greetP.items).filter (fun i => i.kind != .positional))
      = ["--who"] := rfl

/-- Preparation leaves this segment alone: no short forms to split, and the
positional already sits ahead of the option that would have displaced it. -/
theorem greet_prepare_id : Core.prepare (greetP.items) s1 = s1 := by
  have hb := Core.expandBundles_nil_shorts (greetP.items) s1 rfl rfl
  rw [Core.prepare, hb, Core.hoistPositionals]
  simp only [greet_switch_lexemes, greet_value_lexemes, s1, Core.partitionClaimed,
    Core.lexemeClaims, List.any_cons, List.any_nil]
  repeat' split
  all_goals simp_all

/-- The trailing option is not something the positional would claim. -/
theorem nothing_after_who :
    ∀ tok ∈ ([] : List String),
      Core.optionToken? (Builder.optParts String "who" none none "" none .one true false).fst
        tok = false := by
  simp

/-- **Completeness, end to end.** Conforming argv parses, and the bindings are
the ones the command declares. -/
theorem demo_yields :
    Cmd.toParser demoApp (Core.normalize ["greet", "Alice", "--who", "world"])
      = .ok { name := "Alice", who := "world" }
          { pre := [], post := [], cursor := 4 } := by
  rw [argv_normalizes]
  refine node_yields "demo" { name := "demo" } (Pure.pure id)
    [Cmd.leaf "greet" { name := "greet" } greetP]
    (Cmd.leaf "greet" { name := "greet" } greetP)
    (token := "greet") (rest := ["Alice", "--who", "world"])
    (by simpa [Cmd.name] using globals_pass) rfl (by simp [Cmd.name]) ?_
  show greetP.run (Core.prepare (greetP.items) s1) = _
  rw [greet_prepare_id]
  have harg : Yields (Builder.arg String "name") s1 "Alice"
      { pre := ["--who", "world"], post := [], cursor := 2 } := by
    have := arg_yields String "name" none "" false s1 "Alice" ["--who", "world"]
      "Alice" rfl rfl rfl
    simpa [s1, Core.State.withPre] using this
  have hopt : Yields (Builder.option String "who")
      { pre := ["--who", "world"], post := [], cursor := 2 } "world"
      { pre := [], post := [], cursor := 4 } := by
    have := option_yields String "who" "world" "world" none none "" false
      { pre := ["--who", "world"], post := [], cursor := 2 } [] rfl rfl nothing_after_who
    simpa [Core.State.withPre] using this
  exact seq_yields (map_yields (fun n w => Greeting.mk n w) harg) hopt

end ArgParse.Proofs.Complete

import ArgParse.Proofs.Doc
import ArgParse.Proofs.Laws

/-!
# ArgParse.Proofs.PLaws

`Proofs/Laws.lean` proves the applicative laws for the runtime `Parser`. This
file does the same one layer up, for `P` — the pair of a parser and its
description, and the thing applications actually compose.

Both halves have to obey the laws for `P` to, and the description half is the
interesting one. `pure f <*> x` assembles `seq [none, x.doc]` where `f <$> x`
assembles `x.doc`, and `seq [seq [a, b], c]` is a different tree from
`seq [a, seq [b, c]]`. Those describe the same parse without being the same
term, which is why the laws once held only up to a normalization relation.

They hold outright now because the tree is not the representation. `P.doc` is a
`Doc.Normalized`, the constructors normalize, and `Doc.Normalized.seq` is a
monoid with `Doc.Normalized.empty` as its unit — so the trees above are not
merely equivalent, they are equal, and the equality is by `Doc.Normalized.ext`
off `empty_seq`, `seq_empty`, and `seq_assoc`. Every law here is therefore a
pair: the runtime half from `Proofs/Laws.lean`, the descriptive half from
`ArgParse.Doc`.

What the instance buys is that `P` composes under `do`-notation, `<*>`, and
every `Traversable`-style combinator with the rewriting the library expects, and
that `Proofs/Doc.lean`'s `items_seq` — help gains an entry exactly when the
parser gains an item — is stated about the same terms the laws rewrite.
-/

namespace ArgParse

namespace Proofs

/-! ### Functor laws

These need no normalization: `P.map` rewrites `run` and copies `doc`. -/

/-- Mapping the identity leaves the parser alone, description included. -/
theorem p_map_id {α : Type} (p : P α) : P.map id p = p := by
  cases p
  simp only [P.map, P.mk.injEq, true_and]
  exact parser_map_id _

/-- Mapping a composition is composing the maps. -/
theorem p_map_comp {α β γ : Type} (f : α → β) (g : β → γ) (p : P α) :
    P.map (g ∘ f) p = P.map g (P.map f p) := by
  simp [P.map, parser_map_comp]

instance : LawfulFunctor P where
  map_const := rfl
  id_map := by
    intro α x
    exact p_map_id x
  comp_map := by
    intro α β γ g h x
    exact p_map_comp (f := g) (g := h) (p := x)

/-! ### Applicative laws

`seqLeft`, `seqRight`, and `map_pure` are definitional. The other three pair a
runtime law with the corresponding monoid law on descriptions. -/

/-- Sequencing and discarding the right value uses the derived implementation. -/
theorem p_seqLeft_eq {α β : Type} (x : P α) (y : P β) :
    x <* y = Function.const β <$> x <*> y :=
  rfl

/-- Sequencing and discarding the left value uses the derived implementation. -/
theorem p_seqRight_eq {α β : Type} (x : P α) (y : P β) :
    x *> y = Function.const α id <$> x <*> y :=
  rfl

/-- Mapping over `pure` is `pure` of the applied value; neither side documents
anything. -/
theorem p_map_pure {α β : Type} (f : α → β) (a : α) :
    f <$> (Pure.pure a : P α) = Pure.pure (f a) := by
  show P.map f (P.pure a) = P.pure (f a)
  simp only [P.map, P.pure, P.mk.injEq, true_and]
  exact parser_map_pure f a

/-- Sequencing a `pure` function is mapping: `pure` documents nothing, and
nothing is the unit of `seq`. -/
theorem p_pure_seq {α β : Type} (f : α → β) (x : P α) :
    (Pure.pure f : P (α → β)) <*> x = f <$> x := by
  show P.seq (P.pure f) (fun _ => x) = P.map f x
  simp only [P.seq, P.pure, P.map, P.mk.injEq]
  exact ⟨Doc.Normalized.empty_seq x.doc, parser_seq_pure f x.run⟩

/-- Sequencing with a `pure` value is mapping application, by the same unit law
on the other side. -/
theorem p_seq_pure {α β : Type} (g : P (α → β)) (a : α) :
    g <*> (Pure.pure a : P α) = (fun f => f a) <$> g := by
  show P.seq g (fun _ => P.pure a) = P.map (fun f => f a) g
  simp only [P.seq, P.pure, P.map, P.mk.injEq]
  exact ⟨Doc.Normalized.seq_empty g.doc, parser_pure_seq g.run a⟩

/-- Sequencing is associative. The descriptions nest differently — `h` against
`(g, x)` versus `(h, g)` against `x` — and normalization sends both to the same
document. -/
theorem p_seq_assoc {α β γ : Type} (x : P α) (g : P (α → β)) (h : P (β → γ)) :
    h <*> (g <*> x) = (Function.comp <$> h) <*> g <*> x := by
  show P.seq h (fun _ => P.seq g (fun _ => x))
      = P.seq (P.seq (P.map Function.comp h) (fun _ => g)) (fun _ => x)
  simp only [P.seq, P.map, P.mk.injEq]
  exact ⟨Doc.Normalized.seq_assoc h.doc g.doc x.doc,
    (parser_seq_assoc_core h.run g.run x.run).symm⟩

/-- `P` is a lawful applicative: both halves obey the laws, the runtime one
because `Parser` does and the descriptive one because normalized documents form
a monoid under `seq`. -/
instance : LawfulApplicative P where
  seqLeft_eq := by
    intro α β x y
    exact p_seqLeft_eq x y
  seqRight_eq := by
    intro α β x y
    exact p_seqRight_eq x y
  pure_seq := by
    intro α β g x
    exact p_pure_seq g x
  map_pure := by
    intro α β g x
    exact p_map_pure g x
  seq_pure := by
    intro α β g x
    exact p_seq_pure g x
  seq_assoc := by
    intro α β γ x g h
    exact p_seq_assoc x g h

/-- The applicative identity law, in the form applications rely on: `pure id`
adds nothing to the parse and nothing to the help. -/
theorem p_id_seq {α : Type} (x : P α) : (Pure.pure id : P (α → α)) <*> x = x := by
  rw [p_pure_seq]
  exact p_map_id x

end Proofs

end ArgParse

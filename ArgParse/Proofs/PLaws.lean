import ArgParse.Proofs.Doc
import ArgParse.Proofs.Laws

/-!
# ArgParse.Proofs.PLaws

`Proofs/Laws.lean` proves the applicative laws for the runtime `Parser`. This
file does the same one layer up, for `P` — the pair of a parser and its
description, and the thing applications actually compose.

The functor laws hold on the nose, because `P.map` leaves `doc` untouched: a
function applied to the parsed value cannot change which items exist. So
`LawfulFunctor P` is a real instance.

`LawfulApplicative P` is not, and cannot be. `pure f <*> x` documents itself as
`seq [none, x.doc]` where `f <$> x` documents itself as `x.doc`, and
`seq [seq [a, b], c]` is a different tree from `seq [a, seq [b, c]]`. The
descriptions describe the same parse; they are not the same term. The honest
statement is that the laws hold up to `Doc.normalize`, which is exactly what
normalization is for and the only thing it is for.

`P.Equiv` is that statement: identical runtime behaviour, and descriptions that
normalize to the same tree. `Proofs/Doc.lean` supplies the two facts that make
it usable — normalization preserves items, so equivalent parsers document the
same items, and `flattenSeq` distributes over append, which is where
associativity comes from.

Stated with propositional equality throughout: `Doc` has no `DecidableEq`,
since no deriving handler covers an inductive that nests through `List`.
-/

namespace ArgParse

namespace P

/-- Two paired parsers agree when they run identically and their descriptions
normalize to the same tree.

The runtime halves must match exactly — normalization is a statement about
`Doc` and buys the parser nothing. -/
def Equiv {α : Type} (p q : P α) : Prop :=
  p.run = q.run ∧ p.doc.normalize = q.doc.normalize

end P

namespace Proofs

/-! ### `Equiv` is an equivalence -/

/-- Reflexivity. -/
theorem equiv_refl {α : Type} (p : P α) : P.Equiv p p :=
  ⟨rfl, rfl⟩

/-- Symmetry. -/
theorem equiv_symm {α : Type} {p q : P α} (h : P.Equiv p q) : P.Equiv q p :=
  ⟨h.1.symm, h.2.symm⟩

/-- Transitivity. -/
theorem equiv_trans {α : Type} {p q r : P α}
    (hpq : P.Equiv p q) (hqr : P.Equiv q r) : P.Equiv p r :=
  ⟨hpq.1.trans hqr.1, hpq.2.trans hqr.2⟩

/-- Equivalent parsers document the same items. This is what makes `Equiv` the
right relation to state the laws up to: rewriting along a law can reassociate
the description tree, but it cannot change what help prints. -/
theorem equiv_items {α : Type} {p q : P α} (h : P.Equiv p q) :
    p.items = q.items := by
  show Doc.items p.doc = Doc.items q.doc
  rw [← items_normalize p.doc, ← items_normalize q.doc, h.2]

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

`seqLeft`, `seqRight`, and `map_pure` are definitional; the remaining three are
equalities only after normalization. -/

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

/-- Sequencing a `pure` function is mapping, once the leading `none` the `pure`
left in the description is normalized away. -/
theorem p_pure_seq {α β : Type} (f : α → β) (x : P α) :
    P.Equiv (Pure.pure f <*> x) (f <$> x) := by
  refine ⟨parser_seq_pure f x.run, ?_⟩
  show (Doc.seq [Doc.none, x.doc]).normalize = x.doc.normalize
  rw [normalize_seq_congr
        (b := [x.doc])
        (by rw [show ([Doc.none, x.doc] : List Doc) = [Doc.none] ++ [x.doc] from rfl,
              flattenSeq_append, flattenSeq_none, List.nil_append]),
      normalize_seq_singleton]

/-- Sequencing with a `pure` value is mapping application, once the trailing
`none` is normalized away. -/
theorem p_seq_pure {α β : Type} (g : P (α → β)) (a : α) :
    P.Equiv (g <*> (Pure.pure a : P α)) ((fun f => f a) <$> g) := by
  refine ⟨parser_pure_seq g.run a, ?_⟩
  show (Doc.seq [g.doc, Doc.none]).normalize = g.doc.normalize
  rw [normalize_seq_congr
        (b := [g.doc])
        (by rw [show ([g.doc, Doc.none] : List Doc) = [g.doc] ++ [Doc.none] from rfl,
              flattenSeq_append, flattenSeq_none, List.append_nil]),
      normalize_seq_singleton]

/-- Sequencing is associative up to normalization. The two sides nest their
descriptions differently — `seq [h, seq [g, x]]` against `seq [seq [h, g], x]` —
and `flattenSeq` sends both to the same flat list, because it distributes over
append. -/
theorem p_seq_assoc {α β γ : Type} (x : P α) (g : P (α → β)) (h : P (β → γ)) :
    P.Equiv (h <*> (g <*> x)) ((Function.comp <$> h) <*> g <*> x) := by
  refine ⟨(parser_seq_assoc_core h.run g.run x.run).symm, ?_⟩
  show (Doc.seq [h.doc, Doc.seq [g.doc, x.doc]]).normalize
      = (Doc.seq [Doc.seq [h.doc, g.doc], x.doc]).normalize
  refine normalize_seq_congr ?_
  rw [show ([h.doc, Doc.seq [g.doc, x.doc]] : List Doc)
        = [h.doc] ++ [Doc.seq [g.doc, x.doc]] from rfl,
      show ([Doc.seq [h.doc, g.doc], x.doc] : List Doc)
        = [Doc.seq [h.doc, g.doc]] ++ [x.doc] from rfl,
      flattenSeq_append, flattenSeq_append, flattenSeq_seq, flattenSeq_seq,
      show ([g.doc, x.doc] : List Doc) = [g.doc] ++ [x.doc] from rfl,
      show ([h.doc, g.doc] : List Doc) = [h.doc] ++ [g.doc] from rfl,
      flattenSeq_append, flattenSeq_append, List.append_assoc]

/-- The applicative identity law, in the form applications rely on: `pure id`
adds nothing to the parse and nothing to the help. -/
theorem p_id_seq {α : Type} (x : P α) : P.Equiv (Pure.pure id <*> x) x := by
  refine equiv_trans (p_pure_seq id x) ?_
  rw [show (id <$> x) = P.map id x from rfl, p_map_id]
  exact equiv_refl x

end Proofs

end ArgParse

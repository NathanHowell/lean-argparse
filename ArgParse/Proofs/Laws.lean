import ArgParse.Core.Parser

/-!
# ArgParse.Proofs.Laws

Foundational Functor, Applicative, and Alternative laws for the core `Parser`.
The statements follow the operations defined in `ArgParse.Core.Parser` and are
expressed as definitional equalities of parser functions.
-/

namespace ArgParse.Proofs

open ArgParse

/-- Mapping the identity function does not change the parser. -/
theorem parser_map_id {α : Type} (p : Parser α) :
    Parser.map (fun x => x) p = p := by
  funext st
  cases h : p st with
  | err e => simp [Parser.map, h]
  | ok a st' => simp [Parser.map, h]

/-- Mapping the composition of two functions equals composing their maps. -/
theorem parser_map_comp {α β γ : Type}
    (f : α → β) (g : β → γ) (p : Parser α) :
    Parser.map (g ∘ f) p = Parser.map g (Parser.map f p) := by
  funext st
  cases h : p st with
  | err e => simp [Parser.map, h]
  | ok a st' => simp [Parser.map, h, Function.comp]

/-- Mapping a constant function matches sequencing with a pure constant. -/
theorem parser_map_const {α β : Type} (x : β) (p : Parser α) :
    Parser.map (fun _ => x) p =
      Parser.seq (Parser.pure (fun _ : α => x)) (fun _ => p) := by
  funext st
  cases h : p st with
  | err e => simp [Parser.map, Parser.seq, Parser.pure, h]
  | ok a st' => simp [Parser.map, Parser.seq, Parser.pure, h]

/-- Sequencing a pure function is equivalent to mapping. -/
theorem parser_seq_pure {α β : Type}
    (f : α → β) (pa : Parser α) :
    Parser.seq (Parser.pure f) (fun _ => pa) = Parser.map f pa := by
  funext st
  simp [Parser.seq, Parser.pure, Parser.map]

/-- Sequencing with a pure value is equivalent to mapping application. -/
theorem parser_pure_seq {α β : Type}
    (pf : Parser (α → β)) (x : α) :
    Parser.seq pf (fun _ => Parser.pure x) =
      Parser.map (fun f => f x) pf := by
  funext st
  cases h : pf st with
  | err e => simp [Parser.seq, Parser.map, h]
  | ok f st' => simp [Parser.seq, Parser.map, Parser.pure, h]

/-- Associativity of the primitive sequencing operator, expressed via `Parser.seq`. -/
theorem parser_seq_assoc_core {α β γ : Type}
    (pf : Parser (β → γ)) (pg : Parser (α → β)) (pa : Parser α) :
    Parser.seq
        (Parser.seq (Parser.map Function.comp pf) (fun _ => pg))
        (fun _ => pa)
      =
      Parser.seq pf (fun _ => Parser.seq pg (fun _ => pa)) := by
  funext st
  cases h₁ : pf st with
  | err e => simp [Parser.seq, Parser.map, h₁]
  | ok f st₁ =>
      cases h₂ : pg st₁ with
      | err e => simp [Parser.seq, Parser.map, h₁, h₂]
      | ok g st₂ =>
          cases h₃ : pa st₂ with
          | err e => simp [Parser.seq, Parser.map, h₁, h₂, h₃]
          | ok a st₃ => simp [Parser.seq, Parser.map, h₁, h₂, h₃, Function.comp]

/-- Mapping over a pure value is equivalent to applying the function first. -/
theorem parser_map_pure {α β : Type}
    (f : α → β) (x : α) :
    Parser.map f (Parser.pure x) = Parser.pure (f x) := by
  funext st
  simp [Parser.map, Parser.pure]

/-- Sequencing and discarding the right-hand side uses the derived implementation. -/
theorem parser_seqLeft_eq {α β : Type}
    (x : Parser α) (y : Parser β) :
    x <* y = Function.const β <$> x <*> y := by
  rfl

/-- Sequencing and discarding the left-hand side uses the derived implementation. -/
theorem parser_seqRight_eq {α β : Type}
    (x : Parser α) (y : Parser β) :
    x *> y = Function.const α id <$> x <*> y := by
  rfl

-- Each law is the corresponding core lemma with `<$>`/`<*>`/`<*`/`*>` in place
-- of `Parser.map`/`Parser.seq`. The instances make the two notations
-- definitionally equal, so unification converts them and no rewriting is needed.
instance : LawfulFunctor Parser where
  map_const := rfl
  id_map := by
    intro α x
    exact parser_map_id (p := x)
  comp_map := by
    intro α β γ g h x
    exact parser_map_comp (f := g) (g := h) (p := x)

instance : LawfulApplicative Parser where
  seqLeft_eq := by
    intro α β x y
    exact parser_seqLeft_eq (x := x) (y := y)
  seqRight_eq := by
    intro α β x y
    exact parser_seqRight_eq (x := x) (y := y)
  pure_seq := by
    intro α β g x
    exact parser_seq_pure (f := g) (pa := x)
  map_pure := by
    intro α β g x
    exact parser_map_pure (f := g) (x := x)
  seq_pure := by
    intro α β g x
    exact parser_pure_seq (pf := g) (x := x)
  seq_assoc := by
    intro α β γ x g h
    exact (parser_seq_assoc_core (pf := h) (pg := g) (pa := x)).symm

end ArgParse.Proofs

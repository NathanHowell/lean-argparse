import ArgParse.P

/-!
# ArgParse.Proofs.Doc

`Doc.normalize` is described in `ArgParse.P` as a rendering-quality pass that
"never touches parsing". Half of that is free: `run` is not in scope where
`normalize` is defined, so it cannot change how anything parses. The other half
— that it does not change what is *documented* — is what this file proves.

That half is the one worth proving, because it is where a bug would hide in
silence. If `flattenSeq` dropped a child, help would quietly stop mentioning an
option and nothing would fail. `items_normalize` rules that out: normalization
preserves the item list exactly, order included.

The second result is idempotence. It is proved the standard way rather than
directly: `Normal` spells out the shape `normalize` produces, `normalize_normal`
says every output has that shape, and `normalize_eq_self` says every document
with that shape is already a fixed point. Idempotence falls out of the two.
Stating the shape explicitly is what makes the second half provable at all — the
singleton collapse means `normalize` never emits a one-child `seq`, and without
recording that, `normalize (normalize d) = normalize d` has no handle to grab.

Everything recurses through `List Doc`, so each definition and proof comes as a
mutual group with its list form, exactly as `items` does.
-/

namespace ArgParse

namespace Doc

/-! ### Shape tests

Three one-line discriminators. `Normal` needs to say "this child is not a
`none`" and "this child is not a `seq`", and a `Bool` test states that more
briefly than an existential inequality and rewrites better. -/

/-- Whether the document is the empty `none` node. -/
def isNone : Doc → Bool
  | .none => true
  | _ => false

/-- Whether the document is a `seq` node. -/
def isSeq : Doc → Bool
  | .seq _ => true
  | _ => false

/-- Whether the document is an `alt` node. -/
def isAlt : Doc → Bool
  | .alt _ => true
  | _ => false

/-! ### Normal form

The shape `normalize` produces, read straight off its definition: no `seq` or
`alt` has fewer than two children (the singleton collapse), a `seq` child is
never a `none` or a nested `seq` (the splice and the drop), an `alt` child is
never a nested `alt` — but *may* be a `none`, since `alt [d, none]` is how
optionality is spelled — and a `many` never wraps a `none`. -/

mutual

/-- The document is in the shape `normalize` produces. -/
def Normal : Doc → Prop
  | .item _ => True
  | .none => True
  | .many d => Normal d ∧ d.isNone = false
  | .seq ds => 2 ≤ ds.length ∧ NormalSeq ds
  | .alt ds => 2 ≤ ds.length ∧ NormalAlt ds

/-- Children admissible under a normalized `seq`: normal, and neither a `none`
nor a nested `seq`. -/
def NormalSeq : List Doc → Prop
  | [] => True
  | d :: rest => Normal d ∧ d.isNone = false ∧ d.isSeq = false ∧ NormalSeq rest

/-- Children admissible under a normalized `alt`: normal and not a nested `alt`.
A `none` child is allowed — it is what makes an item optional. -/
def NormalAlt : List Doc → Prop
  | [] => True
  | d :: rest => Normal d ∧ d.isAlt = false ∧ NormalAlt rest

end

end Doc

namespace Proofs

/-! ### Item preservation -/

/-- `itemsList` distributes over list append, which is what the `seq`/`alt`
splices need. -/
theorem itemsList_append (a b : List Doc) :
    Doc.itemsList (a ++ b) = Doc.itemsList a ++ Doc.itemsList b := by
  induction a with
  | nil => simp [Doc.itemsList]
  | cons _ _ ih => simp [Doc.itemsList, ih, List.append_assoc]

mutual

/-- Normalization preserves the documented items, in order. This is the guard
against a silent help-loss bug: nothing `normalize` does can drop, duplicate, or
reorder what help will print. -/
theorem items_normalize (d : Doc) : Doc.items d.normalize = Doc.items d := by
  match d with
  | .item _ => simp [Doc.normalize]
  | .none => simp [Doc.normalize]
  | .many d =>
      have ih := items_normalize d
      simp only [Doc.normalize]
      split <;> rename_i h <;> simp only [Doc.items, ← ih, h, Doc.items]
  | .seq ds =>
      have ih := itemsList_flattenSeq ds
      simp only [Doc.normalize, Doc.items, ← ih]
      split <;> rename_i h <;> simp [Doc.items, Doc.itemsList, h]
  | .alt ds =>
      have ih := itemsList_flattenAlt ds
      simp only [Doc.normalize, Doc.items, ← ih]
      split <;> rename_i h <;> simp [Doc.items, Doc.itemsList, h]

/-- Flattening a `seq`'s children preserves their items. Dropping a `none` is
free because it contributes none, and splicing a nested `seq` is `itemsList`
distributing over append. -/
theorem itemsList_flattenSeq (ds : List Doc) :
    Doc.itemsList (Doc.flattenSeq ds) = Doc.itemsList ds := by
  match ds with
  | [] => simp [Doc.flattenSeq]
  | d :: rest =>
      have ihd := items_normalize d
      have iht := itemsList_flattenSeq rest
      simp only [Doc.flattenSeq]
      split <;> rename_i h <;>
        simp [Doc.itemsList, itemsList_append, iht, ← ihd, h, Doc.items]

/-- Flattening an `alt`'s children preserves their items. -/
theorem itemsList_flattenAlt (ds : List Doc) :
    Doc.itemsList (Doc.flattenAlt ds) = Doc.itemsList ds := by
  match ds with
  | [] => simp [Doc.flattenAlt]
  | d :: rest =>
      have ihd := items_normalize d
      have iht := itemsList_flattenAlt rest
      simp only [Doc.flattenAlt]
      split <;> rename_i h <;>
        simp [Doc.itemsList, itemsList_append, iht, ← ihd, h, Doc.items]

end

/-- The same statement one level up: normalizing a parser's description does not
change the items the parser is documented to accept. -/
theorem items_normalize_doc {α : Type} (p : P α) :
    Doc.items p.doc.normalize = p.items :=
  items_normalize p.doc

/-! ### Idempotence

`normalize` lands in `Normal`, and `Normal` documents are fixed points. -/

/-- A list that is neither empty nor a singleton has at least two elements. This
is the shape the `split` on the singleton collapse leaves behind. -/
theorem two_le_length {α : Type} {l : List α}
    (hnil : l ≠ []) (hone : ∀ a, l ≠ [a]) : 2 ≤ l.length := by
  match l with
  | [] => exact absurd rfl hnil
  | [a] => exact absurd rfl (hone a)
  | _ :: _ :: _ => simp

/-- Restate "not a `none`" as the `Bool` test `Normal` is phrased with. -/
theorem isNone_eq_false {d : Doc} (h : d ≠ .none) : d.isNone = false := by
  cases d <;> first | rfl | exact absurd rfl h

/-- Restate "not a `seq`" as the `Bool` test `NormalSeq` is phrased with. -/
theorem isSeq_eq_false {d : Doc} (h : ∀ l, d ≠ .seq l) : d.isSeq = false := by
  cases d <;> first | rfl | exact absurd rfl (h _)

/-- Restate "not an `alt`" as the `Bool` test `NormalAlt` is phrased with. -/
theorem isAlt_eq_false {d : Doc} (h : ∀ l, d ≠ .alt l) : d.isAlt = false := by
  cases d <;> first | rfl | exact absurd rfl (h _)

/-- `NormalSeq` is closed under append, which is what the nested-`seq` splice
needs. -/
theorem normalSeq_append {a b : List Doc}
    (ha : Doc.NormalSeq a) (hb : Doc.NormalSeq b) : Doc.NormalSeq (a ++ b) := by
  induction a with
  | nil => simpa using hb
  | cons _ _ ih =>
      obtain ⟨h1, h2, h3, h4⟩ := ha
      exact ⟨h1, h2, h3, ih h4⟩

/-- `NormalAlt` is closed under append, which is what the nested-`alt` splice
needs. -/
theorem normalAlt_append {a b : List Doc}
    (ha : Doc.NormalAlt a) (hb : Doc.NormalAlt b) : Doc.NormalAlt (a ++ b) := by
  induction a with
  | nil => simpa using hb
  | cons _ _ ih =>
      obtain ⟨h1, h2, h3⟩ := ha
      exact ⟨h1, h2, ih h3⟩

mutual

/-- Every output of `normalize` is in normal form. -/
theorem normalize_normal (d : Doc) : Doc.Normal d.normalize := by
  match d with
  | .item _ => trivial
  | .none => trivial
  | .many d =>
      have ih := normalize_normal d
      simp only [Doc.normalize]
      split
      · trivial
      · rename_i h
        exact ⟨ih, isNone_eq_false h⟩
  | .seq ds =>
      have ih := flattenSeq_normalSeq ds
      simp only [Doc.normalize]
      split
      · trivial
      · rename_i a h
        rw [h] at ih
        exact ih.1
      · rename_i hnil hone
        exact ⟨two_le_length hnil hone, ih⟩
  | .alt ds =>
      have ih := flattenAlt_normalAlt ds
      simp only [Doc.normalize]
      split
      · trivial
      · rename_i a h
        rw [h] at ih
        exact ih.1
      · rename_i hnil hone
        exact ⟨two_le_length hnil hone, ih⟩

/-- Flattening a `seq`'s children yields children admissible under a normalized
`seq`. -/
theorem flattenSeq_normalSeq (ds : List Doc) :
    Doc.NormalSeq (Doc.flattenSeq ds) := by
  match ds with
  | [] => trivial
  | d :: rest =>
      have ihd := normalize_normal d
      have iht := flattenSeq_normalSeq rest
      simp only [Doc.flattenSeq]
      split
      · exact iht
      · rename_i inner h
        rw [h] at ihd
        exact normalSeq_append ihd.2 iht
      · rename_i hnone hseq
        exact ⟨ihd, isNone_eq_false hnone, isSeq_eq_false hseq, iht⟩

/-- Flattening an `alt`'s children yields children admissible under a normalized
`alt`. -/
theorem flattenAlt_normalAlt (ds : List Doc) :
    Doc.NormalAlt (Doc.flattenAlt ds) := by
  match ds with
  | [] => trivial
  | d :: rest =>
      have ihd := normalize_normal d
      have iht := flattenAlt_normalAlt rest
      simp only [Doc.flattenAlt]
      split
      · rename_i inner h
        rw [h] at ihd
        exact normalAlt_append ihd.2 iht
      · rename_i halt
        exact ⟨ihd, isAlt_eq_false halt, iht⟩

end

mutual

/-- A document already in normal form is a fixed point of `normalize`. -/
theorem normalize_eq_self (d : Doc) (h : Doc.Normal d) : d.normalize = d := by
  match d with
  | .item _ => rfl
  | .none => rfl
  | .many d =>
      obtain ⟨hd, hne⟩ := h
      simp only [Doc.normalize, normalize_eq_self d hd]
      cases d <;> first | rfl | simp [Doc.isNone] at hne
  | .seq ds =>
      obtain ⟨hlen, hns⟩ := h
      have hf := flattenSeq_eq_self ds hns
      rcases ds with _ | ⟨a, _ | ⟨b, t⟩⟩
      · simp at hlen
      · simp at hlen
      · simp only [Doc.normalize, hf]
  | .alt ds =>
      obtain ⟨hlen, hna⟩ := h
      have hf := flattenAlt_eq_self ds hna
      rcases ds with _ | ⟨a, _ | ⟨b, t⟩⟩
      · simp at hlen
      · simp at hlen
      · simp only [Doc.normalize, hf]

/-- Children already admissible under a normalized `seq` survive flattening
unchanged. -/
theorem flattenSeq_eq_self (ds : List Doc) (h : Doc.NormalSeq ds) :
    Doc.flattenSeq ds = ds := by
  match ds with
  | [] => rfl
  | d :: rest =>
      obtain ⟨hd, hnone, hseq, hrest⟩ := h
      simp only [Doc.flattenSeq, normalize_eq_self d hd,
        flattenSeq_eq_self rest hrest]
      cases d with
      | item => rfl
      | seq => exact absurd hseq (by simp [Doc.isSeq])
      | alt => rfl
      | many => rfl
      | none => exact absurd hnone (by simp [Doc.isNone])

/-- Children already admissible under a normalized `alt` survive flattening
unchanged. -/
theorem flattenAlt_eq_self (ds : List Doc) (h : Doc.NormalAlt ds) :
    Doc.flattenAlt ds = ds := by
  match ds with
  | [] => rfl
  | d :: rest =>
      obtain ⟨hd, halt, hrest⟩ := h
      simp only [Doc.flattenAlt, normalize_eq_self d hd,
        flattenAlt_eq_self rest hrest]
      cases d <;> first | rfl | simp [Doc.isAlt] at halt

end

/-- Normalization is idempotent: a second pass has nothing left to do. -/
theorem normalize_idem (d : Doc) : d.normalize.normalize = d.normalize :=
  normalize_eq_self _ (normalize_normal d)

/-- Flattening is stable: its output is already flat. -/
theorem flattenSeq_flattenSeq (ds : List Doc) :
    Doc.flattenSeq (Doc.flattenSeq ds) = Doc.flattenSeq ds :=
  flattenSeq_eq_self _ (flattenSeq_normalSeq ds)

/-! ### `seq` associativity

`normalize (.seq ds)` reads `ds` only through `flattenSeq`, and `flattenSeq` is
a monoid homomorphism from `List Doc` under `++`. Those two facts are what make
`seq` associative up to normalization, which is what the applicative law needs.
-/

/-- `flattenSeq` distributes over append: each child is flattened on its own,
independently of what surrounds it. -/
theorem flattenSeq_append (a b : List Doc) :
    Doc.flattenSeq (a ++ b) = Doc.flattenSeq a ++ Doc.flattenSeq b := by
  induction a with
  | nil => simp [Doc.flattenSeq]
  | cons d rest ih =>
      cases hd : Doc.normalize d <;>
        simp [Doc.flattenSeq, hd, ih, List.append_assoc]

/-- Normalizing a `seq` depends on its children only through `flattenSeq`. -/
theorem normalize_seq_congr {a b : List Doc}
    (h : Doc.flattenSeq a = Doc.flattenSeq b) :
    (Doc.seq a).normalize = (Doc.seq b).normalize := by
  simp only [Doc.normalize, h]

/-- A `none` child contributes nothing to a `seq`. -/
theorem flattenSeq_none : Doc.flattenSeq [Doc.none] = [] := by
  simp [Doc.flattenSeq, Doc.normalize]

/-- A nested `seq` child contributes exactly its own children. -/
theorem flattenSeq_seq (ds : List Doc) :
    Doc.flattenSeq [Doc.seq ds] = Doc.flattenSeq ds := by
  have hns := flattenSeq_normalSeq ds
  simp only [Doc.flattenSeq, Doc.normalize]
  rcases hl : Doc.flattenSeq ds with _ | ⟨a, _ | ⟨b, t⟩⟩
  · simp
  · rw [hl] at hns
    obtain ⟨ha, hnone, hseq, -⟩ := hns
    cases a with
    | item => rfl
    | seq => exact absurd hseq (by simp [Doc.isSeq])
    | alt => rfl
    | many => rfl
    | none => exact absurd hnone (by simp [Doc.isNone])
  · simp

/-- A one-child `seq` normalizes to what its child normalizes to. -/
theorem normalize_seq_singleton (d : Doc) :
    (Doc.seq [d]).normalize = d.normalize := by
  have hn := normalize_normal d
  simp only [Doc.normalize, Doc.flattenSeq]
  cases hd : Doc.normalize d with
  | seq inner =>
      rw [hd] at hn
      obtain ⟨hlen, -⟩ := hn
      rcases inner with _ | ⟨a, _ | ⟨b, t⟩⟩
      · simp at hlen
      · simp at hlen
      · simp
  | _ => simp

end Proofs

end ArgParse

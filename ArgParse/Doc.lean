import ArgParse.Spec.Item

/-!
# ArgParse.Doc

The static skeleton of a parser — what help, usage, and completion read — and
the normal form every skeleton is kept in.

`Doc` is the free applicative's shadow: the shape of a parse with the payloads
deleted. Rendering quality is a property of that shape alone. Flattening a
nested `seq` or dropping the `none` a `pure` leaves behind changes how help
reads and cannot change how anything parses, because `run` is not in scope
here.

`Doc.Normalized` is the shape after that pass, carrying the proof that it *is*
that shape. `ArgParse.P` stores one, which is what makes `P` lawfully
applicative: `pure f <*> x` and `f <$> x` build different trees, but they
normalize to the same one, and a type that admits only normal trees cannot tell
the two apart. Normalization stops being a convention the constructors happen
to respect and becomes a property of the type.

This file therefore sits below `P`: the invariant has to be stated before the
structure that carries it. It sits below `Spec.AST` for the same reason —
`CmdSpec` carries a `Doc`, because a usage synopsis has to read how the items
compose. `Proofs/Doc.lean` sits above and proves the other half — that
normalization does not change what is documented.
-/

namespace ArgParse

open ArgParse.Spec

/-- The static skeleton of a parser: what help, usage, and completion read. -/
inductive Doc where
  /-- One flag, option, or positional. -/
  | item (i : ItemSpec)
  /-- Applicative composition: every child participates. -/
  | seq (ds : List Doc)
  /-- Alternation (`<|>`): exactly one child participates. -/
  | alt (ds : List Doc)
  /-- Repetition. `atLeastOne` separates `P.some` from `P.many`: both repeat,
  but only one of them may be satisfied by nothing at all, and a synopsis that
  cannot tell them apart has to guess. -/
  | many (d : Doc) (atLeastOne : Bool)
  /-- `pure` — contributes nothing to help. -/
  | none
deriving Repr, Inhabited

namespace Doc

/-! ### Reading the skeleton

Every function here is total and structural. `Doc` recurses through `List Doc`,
so each is written as a mutual pair with its list form rather than via `map`,
which is what lets Lean see the recursion. -/

mutual

/-- Every item mentioned anywhere in the document, in left-to-right order. -/
def items : Doc → List ItemSpec
  | .item i => [i]
  | .seq ds => itemsList ds
  | .alt ds => itemsList ds
  | .many d _ => items d
  | .none => []

/-- `items` over a list of documents. -/
def itemsList : List Doc → List ItemSpec
  | [] => []
  | d :: rest => items d ++ itemsList rest

end

/-! ### Normalization -/

mutual

/-- Flatten nested `seq`/`alt` nodes and drop the `none`s that `pure` leaves
behind, collapsing singletons. -/
def normalize : Doc → Doc
  | .item i => .item i
  | .none => .none
  | .many d atLeastOne =>
      match normalize d with
      | .none => .none
      | d' => .many d' atLeastOne
  | .seq ds =>
      match flattenSeq ds with
      | [] => .none
      | [d] => d
      | ds' => .seq ds'
  | .alt ds =>
      match flattenAlt ds with
      | [] => .none
      | [d] => d
      | ds' => .alt ds'

/-- Normalize each child of a `seq`, splicing nested `seq`s and dropping `none`s. -/
def flattenSeq : List Doc → List Doc
  | [] => []
  | d :: rest =>
      match normalize d with
      | .none => flattenSeq rest
      | .seq inner => inner ++ flattenSeq rest
      | d' => d' :: flattenSeq rest

/-- Normalize each child of an `alt`, splicing nested `alt`s.

`none` is *kept* here: `alt [d, none]` is how an optional item is spelled, and
dropping it would render a `[--flag]` as a required `--flag`. -/
def flattenAlt : List Doc → List Doc
  | [] => []
  | d :: rest =>
      match normalize d with
      | .alt inner => inner ++ flattenAlt rest
      | d' => d' :: flattenAlt rest

end

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
  | .many d _ => Normal d ∧ d.isNone = false
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

/-! ### `normalize` lands in `Normal`

Everything recurses through `List Doc`, so each proof comes as a mutual group
with its list form, exactly as `items` does. -/

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
    (ha : NormalSeq a) (hb : NormalSeq b) : NormalSeq (a ++ b) := by
  induction a with
  | nil => simpa using hb
  | cons _ _ ih =>
      obtain ⟨h1, h2, h3, h4⟩ := ha
      exact ⟨h1, h2, h3, ih h4⟩

/-- `NormalAlt` is closed under append, which is what the nested-`alt` splice
needs. -/
theorem normalAlt_append {a b : List Doc}
    (ha : NormalAlt a) (hb : NormalAlt b) : NormalAlt (a ++ b) := by
  induction a with
  | nil => simpa using hb
  | cons _ _ ih =>
      obtain ⟨h1, h2, h3⟩ := ha
      exact ⟨h1, h2, ih h3⟩

mutual

/-- Every output of `normalize` is in normal form. -/
theorem normalize_normal (d : Doc) : Normal d.normalize := by
  match d with
  | .item _ => trivial
  | .none => trivial
  | .many d _ =>
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
    NormalSeq (Doc.flattenSeq ds) := by
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
    NormalAlt (Doc.flattenAlt ds) := by
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
theorem normalize_eq_self (d : Doc) (h : Normal d) : d.normalize = d := by
  match d with
  | .item _ => rfl
  | .none => rfl
  | .many d _ =>
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
theorem flattenSeq_eq_self (ds : List Doc) (h : NormalSeq ds) :
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
theorem flattenAlt_eq_self (ds : List Doc) (h : NormalAlt ds) :
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
`seq` associative, which is what the applicative law needs. -/

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

/-- Normalizing a `seq` and then flattening it recovers the flattened children.
This is what lets a `seq` be re-nested freely: whatever bracketing the children
arrive in, `flattenSeq` sees through it to the same list. -/
theorem flattenSeq_normalize_seq (ds : List Doc) :
    Doc.flattenSeq [(Doc.seq ds).normalize] = Doc.flattenSeq ds := by
  have hns := flattenSeq_normalSeq ds
  simp only [Doc.normalize]
  rcases hl : Doc.flattenSeq ds with _ | ⟨a, _ | ⟨b, t⟩⟩
  · exact flattenSeq_none
  · rw [hl] at hns
    exact flattenSeq_eq_self [a] hns
  · rw [hl] at hns
    rw [flattenSeq_seq]
    exact flattenSeq_eq_self _ hns

/-! ### The normalized subtype

`P` stores one of these rather than a bare `Doc`. The proof field is what turns
"every description Layer 3 builds is normal" from a convention into a fact the
type checker enforces, and it is what makes the applicative laws hold on the
nose: `Normal` is a `Prop`, so two normalized documents are equal exactly when
their documents are, and normalization is what makes those agree. -/

/-- A document in the shape `normalize` produces, carrying the proof. -/
structure Normalized where
  /-- The document itself. -/
  val : Doc
  /-- Evidence that it is already normalized. -/
  normal : Normal val

namespace Normalized

/-- Two normalized documents agree exactly when their documents do: the proof
field is a `Prop` and cannot tell them apart. -/
@[ext] theorem ext {a b : Normalized} (h : a.val = b.val) : a = b := by
  cases a; cases b; subst h; rfl

/-- Normalize a document, and remember that the result is normal. This is the
only way to build one from an arbitrary `Doc`. -/
def of (d : Doc) : Normalized := ⟨d.normalize, normalize_normal d⟩

@[simp] theorem val_of (d : Doc) : (of d).val = d.normalize := rfl

/-- Normalizing an already-normalized document changes nothing. -/
@[simp] theorem of_val (n : Normalized) : of n.val = n :=
  ext (normalize_eq_self n.val n.normal)

/-- The items this document mentions, in order. -/
def items (n : Normalized) : List ItemSpec := Doc.items n.val

/-! #### The shapes Layer 3 builds

Every builder produces one of these four, each already normal — so the proof
obligation at a construction site is discharged once, here, rather than at each
call. -/

/-- Documents nothing: what `pure` and `failure` contribute. -/
def empty : Normalized := ⟨.none, trivial⟩

/-- One required item. -/
def item (i : ItemSpec) : Normalized := ⟨.item i, trivial⟩

/-- One item that may be absent, spelled as the alternation renderers print as
`[…]`. -/
def optionalItem (i : ItemSpec) : Normalized :=
  ⟨.alt [.item i, .none], ⟨by simp, trivial, rfl, trivial, rfl, trivial⟩⟩

/-- One item that may repeat, `atLeastOne` saying whether it must appear. -/
def repeatedItem (i : ItemSpec) (atLeastOne : Bool) : Normalized :=
  ⟨.many (.item i) atLeastOne, ⟨trivial, rfl⟩⟩

/-! #### Composition

`seq` and `alt` normalize their result, which is what keeps the invariant and
what makes the laws below true rather than true-up-to-rewriting. -/

/-- Both documents participate. -/
def seq (a b : Normalized) : Normalized := of (.seq [a.val, b.val])

/-- One of the two documents participates. -/
def alt (a b : Normalized) : Normalized := of (.alt [a.val, b.val])

/-- The document repeats, `atLeastOne` saying whether it must appear once. -/
def repeated (n : Normalized) (atLeastOne : Bool) : Normalized :=
  of (.many n.val atLeastOne)

/-! #### The monoid laws

`seq` is a monoid with `empty` as its unit — which is exactly what
`LawfulApplicative P` needs from the description half, and the reason `P` can
have that instance at all. Both proofs go through `flattenSeq`: normalizing a
`seq` reads its children only through it, and it is a homomorphism into
`List Doc` under `++`. -/

/-- `empty` is a left unit for `seq`. -/
@[simp] theorem empty_seq (n : Normalized) : seq empty n = n := by
  refine ext ?_
  show (Doc.seq [Doc.none, n.val]).normalize = n.val
  rw [normalize_seq_congr
        (b := [n.val])
        (by rw [show ([Doc.none, n.val] : List Doc) = [Doc.none] ++ [n.val] from rfl,
              flattenSeq_append, flattenSeq_none, List.nil_append]),
      normalize_seq_singleton, normalize_eq_self n.val n.normal]

/-- `empty` is a right unit for `seq`. -/
@[simp] theorem seq_empty (n : Normalized) : seq n empty = n := by
  refine ext ?_
  show (Doc.seq [n.val, Doc.none]).normalize = n.val
  rw [normalize_seq_congr
        (b := [n.val])
        (by rw [show ([n.val, Doc.none] : List Doc) = [n.val] ++ [Doc.none] from rfl,
              flattenSeq_append, flattenSeq_none, List.append_nil]),
      normalize_seq_singleton, normalize_eq_self n.val n.normal]

/-- `seq` is associative. The two nestings flatten to the same list of children,
and normalizing a `seq` sees its children only through that list. -/
theorem seq_assoc (a b c : Normalized) : seq a (seq b c) = seq (seq a b) c := by
  refine ext ?_
  show (Doc.seq [a.val, (Doc.seq [b.val, c.val]).normalize]).normalize
      = (Doc.seq [(Doc.seq [a.val, b.val]).normalize, c.val]).normalize
  refine normalize_seq_congr ?_
  rw [show ([a.val, (Doc.seq [b.val, c.val]).normalize] : List Doc)
        = [a.val] ++ [(Doc.seq [b.val, c.val]).normalize] from rfl,
      show ([(Doc.seq [a.val, b.val]).normalize, c.val] : List Doc)
        = [(Doc.seq [a.val, b.val]).normalize] ++ [c.val] from rfl,
      flattenSeq_append, flattenSeq_append,
      flattenSeq_normalize_seq, flattenSeq_normalize_seq,
      show ([b.val, c.val] : List Doc) = [b.val] ++ [c.val] from rfl,
      show ([a.val, b.val] : List Doc) = [a.val] ++ [b.val] from rfl,
      flattenSeq_append, flattenSeq_append, List.append_assoc]

end Normalized

end Doc

end ArgParse

import ArgParse.P

/-!
# ArgParse.Proofs.Doc

`Doc.normalize` is described in `ArgParse.Doc` as a rendering-quality pass that
"never touches parsing". Half of that is free: `run` is not in scope where
`normalize` is defined, so it cannot change how anything parses. The other half
— that it does not change what is *documented* — is what this file proves.

That half is the one worth proving, because it is where a bug would hide in
silence. If `flattenSeq` dropped a child, help would quietly stop mentioning an
option and nothing would fail. `items_normalize` rules that out: normalization
preserves the item list exactly, order included.

It matters more now than it used to. `P` normalizes at every composition, so
this is what stands between a parser gaining an option and its help mentioning
it — `items_seq` says composing two parsers documents exactly the two item
lists, concatenated, and that is a corollary of `items_normalize` rather than
something visible in the definition of `P.seq`.

The other half of the normalization story — that `normalize` lands in `Normal`
and that `Normal` documents are fixed points — lives in `ArgParse.Doc`, because
`P` cannot be defined without it.

Everything recurses through `List Doc`, so each proof comes as a mutual group
with its list form, exactly as `items` does.
-/

namespace ArgParse

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
  | .many d _ =>
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

/-! ### Composition documents everything it composes

`Doc.Normalized.of` runs `normalize`, so reading the items back off a composed
description is only faithful because `items_normalize` says normalization is
item-preserving. These three are the statements Layer 4 and 5 actually rely on:
whatever the constructors do to the shape of the tree, no item goes missing. -/

/-- Normalizing into the subtype preserves the items. -/
theorem items_of (d : Doc) : (Doc.Normalized.of d).items = Doc.items d :=
  items_normalize d

/-- Sequencing two descriptions documents both, in order. -/
theorem items_seq (a b : Doc.Normalized) :
    (a.seq b).items = a.items ++ b.items := by
  rw [Doc.Normalized.seq, items_of]
  simp [Doc.items, Doc.itemsList, Doc.Normalized.items]

/-- Alternating two descriptions documents both: help shows every branch. -/
theorem items_alt (a b : Doc.Normalized) :
    (a.alt b).items = a.items ++ b.items := by
  rw [Doc.Normalized.alt, items_of]
  simp [Doc.items, Doc.itemsList, Doc.Normalized.items]

/-- Repeating a description documents what it repeats, however many times it
must appear. -/
theorem items_repeated (a : Doc.Normalized) (atLeastOne : Bool) :
    (a.repeated atLeastOne).items = a.items := by
  rw [Doc.Normalized.repeated, items_of]
  simp [Doc.items, Doc.Normalized.items]

/-- The same statement one level up: composing two parsers documents the items
of both, so help gains an entry exactly when the parser gains an item. -/
theorem items_seq_p {α β : Type} (pf : P (α → β)) (pa : P α) :
    (Seq.seq pf (fun _ => pa)).items = pf.items ++ pa.items :=
  items_seq pf.doc pa.doc

/-- Alternation at the parser level documents both branches. -/
theorem items_orElse_p {α : Type} (pa pb : P α) :
    (OrElse.orElse pa (fun _ => pb)).items = pa.items ++ pb.items :=
  items_alt pa.doc pb.doc

/-- Repetition at the parser level documents what it repeats. -/
theorem items_many_p {α : Type} (p : P α) : (P.many p).items = p.items :=
  items_repeated p.doc false

/-- One-or-more repetition documents the same items as zero-or-more. -/
theorem items_some_p {α : Type} (p : P α) : (P.some p).items = p.items :=
  items_repeated p.doc true

end Proofs

end ArgParse

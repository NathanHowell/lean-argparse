import ArgParse.Core.Types

/-!
# ArgParse.Core.Normalize

Normalization pass that splits tokens on the first `--` sentinel while
recording enough metadata to support downstream proofs.
-/

namespace ArgParse.Core

open ArgParse

/-- Result of splitting argv tokens around the first `--` sentinel. -/
structure SentinelSplit where
  /-- Tokens that appear before the sentinel (or all tokens if no sentinel). -/
  pre : List String
  /-- Tokens that appear strictly after the sentinel. -/
  post : List String
  /-- `true` when a sentinel was encountered during the split. -/
  saw : Bool
deriving Repr, DecidableEq

namespace SentinelSplit

/-- Flatten the split back into the original token stream. -/
def flatten (split : SentinelSplit) : List String :=
  split.pre ++ (if split.saw then ["--"] else []) ++ split.post

end SentinelSplit

/-- Helper that performs the sentinel-aware split while producing metadata. -/
def split : Tokens → SentinelSplit
  | [] => { pre := [], post := [], saw := false }
  | token :: rest =>
      if token = "--" then
        { pre := [], post := rest, saw := true }
      else
        let tail := split rest
        { pre := token :: tail.pre, post := tail.post, saw := tail.saw }

@[simp] theorem split_nil : split ([] : Tokens) = { pre := [], post := [], saw := false } := rfl

@[simp] theorem split_cons_sentinel (rest : Tokens) :
    split ("--" :: rest) = { pre := [], post := rest, saw := true } := by
  simp [split]

@[simp] theorem split_cons_token {token : String} {rest : Tokens}
    (h : token ≠ "--") :
    split (token :: rest) =
      { pre := token :: (split rest).pre
        , post := (split rest).post
        , saw := (split rest).saw } := by
  simp [split, h]

/-- Reconstruction lemma: flattening the split recovers the original input. -/
theorem split_flatten (tokens : Tokens) :
    (split tokens).flatten = tokens := by
  induction tokens with
  | nil =>
      simp [SentinelSplit.flatten]
  | cons token rest ih =>
      by_cases h : token = "--"
      · subst h
        simp [SentinelSplit.flatten, split]
      ·
        have restEq : (split rest).pre ++ (if (split rest).saw then ["--"] else []) ++ (split rest).post = rest := by
          simpa [SentinelSplit.flatten] using ih
        simp [SentinelSplit.flatten, split, h, List.cons_append, restEq]

/-- If the sentinel was not seen, the post list is empty. -/
theorem split_post_nil_of_not_saw {tokens : Tokens}
    (h : (split tokens).saw = false) : (split tokens).post = [] := by
  induction tokens with
  | nil => rfl
  | cons token rest ih =>
      by_cases htoken : token = "--"
      · subst htoken
        simp [split] at h
      ·
        have hrest : (split rest).saw = false := by
          simpa [split, htoken] using h
        have hpost := ih hrest
        simpa [split, htoken] using hpost

/-- The split saw a sentinel exactly when `"--"` appears in the input tokens. -/
theorem split_saw_iff_mem (tokens : Tokens) :
    (split tokens).saw = true ↔ "--" ∈ tokens := by
  induction tokens with
  | nil => simp [split]
  | cons token rest ih =>
      by_cases htoken : token = "--"
      · subst htoken
        simp [split]
      ·
        have hSaw : (split (token :: rest)).saw = true ↔ (split rest).saw = true := by
          simp [split, htoken]
        have hMem : "--" ∈ token :: rest ↔ "--" ∈ rest := by
          constructor
          · intro hmem
            rcases List.mem_cons.1 hmem with hEq | hTail
            · exact False.elim (htoken (by simpa using hEq.symm))
            · exact hTail
          · intro hTail
            exact List.mem_cons.2 (Or.inr hTail)
        exact (hSaw.trans ih).trans hMem.symm

/-- If the sentinel was seen, the original stream factors as `pre ++ "--" :: post`. -/
theorem split_saw_iff_factor {tokens : Tokens} :
    (split tokens).saw = true ↔
      tokens = (split tokens).pre ++ "--" :: (split tokens).post := by
  constructor
  · intro hsaw
    have flattenEq : tokens = (split tokens).pre ++ (if (split tokens).saw then ["--"] else []) ++ (split tokens).post := by
      simpa [SentinelSplit.flatten] using (split_flatten tokens).symm
    simpa [hsaw] using flattenEq
  · intro h
    have memSentinel : "--" ∈ tokens := by
      have : "--" ∈ (split tokens).pre ++ "--" :: (split tokens).post := by
        simp
      exact h ▸ this
    exact (split_saw_iff_mem tokens).mpr memSentinel

/-- Build the initial parser state from raw argv tokens. -/
def normalize (tokens : Tokens) : State :=
  let s := split tokens
  { pre := s.pre, post := s.post, cursor := 0 }

@[simp] theorem normalize_pre (tokens : Tokens) :
    (normalize tokens).pre = (split tokens).pre := rfl

@[simp] theorem normalize_post (tokens : Tokens) :
    (normalize tokens).post = (split tokens).post := rfl

@[simp] theorem normalize_cursor (tokens : Tokens) :
    (normalize tokens).cursor = 0 := rfl

/-- The normalized state reconstructs the original tokens up to the sentinel. -/
theorem normalize_reassemble (tokens : Tokens) :
    ∃ saw : Bool,
      (normalize tokens).pre ++ (if saw then ["--"] else []) ++ (normalize tokens).post = tokens := by
  refine ⟨(split tokens).saw, ?_⟩
  simpa [SentinelSplit.flatten, normalize] using split_flatten tokens

/-- In the absence of a sentinel, the `post` portion is empty. -/
theorem normalize_post_nil_of_no_sentinel {tokens : Tokens}
    (h : (split tokens).saw = false) : (normalize tokens).post = [] := by
  simpa [normalize] using split_post_nil_of_not_saw (tokens := tokens) h

/-- If a sentinel is present, the normalized state reflects it as the split point. -/
theorem normalize_sentinel_factor {tokens : Tokens}
    (h : "--" ∈ tokens) :
    tokens = (normalize tokens).pre ++ "--" :: (normalize tokens).post := by
  have hs : (split tokens).saw = true := (split_saw_iff_mem tokens).mpr h
  have := (split_saw_iff_factor (tokens := tokens)).mp hs
  simpa [normalize] using this

/-- Without a sentinel, the `post` portion is empty. -/
theorem normalize_post_nil_of_not_mem {tokens : Tokens}
    (h : "--" ∉ tokens) : (normalize tokens).post = [] := by
  classical
  have hs : (split tokens).saw = false := by
    cases hSaw : (split tokens).saw with
    | false => rfl
    | true =>
        have : "--" ∈ tokens := (split_saw_iff_mem tokens).mp hSaw
        exact False.elim (h this)
  exact normalize_post_nil_of_no_sentinel (tokens := tokens) hs

end ArgParse.Core

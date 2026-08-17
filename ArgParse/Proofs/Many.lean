import ArgParse.Builder

/-!
# ArgParse.Proofs.Many

`P.many` is fuel-bounded, and the bound is `State.budget`. This file shows the
bound is slack: for a parser that makes progress, running the loop with any fuel
at or above `st.budget` gives the same answer, so the repetition always ends
because the parser failed or stalled — never because it ran out.

That statement is worth having because the previous bound did *not* have it.
Counting tokens is too coarse: matching a short flag out of a bundle rewrites
`-vvv` to `-vv`, advancing the cursor without shortening the stream, and
`many (flag …)` silently returned two occurrences instead of three. Running out
of fuel is indistinguishable from the parser declining, so nothing failed. The
budget now charges per character as well as per token, and
`flagScan_progresses` is the lemma that says a bundle step really does spend it.

`Progresses` is a hypothesis rather than a fact about all parsers, because it is
not one: `Parser` is an arbitrary `State → Result α`, and `many` is exposed to
user-written parsers. What is proved here is that every parser the library
builds satisfies it.
-/

namespace ArgParse.Proofs

open ArgParse ArgParse.Core ArgParse.Spec

/-! ### Budget arithmetic -/

@[simp] theorem tokensBudget_nil : tokensBudget [] = 0 := rfl

@[simp] theorem tokensBudget_cons (t : String) (l : List String) :
    tokensBudget (t :: l) = t.length + 1 + tokensBudget l := rfl

/-- Dropping a prefix shortens a string by the amount dropped. -/
theorem drop_toString_length (t : String) (n : Nat) :
    (t.drop n).toString.length = t.length - n := by
  have h : (t.drop n).toString.toList = t.toList.drop n := by simp
  rw [← String.length_toList, h, List.length_drop, String.length_toList]

/-- A short lexeme is a dash and a character. -/
theorem shortLexeme_length (short : Spec.Short) : (shortLexeme short).length = 2 := by
  rw [← String.length_toList]
  simp [shortLexeme]

/-- Re-dashing a bundle tail costs one character. -/
theorem length_dash_append (t : String) : ("-" ++ t).length = t.length + 1 := by
  rw [← String.length_toList, ← String.length_toList]
  simp

/-! ### A bundle step spends budget

The rewritten token `-xy` is one character shorter than the `-vxy` it came from.
That is the whole reason the budget counts characters, so it is worth proving
rather than assuming. -/

/-- The tail left by a bundled match is two characters shorter than the token,
having lost the dash and the matched character. -/
theorem short_bundled_length {short : Spec.Short} {token tail : String}
    (hpre : token.startsWith (shortLexeme short) = true)
    (heq : tail = (token.drop (shortLexeme short).length).toString) :
    tail.length + 2 ≤ token.length := by
  have hlen := shortLexeme_length short
  have hp : (shortLexeme short).toList <+: token.toList := by
    simpa [String.startsWith_string_iff] using hpre
  have hle : 2 ≤ token.length := by
    have hl := hp.length_le
    rw [String.length_toList, String.length_toList, hlen] at hl
    exact hl
  subst heq
  rw [hlen, drop_toString_length]
  omega

/-- The short-form half of `matchFlagToken`, stated on its own so the long-form
and no-long-form branches can share it. -/
theorem short_branch_bundled {short : Spec.Short} {token tail : String}
    (h : (if token = shortLexeme short then FlagMatch.short
          else if token.startsWith (shortLexeme short) then
            (if ((token.drop (shortLexeme short).length).toString).isEmpty then
              FlagMatch.short
             else if token.startsWith "--" then FlagMatch.none
             else FlagMatch.shortBundled ((token.drop (shortLexeme short).length).toString))
          else FlagMatch.none) = FlagMatch.shortBundled tail) :
    tail.length + 2 ≤ token.length := by
  by_cases h1 : token = shortLexeme short
  · rw [if_pos h1] at h; exact absurd h (by simp)
  · rw [if_neg h1] at h
    by_cases h2 : token.startsWith (shortLexeme short) = true
    · rw [if_pos h2] at h
      by_cases h3 : ((token.drop (shortLexeme short).length).toString).isEmpty = true
      · rw [if_pos h3] at h; exact absurd h (by simp)
      · rw [if_neg h3] at h
        by_cases h4 : token.startsWith "--" = true
        · rw [if_pos h4] at h; exact absurd h (by simp)
        · rw [if_neg h4] at h
          simp only [FlagMatch.shortBundled.injEq] at h
          exact short_bundled_length h2 h.symm
    · rw [if_neg h2] at h; exact absurd h (by simp)

/-- A bundled match leaves a strictly shorter token. -/
theorem matchFlagToken_shortBundled_length {spec : FlagSpec} {token tail : String}
    (h : matchFlagToken spec token = .shortBundled tail) :
    tail.length + 2 ≤ token.length := by
  unfold matchFlagToken at h
  cases hL : spec.long? with
  | none =>
      simp only [hL] at h
      cases hS : spec.short? with
      | none => simp only [hS] at h; exact absurd h (by simp)
      | some short => simp only [hS] at h; exact short_branch_bundled h
  | some name =>
      simp only [hL] at h
      by_cases h1 : token = longLexeme name
      · rw [if_pos h1] at h; exact absurd h (by simp)
      · rw [if_neg h1] at h
        cases hS : spec.short? with
        | none => simp only [hS] at h; exact absurd h (by simp)
        | some short => simp only [hS] at h; exact short_branch_bundled h

/-- **Every flag match spends budget**, whether it removes a token or only
rewrites one. This is the fact the token-count bound was missing. -/
theorem tokensBudget_scanFlagPre {spec : FlagSpec} :
    ∀ {pre pre' : List String}, scanFlagPre spec pre = some pre' →
      tokensBudget pre' < tokensBudget pre := by
  intro pre
  induction pre with
  | nil => intro pre' h; simp [scanFlagPre] at h
  | cons token rest ih =>
      intro pre' h
      simp only [scanFlagPre] at h
      cases hm : matchFlagToken spec token with
      | none =>
          rw [hm] at h
          cases hr : scanFlagPre spec rest with
          | none => rw [hr] at h; simp at h
          | some p2 =>
              rw [hr] at h
              simp only [Option.map_some, Option.some.injEq] at h
              subst h
              have := ih hr
              simp only [tokensBudget_cons]
              omega
      | short =>
          rw [hm] at h
          simp only [Option.some.injEq] at h
          subst h
          simp only [tokensBudget_cons]
          omega
      | long =>
          rw [hm] at h
          simp only [Option.some.injEq] at h
          subst h
          simp only [tokensBudget_cons]
          omega
      | shortBundled tail =>
          rw [hm] at h
          simp only [Option.some.injEq] at h
          subst h
          have hlen := matchFlagToken_shortBundled_length hm
          simp only [tokensBudget_cons, length_dash_append]
          omega

/-! ### Progress -/

/-- A parser makes progress when advancing the cursor also spends budget.

Not every `Parser` does — the type is an arbitrary state function — so this is a
hypothesis on the theorems below and a fact about the library's builders. -/
def Progresses {α : Type} (p : Parser α) : Prop :=
  ∀ st a st', p st = .ok a st' → st.cursor < st'.cursor → st'.budget < st.budget

/-- Flag scanning progresses. -/
theorem flagScan_progresses (spec : FlagSpec) : Progresses (flagScan spec) := by
  intro st a st' hrun hlt
  simp only [flagScan] at hrun
  cases hscan : scanFlagPre spec st.pre with
  | none =>
      rw [hscan] at hrun
      simp only [Result.ok.injEq] at hrun
      obtain ⟨-, rfl⟩ := hrun
      omega
  | some pre' =>
      rw [hscan] at hrun
      simp only [Result.ok.injEq] at hrun
      obtain ⟨-, rfl⟩ := hrun
      have := tokensBudget_scanFlagPre hscan
      simp only [State.budget, State.withPre]
      omega

/-- The flag builder progresses, so `many` over it is bounded correctly. -/
theorem flag_progresses (long : String) (short : Option Char) (help : String)
    (hidden : Bool) : Progresses (Builder.flag long short help hidden).run :=
  flagScan_progresses _

/-! ### The bound is slack

For a progressing parser the loop reaches its own stopping condition before the
fuel runs out, so the fuel is not part of the semantics. -/

/-- One more unit of fuel changes nothing once there is enough. -/
theorem go_succ {α : Type} {p : Parser α} (hp : Progresses p) :
    ∀ (fuel : Nat) (acc : List α) (st : State), st.budget ≤ fuel →
      P.runMany.go p fuel acc st = P.runMany.go p (fuel + 1) acc st := by
  intro fuel
  induction fuel with
  | zero =>
      intro acc st hb
      simp only [P.runMany.go]
      cases hstep : p st with
      | err e => simp
      | ok a st' =>
          dsimp only
          rw [if_neg]
          intro hlt
          have := hp st a st' hstep hlt
          omega
  | succ f ih =>
      intro acc st hb
      simp only [P.runMany.go]
      cases hstep : p st with
      | err e => simp
      | ok a st' =>
          dsimp only
          by_cases hlt : st.cursor < st'.cursor
          · rw [if_pos hlt, if_pos hlt]
            have hbud := hp st a st' hstep hlt
            exact ih (a :: acc) st' (by omega)
          · rw [if_neg hlt, if_neg hlt]

/-- Any amount of extra fuel changes nothing once there is enough. -/
theorem go_stable {α : Type} {p : Parser α} (hp : Progresses p) :
    ∀ (extra fuel : Nat) (acc : List α) (st : State), st.budget ≤ fuel →
      P.runMany.go p fuel acc st = P.runMany.go p (fuel + extra) acc st := by
  intro extra
  induction extra with
  | zero => intro fuel acc st _; rfl
  | succ k ih =>
      intro fuel acc st hb
      rw [ih fuel acc st hb, ← Nat.add_assoc]
      exact go_succ hp (fuel + k) acc st (by omega)

/-- **The repetition bound is not part of the semantics.** Running the loop with
any fuel at or above `State.budget` gives what `many` gives, so `many` always
stops because the parser failed or stalled. -/
theorem runMany_eq_go {α : Type} {p : Parser α} (hp : Progresses p)
    (st : State) (fuel : Nat) (h : st.budget ≤ fuel) :
    P.runMany p st = P.runMany.go p fuel [] st := by
  have hbase : ∀ n, st.budget ≤ n →
      P.runMany.go p st.budget [] st = P.runMany.go p n [] st := by
    intro n hn
    have := go_stable hp (n - st.budget) st.budget [] st (Nat.le_refl _)
    rwa [Nat.add_sub_cancel' hn] at this
  show P.runMany.go p (st.budget + 1) [] st = P.runMany.go p fuel [] st
  rw [← hbase (st.budget + 1) (by omega), hbase fuel h]

/-- The flag instance, spelled out: `many (flag …)` is bounded, not truncated. -/
theorem runMany_flag_eq_go (long : String) (short : Option Char) (help : String)
    (hidden : Bool) (st : State) (fuel : Nat) (h : st.budget ≤ fuel) :
    P.runMany (Builder.flag long short help hidden).run st
      = P.runMany.go (Builder.flag long short help hidden).run fuel [] st :=
  runMany_eq_go (flag_progresses long short help hidden) st fuel h

end ArgParse.Proofs

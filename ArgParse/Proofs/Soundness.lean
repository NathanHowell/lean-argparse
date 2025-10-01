import ArgParse.Core.Parser
import ArgParse.Core.Runner
import ArgParse.Spec.Elab

/-!
# ArgParse.Proofs.Soundness

Early payload soundness lemmas for the `Spec.Partial` accumulator.
-/

namespace ArgParse.Proofs

open ArgParse

open ArgParse.Spec

namespace Partial

open Classical
open ArgParse.Spec.Partial

/-- Auxiliary simp lemma describing the updated `flags` list. -/
@[simp] theorem addFlag_flags (p : Spec.Partial) (name : String) (value : Bool) :
    (p.addFlag name value).flags = p.flags ++ [(name, value)] := rfl

/-- Recording a flag leaves other bindings unchanged. -/
@[simp] theorem addFlag_options (p : Spec.Partial) (name : String) (value : Bool) :
    (p.addFlag name value).options = p.options := rfl

/-- Recording a flag leaves other bindings unchanged. -/
@[simp] theorem addFlag_positionals (p : Spec.Partial) (name : String) (value : Bool) :
    (p.addFlag name value).positionals = p.positionals := rfl

/-- Auxiliary simp lemma describing the updated `options` list. -/
@[simp] theorem addOption_options (p : Spec.Partial) (name : String) (value : String) :
    (p.addOption name value).options = p.options ++ [(name, value)] := rfl

/-- Recording an option leaves other bindings unchanged. -/
@[simp] theorem addOption_flags (p : Spec.Partial) (name : String) (value : String) :
    (p.addOption name value).flags = p.flags := rfl

/-- Recording an option leaves other bindings unchanged. -/
@[simp] theorem addOption_positionals (p : Spec.Partial) (name : String) (value : String) :
    (p.addOption name value).positionals = p.positionals := rfl

/-- Auxiliary simp lemma describing the updated `positionals` list. -/
@[simp] theorem addPositional_positionals
    (p : Spec.Partial) (name : String) (value : String) :
    (p.addPositional name value).positionals = p.positionals ++ [(name, value)] := rfl

/-- Recording a positional leaves other bindings unchanged. -/
@[simp] theorem addPositional_flags (p : Spec.Partial) (name : String) (value : String) :
    (p.addPositional name value).flags = p.flags := rfl

/-- Recording a positional leaves other bindings unchanged. -/
@[simp] theorem addPositional_options (p : Spec.Partial) (name : String) (value : String) :
    (p.addPositional name value).options = p.options := rfl

/-- Folding `addFlag` appends the new bindings to the end of `flags`. -/
@[simp] theorem foldl_addFlag_flags
    (entries : List (String × Bool)) (p : Spec.Partial) :
    (entries.foldl (fun acc entry => acc.addFlag entry.fst entry.snd) p).flags =
      p.flags ++ entries := by
  induction entries generalizing p with
  | nil => simp
  | cons entry rest ih =>
      rcases entry with ⟨flagName, flagValue⟩
      simp [List.foldl_cons, ih, List.append_assoc]

/-- Folding `addOption` appends the new bindings to the end of `options`. -/
@[simp] theorem foldl_addOption_options
    (entries : List (String × String)) (p : Spec.Partial) :
    (entries.foldl (fun acc entry => acc.addOption entry.fst entry.snd) p).options =
      p.options ++ entries := by
  induction entries generalizing p with
  | nil => simp
  | cons entry rest ih =>
      rcases entry with ⟨optName, optValue⟩
      simp [List.foldl_cons, ih, List.append_assoc]

/-- Folding `addPositional` appends the new bindings to the end of `positionals`. -/
@[simp] theorem foldl_addPositional_positionals
    (entries : List (String × String)) (p : Spec.Partial) :
    (entries.foldl (fun acc entry => acc.addPositional entry.fst entry.snd) p).positionals =
      p.positionals ++ entries := by
  induction entries generalizing p with
  | nil => simp
  | cons entry rest ih =>
      rcases entry with ⟨posName, posValue⟩
      simp [List.foldl_cons, ih, List.append_assoc]

/-- Folding `addFlag` mirrors the last-write-wins semantics of `Summary.flagValue?`. -/
@[simp] theorem flagValue?_fold_addFlag
    (entries : List (String × Bool)) (p : Spec.Partial) (name : String) :
    Spec.Partial.Summary.flagValue?
        ((entries.foldl (fun acc entry => acc.addFlag entry.fst entry.snd) p).toSummary) name =
      entries.foldl
        (fun latest entry =>
          if entry.fst = name then some entry.snd else latest)
        (Spec.Partial.Summary.flagValue? (Spec.Partial.toSummary p) name) := by
  classical
  simp [Spec.Partial.Summary.flagValue?, Spec.Partial.toSummary,
        foldl_addFlag_flags, List.foldl_append]

/-- Folding `addOption` accumulates option payloads in chronological order. -/
@[simp] theorem optionValues_fold_addOption
    (entries : List (String × String)) (p : Spec.Partial) (name : String) :
    Spec.Partial.Summary.optionValues
        ((entries.foldl (fun acc entry => acc.addOption entry.fst entry.snd) p).toSummary) name =
      Spec.Partial.Summary.optionValues (Spec.Partial.toSummary p) name ++
        entries.filterMap (fun entry =>
          if entry.fst = name then some entry.snd else none) := by
  classical
  simp [Spec.Partial.Summary.optionValues, Spec.Partial.toSummary,
        foldl_addOption_options,
        List.filterMap_append]

/-- Folding `addPositional` accumulates positional payloads in chronological order. -/
@[simp] theorem positionalValues_fold_addPositional
    (entries : List (String × String)) (p : Spec.Partial) (name : String) :
    Spec.Partial.Summary.positionalValues
        ((entries.foldl (fun acc entry => acc.addPositional entry.fst entry.snd) p).toSummary) name =
      Spec.Partial.Summary.positionalValues (Spec.Partial.toSummary p) name ++
        entries.filterMap (fun entry =>
          if entry.fst = name then some entry.snd else none) := by
  classical
  simp [Spec.Partial.Summary.positionalValues, Spec.Partial.toSummary,
        foldl_addPositional_positionals, List.filterMap_append]

@[simp] private def flagStep (name : String) : Option Bool → (String × Bool) → Option Bool :=
  fun latest entry => if entry.fst = name then some entry.snd else latest

private theorem flagFold_from_some_ne_none
    (name : String) (entries : List (String × Bool)) (value : Bool) :
    entries.foldl (flagStep name) (some value) ≠ none := by
  classical
  induction entries generalizing value with
  | nil => simp
  | cons entry rest ih =>
      by_cases hMatch : entry.fst = name
      · have := ih entry.snd
        simp [List.foldl, hMatch, this]
      · have := ih value
        simp [List.foldl, hMatch, this]

private theorem flagFold_override
    (name : String) :
    ∀ (entries : List (String × Bool)) (init : Option Bool),
      entries.foldl (flagStep name) init =
        match entries.foldl (flagStep name) none with
        | some value => some value
        | none => init := by
  classical
  intro entries
  induction entries with
  | nil =>
      intro init
      simp
  | cons entry rest ih =>
      intro init
      by_cases hMatch : entry.fst = name
      ·
        have hNe := flagFold_from_some_ne_none name rest entry.snd
        cases hRes : rest.foldl (flagStep name) (some entry.snd) with
        | none =>
            exact (hNe (by simpa [List.foldl, hMatch] using hRes)).elim
        | some value =>
            simp [List.foldl, hMatch, hRes]
      · have := ih init
        simp [List.foldl, hMatch, this]

/-- Summary lookup over a merged payload prefers entries from the right operand. -/
@[simp] theorem flagValue?_merge (earlier later : Spec.Partial) (name : String) :
    Spec.Partial.Summary.flagValue?
        ((Spec.Partial.merge earlier later).toSummary) name =
      match Spec.Partial.Summary.flagValue? (Spec.Partial.toSummary later) name with
      | some value => some value
      | none => Spec.Partial.Summary.flagValue? (Spec.Partial.toSummary earlier) name := by
  classical
  have := flagFold_override name later.flags
    (earlier.flags.foldl (flagStep name) none)
  simpa [Spec.Partial.merge, Spec.Partial.toSummary,
    Spec.Partial.Summary.flagValue?, List.foldl_append]
    using this

/-- Option summary values concatenate when merging partial payloads. -/
@[simp] theorem optionValues_merge (earlier later : Spec.Partial) (name : String) :
    Spec.Partial.Summary.optionValues
        ((Spec.Partial.merge earlier later).toSummary) name =
      Spec.Partial.Summary.optionValues (Spec.Partial.toSummary earlier) name ++
        Spec.Partial.Summary.optionValues (Spec.Partial.toSummary later) name := by
  classical
  simp [Spec.Partial.Summary.optionValues, Spec.Partial.merge, Spec.Partial.toSummary,
        List.filterMap_append]

/-- Positional summary values concatenate when merging partial payloads. -/
@[simp] theorem positionalValues_merge (earlier later : Spec.Partial) (name : String) :
    Spec.Partial.Summary.positionalValues
        ((Spec.Partial.merge earlier later).toSummary) name =
      Spec.Partial.Summary.positionalValues (Spec.Partial.toSummary earlier) name ++
        Spec.Partial.Summary.positionalValues (Spec.Partial.toSummary later) name := by
  classical
  simp [Spec.Partial.Summary.positionalValues, Spec.Partial.merge, Spec.Partial.toSummary,
        List.filterMap_append]

/-- `Partial.empty` is a left identity for `Partial.merge`. -/
@[simp] theorem merge_empty_left (p : Spec.Partial) :
    Spec.Partial.merge Spec.Partial.empty p = p := by
  cases p with
  | mk flags options positionals =>
      simp [Spec.Partial.merge, Spec.Partial.empty]

/-- `Partial.empty` is a right identity for `Partial.merge`. -/
@[simp] theorem merge_empty_right (p : Spec.Partial) :
    Spec.Partial.merge p Spec.Partial.empty = p := by
  cases p with
  | mk flags options positionals =>
      simp [Spec.Partial.merge, Spec.Partial.empty]

/-- `Partial.merge` is associative. -/
@[simp] theorem merge_assoc (a b c : Spec.Partial) :
    Spec.Partial.merge (Spec.Partial.merge a b) c =
      Spec.Partial.merge a (Spec.Partial.merge b c) := by
  cases a with
  | mk af ao ap =>
      cases b with
      | mk bf bo bp =>
          cases c with
          | mk cf co cp =>
              simp [Spec.Partial.merge, List.append_assoc]

/-- Two partial payloads agree when each component list agrees. -/
@[ext] theorem ext
    {p q : Spec.Partial}
    (hFlags : p.flags = q.flags)
    (hOptions : p.options = q.options)
    (hPositionals : p.positionals = q.positionals) :
    p = q := by
  cases p
  cases q
  cases hFlags
  cases hOptions
  cases hPositionals
  rfl

/-- Functions that append payloads to the right via `Partial.merge`. -/
def mergesRight (f : Spec.Partial → Spec.Partial) : Prop :=
  ∀ base, f base = Spec.Partial.merge base (f Spec.Partial.empty)

/-- The identity transformer is merge-compatible. -/
theorem mergesRight_id : mergesRight (fun p => p) := by
  intro base
  symm
  simpa using (merge_empty_right (p := base))

/-- Recording a flag is merge-compatible. -/
theorem mergesRight_flag (name : String) (value : Bool) :
    mergesRight (fun p => if value then p.addFlag name true else p) := by
  intro base
  cases value with
  | false =>
      symm
      simpa [Spec.Partial.merge, Spec.Partial.empty]
  | true =>
      cases base with
      | mk flags options positionals =>
          refine ext ?hf ?ho ?hp
          · simp [Spec.Partial.merge, Spec.Partial.empty, addFlag_flags]
          · simp [Spec.Partial.merge, Spec.Partial.empty, addFlag_options]
          · simp [Spec.Partial.merge, Spec.Partial.empty, addFlag_positionals]

/-- Recording an option payload is merge-compatible. -/
theorem mergesRight_addOption (name : String) (raw : String) :
    mergesRight (fun p => p.addOption name raw) := by
  intro base
  cases base with
  | mk flags options positionals =>
      refine ext ?hf ?ho ?hp
      · simp [Spec.Partial.merge, Spec.Partial.empty, addOption_flags]
      · simp [Spec.Partial.merge, Spec.Partial.empty, addOption_options]
      · simp [Spec.Partial.merge, Spec.Partial.empty, addOption_positionals]

/-- Recording a positional payload is merge-compatible. -/
theorem mergesRight_addPositional (name : String) (raw : String) :
    mergesRight (fun p => p.addPositional name raw) := by
  intro base
  cases base with
  | mk flags options positionals =>
      refine ext ?hf ?ho ?hp
      · simp [Spec.Partial.merge, Spec.Partial.empty, addPositional_flags]
      · simp [Spec.Partial.merge, Spec.Partial.empty, addPositional_options]
      · simp [Spec.Partial.merge, Spec.Partial.empty, addPositional_positionals]

/-- Composition of merge-compatible transformers remains merge-compatible. -/
theorem mergesRight_comp
    {g h : Spec.Partial → Spec.Partial}
    (hg : mergesRight g) (hh : mergesRight h) :
    mergesRight (fun p => h (g p)) := by
  intro base
  have hgBase := hg base
  have hhBase := hh (g base)
  have hhEmpty := hh (g Spec.Partial.empty)
  calc
    h (g base)
        = Spec.Partial.merge (g base) (h Spec.Partial.empty) := by
            simpa using hhBase
    _ = Spec.Partial.merge (Spec.Partial.merge base (g Spec.Partial.empty))
          (h Spec.Partial.empty) := by
            simpa [hgBase]
    _ = Spec.Partial.merge base
          (Spec.Partial.merge (g Spec.Partial.empty) (h Spec.Partial.empty)) := by
            simpa using
              (merge_assoc base (g Spec.Partial.empty) (h Spec.Partial.empty))
    _ = Spec.Partial.merge base (h (g Spec.Partial.empty)) := by
            simpa [hhEmpty.symm]

/-- Folding a list of option payloads preserves the merge-right property. -/
theorem mergesRight_fold_addOption (name : String) :
    ∀ (raws : List String),
      mergesRight (fun p => raws.foldl (fun acc raw => acc.addOption name raw) p)
  | [] => by
      simpa using mergesRight_id
  | raw :: rest => by
      simpa [List.foldl_cons] using
        (mergesRight_comp
          (g := fun p => p.addOption name raw)
          (h := fun p => rest.foldl (fun acc raw => acc.addOption name raw) p)
          (hg := mergesRight_addOption (name := name) (raw := raw))
          (hh := mergesRight_fold_addOption (name := name) rest))

/-- Folding optional positional payloads preserves the merge-right property. -/
theorem mergesRight_option_addPositional (name : String) :
    ∀ (raw? : Option String), mergesRight
      (fun p => match raw? with
        | none => p
        | some raw => p.addPositional name raw)
  | none => by
      simpa using mergesRight_id
  | some raw => by
      simpa using mergesRight_addPositional (name := name) (raw := raw)

/-- Folding a list of positional payloads preserves the merge-right property. -/
theorem mergesRight_fold_addPositional (name : String) :
    ∀ (raws : List String),
      mergesRight
        (fun p => raws.foldl (fun acc raw => acc.addPositional name raw) p)
  | [] => by
      simpa using mergesRight_id
  | raw :: rest => by
      simpa [List.foldl_cons] using
        (mergesRight_comp
          (g := fun p => p.addPositional name raw)
          (h := fun p => rest.foldl (fun acc raw => acc.addPositional name raw) p)
          (hg := mergesRight_addPositional (name := name) (raw := raw))
          (hh := mergesRight_fold_addPositional (name := name) rest))

end Partial

open Partial

theorem elaborateItem_flag_mergesRight
    (spec : FlagSpec) (st : State)
    {f : Spec.Partial → Spec.Partial} {st' : State}
    (h : Spec.elaborateItem (.flag spec) st = Result.ok f st') :
    mergesRight f := by
  classical
  unfold Spec.elaborateItem at h
  cases hFlag : ArgParse.Core.flag spec st with
  | err _ =>
      have : False := by
        simpa [Parser.map, hFlag] using h
      exact this.elim
  | ok value st₁ =>
      have hParts := by
        simpa [Parser.map, hFlag] using h
      obtain ⟨hf, _⟩ := hParts
      simpa [hf] using mergesRight_flag (name := spec.«meta».name) (value := value)

theorem elaborateItem_opt_zero_mergesRight
    {α : Type} [ArgParse.FromArg α]
    (spec : OptSpec α) (st : State)
    {f : Spec.Partial → Spec.Partial} {st' : State}
    (hArity : spec.arity = .zero)
    (h : Spec.elaborateItem (.opt spec) st = Result.ok f st') :
    mergesRight f := by
  classical
  unfold Spec.elaborateItem at h
  simp [hArity] at h
  cases h
  simpa using mergesRight_id

theorem elaborateItem_opt_one_mergesRight
    {α : Type} [ArgParse.FromArg α]
    (spec : OptSpec α) (st : State)
    {f : Spec.Partial → Spec.Partial} {st' : State}
    (hArity : spec.arity = .one)
    (h : Spec.elaborateItem (.opt spec) st = Result.ok f st') :
    mergesRight f := by
  classical
  unfold Spec.elaborateItem at h
  simp [hArity] at h
  cases hCollect : ArgParse.Core.collectOptionValues (α := α) spec st with
  | error err =>
      have : False := by
        simpa [Parser.map, hCollect] using h
      exact this.elim
  | ok payload =>
      rcases payload with ⟨values, raws, st₁⟩
      have hParts := by
        simpa [Parser.map, hCollect] using h
      obtain ⟨hf, _⟩ := hParts
      simpa [hf] using mergesRight_fold_addOption (name := spec.«meta».name) raws

theorem elaborateItem_opt_many_mergesRight
    {α : Type} [ArgParse.FromArg α]
    (spec : OptSpec α) (st : State)
    {f : Spec.Partial → Spec.Partial} {st' : State}
    (hArity : spec.arity = .many)
    (h : Spec.elaborateItem (.opt spec) st = Result.ok f st') :
    mergesRight f := by
  classical
  unfold Spec.elaborateItem at h
  simp [hArity] at h
  cases hCollect : ArgParse.Core.collectOptionValues (α := α) spec st with
  | error err =>
      have : False := by
        simpa [Parser.map, hCollect] using h
      exact this.elim
  | ok payload =>
      rcases payload with ⟨values, raws, st₁⟩
      have hParts := by
        simpa [Parser.map, hCollect] using h
      obtain ⟨hf, _⟩ := hParts
      simpa [hf] using mergesRight_fold_addOption (name := spec.«meta».name) raws

theorem elaborateItem_opt_some_mergesRight
    {α : Type} [ArgParse.FromArg α]
    (spec : OptSpec α) (st : State)
    {f : Spec.Partial → Spec.Partial} {st' : State}
    (hArity : spec.arity = .some)
    (h : Spec.elaborateItem (.opt spec) st = Result.ok f st') :
    mergesRight f := by
  classical
  unfold Spec.elaborateItem at h
  simp [hArity] at h
  cases hCollect : ArgParse.Core.collectOptionValues (α := α) spec st with
  | error err =>
      have : False := by
        simpa [Parser.map, hCollect] using h
      exact this.elim
  | ok payload =>
      rcases payload with ⟨values, raws, st₁⟩
      cases hValues : values with
      | nil =>
          have : False := by
            simpa [Parser.map, hCollect, hValues] using h
          exact this.elim
      | cons head tail =>
          have hParts := by
            simpa [Parser.map, hCollect, hValues] using h
          obtain ⟨hf, _⟩ := hParts
          simpa [hf] using mergesRight_fold_addOption (name := spec.«meta».name) raws

theorem elaborateItem_pos_zero_mergesRight
    {α : Type} [ArgParse.FromArg α]
    (spec : PosSpec α) (st : State)
    {f : Spec.Partial → Spec.Partial} {st' : State}
    (hArity : spec.arity = .zero)
    (h : Spec.elaborateItem (.pos spec) st = Result.ok f st') :
    mergesRight f := by
  classical
  unfold Spec.elaborateItem at h
  simp [hArity] at h
  cases h
  simpa using mergesRight_id

theorem elaborateItem_pos_one_mergesRight
    {α : Type} [ArgParse.FromArg α]
    (spec : PosSpec α) (st : State)
    {f : Spec.Partial → Spec.Partial} {st' : State}
    (hArity : spec.arity = .one)
    (h : Spec.elaborateItem (.pos spec) st = Result.ok f st') :
    mergesRight f := by
  classical
  unfold Spec.elaborateItem at h
  simp [hArity] at h
  cases hTake : ArgParse.Core.takePositionalValue? (α := α) spec st with
  | error err =>
      have : False := by
        simpa [Parser.map, hTake] using h
      exact this.elim
  | ok result =>
      rcases result with ⟨ov, st₁⟩
      have hParts := by
        simpa [Parser.map, hTake] using h
      obtain ⟨hf, _⟩ := hParts
      cases ov with
      | none =>
          simpa [hf.symm] using mergesRight_id
      | some pair =>
          rcases pair with ⟨_, raw⟩
          simpa [hf.symm] using
            mergesRight_addPositional (name := spec.«meta».name) (raw := raw)

theorem elaborateItem_pos_many_mergesRight
    {α : Type} [ArgParse.FromArg α]
    (spec : PosSpec α) (st : State)
    {f : Spec.Partial → Spec.Partial} {st' : State}
    (hArity : spec.arity = .many)
    (h : Spec.elaborateItem (.pos spec) st = Result.ok f st') :
    mergesRight f := by
  classical
  unfold Spec.elaborateItem at h
  simp [hArity] at h
  cases hCollect : ArgParse.Core.collectPositionalValues (α := α) spec st with
  | error err =>
      have : False := by
        simpa [Parser.map, hCollect] using h
      exact this.elim
  | ok payload =>
      rcases payload with ⟨values, raws, st₁⟩
      have hParts := by
        simpa [Parser.map, hCollect] using h
      obtain ⟨hf, _⟩ := hParts
      simpa [hf] using mergesRight_fold_addPositional (name := spec.«meta».name) raws

theorem elaborateItem_pos_some_mergesRight
    {α : Type} [ArgParse.FromArg α]
    (spec : PosSpec α) (st : State)
    {f : Spec.Partial → Spec.Partial} {st' : State}
    (hArity : spec.arity = .some)
    (h : Spec.elaborateItem (.pos spec) st = Result.ok f st') :
    mergesRight f := by
  classical
  unfold Spec.elaborateItem at h
  simp [hArity] at h
  cases hCollect : ArgParse.Core.collectPositionalValues (α := α) spec st with
  | error err =>
      have : False := by
        simpa [Parser.map, hCollect] using h
      exact this.elim
  | ok payload =>
      rcases payload with ⟨values, raws, st₁⟩
      have hParts := by
        simpa [Parser.map, hCollect] using h
      obtain ⟨hf, _⟩ := hParts
      simpa [hf] using mergesRight_fold_addPositional (name := spec.«meta».name) raws

theorem elaborateItem_opt_mergesRight
    {α : Type} [ArgParse.FromArg α]
    (spec : OptSpec α) (st : State)
    {f : Spec.Partial → Spec.Partial} {st' : State}
    (h : Spec.elaborateItem (.opt spec) st = Result.ok f st') :
    mergesRight f := by
  classical
  cases hArity : spec.arity with
  | zero =>
      exact elaborateItem_opt_zero_mergesRight (spec := spec) (st := st) (f := f)
        (st' := st') (hArity := hArity) h
  | one =>
      exact elaborateItem_opt_one_mergesRight (spec := spec) (st := st) (f := f)
        (st' := st') (hArity := hArity) h
  | many =>
      exact elaborateItem_opt_many_mergesRight (spec := spec) (st := st) (f := f)
        (st' := st') (hArity := hArity) h
  | some =>
      exact elaborateItem_opt_some_mergesRight (spec := spec) (st := st) (f := f)
        (st' := st') (hArity := hArity) h

theorem elaborateItem_pos_mergesRight
    {α : Type} [ArgParse.FromArg α]
    (spec : PosSpec α) (st : State)
    {f : Spec.Partial → Spec.Partial} {st' : State}
    (h : Spec.elaborateItem (.pos spec) st = Result.ok f st') :
    mergesRight f := by
  classical
  cases hArity : spec.arity with
  | zero =>
      exact elaborateItem_pos_zero_mergesRight (spec := spec) (st := st) (f := f)
        (st' := st') (hArity := hArity) h
  | one =>
      exact elaborateItem_pos_one_mergesRight (spec := spec) (st := st) (f := f)
        (st' := st') (hArity := hArity) h
  | many =>
      exact elaborateItem_pos_many_mergesRight (spec := spec) (st := st) (f := f)
        (st' := st') (hArity := hArity) h
  | some =>
      exact elaborateItem_pos_some_mergesRight (spec := spec) (st := st) (f := f)
        (st' := st') (hArity := hArity) h

theorem elaborateItem_mergesRight
    (item : ItemSpec) (st : State)
    {f : Spec.Partial → Spec.Partial} {st' : State}
    (h : Spec.elaborateItem item st = Result.ok f st') :
    mergesRight f := by
  classical
  cases item with
  | flag spec =>
      exact elaborateItem_flag_mergesRight (spec := spec) (st := st) (f := f)
        (st' := st') h
  | @opt α inst spec =>
      exact elaborateItem_opt_mergesRight (spec := spec) (st := st) (f := f)
        (st' := st') h
  | @pos α inst spec =>
      exact elaborateItem_pos_mergesRight (spec := spec) (st := st) (f := f)
        (st' := st') h

@[simp] theorem elaborateItems_nil_eq :
    Spec.elaborateItems [] = Parser.pure id := rfl

@[simp] theorem elaborateItems_cons_eq (item : ItemSpec) (rest : List ItemSpec) :
    Spec.elaborateItems (item :: rest) =
      Parser.seq
        (Parser.map (fun f => fun (g : Spec.Partial → Spec.Partial) => g ∘ f)
          (Spec.elaborateItem item))
        (fun _ => Spec.elaborateItems rest) := rfl

theorem elaborateItems_nil_mergesRight
    (st : State)
    {f : Spec.Partial → Spec.Partial} {st' : State}
    (h : Spec.elaborateItems [] st = Result.ok f st') :
    mergesRight f := by
  classical
  have hEval : Result.ok (fun p => p) st = Result.ok f st' := by
    simpa [elaborateItems_nil_eq, Parser.pure] using h
  cases hEval
  simpa using mergesRight_id

theorem elaborateItems_mergesRight
    : ∀ items (st : State)
        {f : Spec.Partial → Spec.Partial} {st' : State},
        Spec.elaborateItems items st = Result.ok f st' → mergesRight f
  | [], st, f, st', h =>
      elaborateItems_nil_mergesRight (st := st) (f := f) (st' := st') h
  | item :: rest, st, f, st', h => by
      classical
      -- expand the sequence structure produced for a non-empty item list
      have hSeq :
          Parser.seq
              (Parser.map (fun f => fun (g : Spec.Partial → Spec.Partial) => g ∘ f)
                (Spec.elaborateItem item))
              (fun _ => Spec.elaborateItems rest) st = Result.ok f st' := by
            simpa [elaborateItems_cons_eq] using h
      -- analyse the head parser's outcome
      cases hItem : Spec.elaborateItem item st with
      | err err =>
          have : Result.ok f st' ≠ Result.err err := by simp
          simpa [Parser.seq, Parser.map, hItem] using hSeq
      | ok headFun st₁ =>
          -- evaluate the sequential composition in the success case
          simp [Parser.seq, Parser.map, hItem] at hSeq
          -- evaluate the tail parser on the updated state
          cases hTail : Spec.elaborateItems rest st₁ with
          | err err =>
              have : Result.ok f st' ≠ Result.err err := by simp
              simpa [Parser.seq, Parser.map, hTail] using hSeq
          | ok tailFun st₂ =>
              -- identify the final transformer emitted by the sequence
              have hSeqOk : Result.ok (tailFun ∘ headFun) st₂ = Result.ok f st' := by
                simpa [Parser.seq, Parser.map, hTail] using hSeq
              have hHeadMerge : mergesRight headFun :=
                elaborateItem_mergesRight (item := item) (st := st) (f := headFun)
                  (st' := st₁)
                  (by simpa [hItem])
              have hTailMerge : mergesRight tailFun :=
                elaborateItems_mergesRight rest st₁ (f := tailFun) (st' := st₂)
                  (by simpa using hTail)
              cases hSeqOk
              exact mergesRight_comp (g := headFun) (h := tailFun) hHeadMerge hTailMerge

theorem elaborateCommandCore_zero_result
    (cmd : CmdSpec) (st : State)
    {p : Spec.Partial} {st' : State}
    (h : Spec.elaborateCommandCore 0 cmd st = Result.ok p st') :
    p = Spec.Partial.empty ∧ st' = st := by
  have hEval : Result.ok Spec.Partial.empty st = Result.ok p st' := by
    simpa [Spec.elaborateCommandCore_zero, Parser.pure] using h
  cases hEval
  exact ⟨rfl, rfl⟩

end ArgParse.Proofs

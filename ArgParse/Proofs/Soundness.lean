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

/-- Auxiliary simp lemma describing the updated `options` list. -/
@[simp] theorem addOption_options (p : Spec.Partial) (name : String) (value : String) :
    (p.addOption name value).options = p.options ++ [(name, value)] := rfl

/-- Auxiliary simp lemma describing the updated `positionals` list. -/
@[simp] theorem addPositional_positionals
    (p : Spec.Partial) (name : String) (value : String) :
    (p.addPositional name value).positionals = p.positionals ++ [(name, value)] := rfl

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

end Partial

end ArgParse.Proofs

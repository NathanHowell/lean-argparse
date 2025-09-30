import ArgParse.Spec.Elab
import ArgParse.Proofs.Soundness
import ArgParse.Doc.Help
import ArgParse.Doc.Man
import ArgParse.Doc.Completion

/-!
# ArgParse.Proofs.Soundness.Summary

Soundness facts connecting the `Partial` accumulator with summary-based runners
and documentation helpers.
-/

namespace ArgParse.Proofs

open ArgParse
open ArgParse.Spec
open ArgParse.Doc
open Classical
open ArgParse.RunOutcome

namespace PartialSummary

/-- Folding `Partial.addFlag` is compatible with the `Summary.flagValue?` query. -/
@[simp] theorem flagValue?_fold_addFlag
    (entries : List (String × Bool)) (p : Spec.Partial) (name : String) :
    Spec.Partial.Summary.flagValue?
        ((entries.foldl (fun acc entry => acc.addFlag entry.fst entry.snd) p).toSummary) name =
      entries.foldl
        (fun latest entry => if entry.fst = name then some entry.snd else latest)
        (Spec.Partial.Summary.flagValue? (Spec.Partial.toSummary p) name) :=
  Partial.flagValue?_fold_addFlag entries p name

/-- Folding `Partial.addOption` preserves the chronological order queried by `Summary.optionValues`. -/
@[simp] theorem optionValues_fold_addOption
    (entries : List (String × String)) (p : Spec.Partial) (name : String) :
    Spec.Partial.Summary.optionValues
        ((entries.foldl (fun acc entry => acc.addOption entry.fst entry.snd) p).toSummary) name =
      Spec.Partial.Summary.optionValues (Spec.Partial.toSummary p) name ++
        entries.filterMap (fun entry =>
          if entry.fst = name then some entry.snd else none) :=
  Partial.optionValues_fold_addOption entries p name

/-- Folding `Partial.addPositional` preserves the chronological order queried by `Summary.positionalValues`. -/
@[simp] theorem positionalValues_fold_addPositional
    (entries : List (String × String)) (p : Spec.Partial) (name : String) :
    Spec.Partial.Summary.positionalValues
        ((entries.foldl (fun acc entry => acc.addPositional entry.fst entry.snd) p).toSummary) name =
      Spec.Partial.Summary.positionalValues (Spec.Partial.toSummary p) name ++
        entries.filterMap (fun entry =>
          if entry.fst = name then some entry.snd else none) :=
  Partial.positionalValues_fold_addPositional entries p name

/-- Running with summaries maps the raw payload through `Partial.toSummary`. -/
@[simp] theorem runNormalizedSummary_matches_raw (app : AppSpec) (st : State) :
    ArgParse.runNormalizedSummary app st =
      map Spec.Partial.toSummary (ArgParse.runNormalizedRaw app st) := rfl

/-- Running from tokens behaves identically after mapping the raw payload. -/
@[simp] theorem runSummary_matches_raw (app : AppSpec) (tokens : Tokens) :
    ArgParse.runSummary app tokens =
      map Spec.Partial.toSummary (ArgParse.runRaw app tokens) := rfl

/-- Rendering help with a summary argument agrees with the partial-based helper. -/
@[simp] theorem renderHelpWithSummary_eq_partial
    (spec : AppSpec) (partial? : Option Spec.Partial) :
    renderHelpWithSummary spec (partial?.map Partial.toSummary) =
      renderHelpWith spec partial? := rfl

/-- Rendering manpages with a summary argument agrees with the partial-based helper. -/
@[simp] theorem renderManWithSummary_eq_partial
    (spec : AppSpec) (partial? : Option Spec.Partial) :
    renderManWithSummary spec (partial?.map Partial.toSummary) =
      renderManWith spec partial? := rfl

/-- Rendering completions with a summary argument agrees with the partial-based helper. -/
@[simp] theorem renderCompletionsWithSummary_eq_partial
    (spec : AppSpec) (partial? : Option Spec.Partial) :
    renderCompletionWithSummary spec (partial?.map Partial.toSummary) =
      renderCompletionWith spec partial? := rfl

/-- Runtime annotations for a flag prefer the right-hand payload when partials are merged. -/
@[simp] theorem runtimeLinesForSummary_merge_flag
    (name : String) (lines : List String)
    (earlier later : Spec.Partial) :
    runtimeLinesForSummary
        (some (Spec.Partial.merge earlier later).toSummary)
        { heading := name, lines := lines, kind := EntryKind.flag } =
      match Spec.Partial.Summary.flagValue? (Spec.Partial.toSummary later) name with
      | some true => ["current: enabled"]
      | some false => ["current: disabled"]
      | none =>
          match Spec.Partial.Summary.flagValue? (Spec.Partial.toSummary earlier) name with
          | some true => ["current: enabled"]
          | some false => ["current: disabled"]
          | none => [] := by
  classical
  cases hLater :
      Spec.Partial.Summary.flagValue? (Spec.Partial.toSummary later) name with
  | none =>
      cases hEarlier :
          Spec.Partial.Summary.flagValue? (Spec.Partial.toSummary earlier) name with
      | none =>
          simp [runtimeLinesForSummary, hLater, hEarlier,
            Partial.flagValue?_merge (earlier := earlier) (later := later) (name := name)]
      | some prev =>
          cases prev <;>
            simp [runtimeLinesForSummary, hLater, hEarlier,
              Partial.flagValue?_merge (earlier := earlier) (later := later) (name := name)]
  | some value =>
      cases value <;>
        simp [runtimeLinesForSummary, hLater,
          Partial.flagValue?_merge (earlier := earlier) (later := later) (name := name)]

end PartialSummary

end ArgParse.Proofs

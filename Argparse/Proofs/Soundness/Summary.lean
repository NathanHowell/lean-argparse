import Argparse.Spec.Elab
import Argparse.Proofs.Soundness
import Argparse.Doc.Help
import Argparse.Doc.Man
import Argparse.Doc.Completion

/-!
# ArgParse.Proofs.Soundness.Summary

Soundness lemmas for the `Partial.Summary` helpers.
-/

namespace ArgParse.Proofs

open ArgParse
open ArgParse.Spec
open ArgParse.Doc
open Classical

namespace Partial.Summary

/-- Folding flag assignments and summarising still yields last-write-wins semantics. -/
lemma flagValue?_fold_addFlag
    (p : Partial) (name : String) (values : List Bool) :
    (Partial.toSummary (values.foldl (fun acc value => Partial.addFlag name value acc) p)).flagValue? name =
      match values.last? with
      | some value => some value
      | none => (Partial.toSummary p).flagValue? name := by
  classical
  set q := values.foldl (fun acc value => Partial.addFlag name value acc) p
  have hPartial := Partial.flagValue?_fold_addFlag (p := p) (name := name) (values := values)
  have hSummary := Partial.Summary.flagValue?_toSummary (p := q) (name := name)
  have hBase := Partial.Summary.flagValue?_toSummary (p := p) (name := name)
  cases hLast : values.last? with
  | none =>
      have hPartial' : q.flagValue? name = p.flagValue? name := by
        simpa [q, hLast] using hPartial
      calc
        (Partial.toSummary q).flagValue? name
            = q.flagValue? name := hSummary
        _ = p.flagValue? name := hPartial'
        _ = (Partial.toSummary p).flagValue? name := hBase.symm
  | some value =>
      have hPartial' : q.flagValue? name = some value := by
        simpa [q, hLast] using hPartial
      simpa [q, hLast, hPartial'] using hSummary

/-- Folding option values and summarising preserves deterministic accumulation order. -/
lemma optionValues_fold_addOption
    (p : Partial) (name : String) (values : List String) :
    (Partial.toSummary (values.foldl (fun acc value => Partial.addOption name value acc) p)).optionValues name =
      values.reverse ++ (Partial.toSummary p).optionValues name := by
  classical
  set q := values.foldl (fun acc value => Partial.addOption name value acc) p
  have hPartial := Partial.optionValues_fold_addOption (p := p) (name := name) (values := values)
  have hSummary := Partial.Summary.optionValues_toSummary (p := q) (name := name)
  have hBase := Partial.Summary.optionValues_toSummary (p := p) (name := name)
  calc
    (Partial.toSummary q).optionValues name
        = q.optionValues name := hSummary
    _ = values.reverse ++ p.optionValues name := by
          simpa [q] using hPartial
    _ = values.reverse ++ (Partial.toSummary p).optionValues name := by
          simpa [hBase.symm]

/-- Folding positional values and summarising preserves deterministic accumulation order. -/
lemma positionalValues_fold_addPositional
    (p : Partial) (name : String) (values : List String) :
    (Partial.toSummary (values.foldl (fun acc value => Partial.addPositional name value acc) p)).positionalValues name =
      values.reverse ++ (Partial.toSummary p).positionalValues name := by
  classical
  set q := values.foldl (fun acc value => Partial.addPositional name value acc) p
  have hPartial := Partial.positionalValues_fold_addPositional (p := p) (name := name) (values := values)
  have hSummary := Partial.Summary.positionalValues_toSummary (p := q) (name := name)
  have hBase := Partial.Summary.positionalValues_toSummary (p := p) (name := name)
  calc
    (Partial.toSummary q).positionalValues name
        = q.positionalValues name := hSummary
    _ = values.reverse ++ p.positionalValues name := by
          simpa [q] using hPartial
    _ = values.reverse ++ (Partial.toSummary p).positionalValues name := by
          simpa [hBase.symm]

/-- `runNormalizedSummary` mirrors the raw runner, only post-processing payloads. -/
lemma runNormalizedSummary_matches_raw
    (app : AppSpec) (st : State) :
    let raw := ArgParse.runNormalizedRaw app st
    let summary := ArgParse.runNormalizedSummary app st
    summary =
      match raw.result with
      | .ok payload => { result := RunResult.ok (Partial.toSummary payload), state := raw.state }
      | .help txt => { result := .help txt, state := raw.state }
      | .man txt => { result := .man txt, state := raw.state }
      | .completions txt => { result := .completions txt, state := raw.state }
      | .err err => { result := .err err, state := raw.state } := by
  classical
  unfold ArgParse.runNormalizedRaw ArgParse.runNormalizedSummary ArgParse.runNormalized
  -- `builtinOutcome?` pattern matches on `st.pre`; expand it explicitly.
  unfold ArgParse.builtinOutcome?
  cases hPre : st.pre with
  | nil =>
      simp [hPre]
      cases hEval : Spec.elaborateApp app st with
      | err error =>
          simp [hEval]
      | ok payload st' =>
          simp [hEval]
  | cons token rest =>
      by_cases hHelp : token = "--help"
      · subst hHelp
        simp
      · by_cases hMan : token = "--man"
        · subst hMan
          simp
        · by_cases hComp : token = "--generate-completions"
          · subst hComp
            simp
          · simp [hHelp, hMan, hComp] -- simplify builtin match to `none`
            cases hEval : Spec.elaborateApp app st with
            | err error =>
                simp [hEval]
            | ok payload st' =>
                simp [hEval]

/-- `runSummary` mirrors `runRaw`, only summarising the payload. -/
lemma runSummary_matches_raw
    (app : AppSpec) (tokens : Tokens) :
    let raw := ArgParse.runRaw app tokens
    let summary := ArgParse.runSummary app tokens
    summary =
      match raw.result with
      | .ok payload => { result := RunResult.ok (Partial.toSummary payload), state := raw.state }
      | .help txt => { result := .help txt, state := raw.state }
      | .man txt => { result := .man txt, state := raw.state }
      | .completions txt => { result := .completions txt, state := raw.state }
      | .err err => { result := .err err, state := raw.state } := by
  classical
  set st := ArgParse.Core.normalize tokens
  have h := runNormalizedSummary_matches_raw (app := app) (st := st)
  simpa [ArgParse.runRaw, ArgParse.runSummary, st] using h

/-- Rendering help with a summary derived from `Partial` matches the partial-based renderer. -/
lemma renderHelpWithSummary_eq_partial
    (spec : AppSpec) (p : Partial) :
    renderHelpWithSummary spec (some (Partial.toSummary p)) =
      renderHelpWith spec (some p) := rfl

/-- Rendering manpages with a summary derived from `Partial` matches the partial-based renderer. -/
lemma renderManWithSummary_eq_partial
    (spec : AppSpec) (p : Partial) :
    renderManWithSummary spec (some (Partial.toSummary p)) =
      renderManWith spec (some p) := rfl

/-- Rendering completions with a summary derived from `Partial` matches the partial-based renderer. -/
lemma renderCompletionsWithSummary_eq_partial
    (spec : AppSpec) (p : Partial) :
    renderCompletionsWithSummary spec (some (Partial.toSummary p)) =
      renderCompletionsWith spec (some p) := rfl

end Partial.Summary

end ArgParse.Proofs

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

theorem runNormalizedSummary_ok_exists_partial
    (app : AppSpec) (st : State)
    {summary : Spec.Partial.Summary} {st' : State}
    (h : ArgParse.runNormalizedSummary app st = RunOutcome.ok summary st') :
    ∃ payload : Spec.Partial,
      summary = Spec.Partial.toSummary payload ∧
        ArgParse.runNormalizedRaw app st = RunOutcome.ok payload st' := by
  classical
  cases hRaw : ArgParse.runNormalizedRaw app st with
  | mk result st₀ =>
      cases result with
      | ok payload =>
          have : RunOutcome.ok (Spec.Partial.toSummary payload) st₀ =
              RunOutcome.ok summary st' := by
            simpa [runNormalizedSummary_matches_raw, hRaw]
              using h
          cases this
          refine ⟨payload, rfl, ?_⟩
          simpa [hRaw]
      | help text =>
          have : ⟨RunResult.help text, st₀⟩ =
              RunOutcome.ok summary st' := by
            simpa [runNormalizedSummary_matches_raw, hRaw]
              using h
          cases this
      | man text =>
          have : ⟨RunResult.man text, st₀⟩ =
              RunOutcome.ok summary st' := by
            simpa [runNormalizedSummary_matches_raw, hRaw]
              using h
          cases this
      | completions text =>
          have : ⟨RunResult.completions text, st₀⟩ =
              RunOutcome.ok summary st' := by
            simpa [runNormalizedSummary_matches_raw, hRaw]
              using h
          cases this
      | err err =>
          have : RunOutcome.err err st₀ = RunOutcome.ok summary st' := by
            simpa [runNormalizedSummary_matches_raw, hRaw] using h
          cases this

theorem runSummary_ok_exists_partial
    (app : AppSpec) (tokens : Tokens)
    {summary : Spec.Partial.Summary} {st' : State}
    (h : ArgParse.runSummary app tokens = RunOutcome.ok summary st') :
    ∃ payload : Spec.Partial,
      summary = Spec.Partial.toSummary payload ∧
        ArgParse.runRaw app tokens = RunOutcome.ok payload st' := by
  classical
  cases hRaw : ArgParse.runRaw app tokens with
  | mk result st₀ =>
      cases result with
      | ok payload =>
          have : RunOutcome.ok (Spec.Partial.toSummary payload) st₀ =
              RunOutcome.ok summary st' := by
            simpa [runSummary_matches_raw, hRaw] using h
          cases this
          refine ⟨payload, rfl, ?_⟩
          simpa [hRaw]
      | help text =>
          have : ⟨RunResult.help text, st₀⟩ =
              RunOutcome.ok summary st' := by
            simpa [runSummary_matches_raw, hRaw] using h
          cases this
      | man text =>
          have : ⟨RunResult.man text, st₀⟩ =
              RunOutcome.ok summary st' := by
            simpa [runSummary_matches_raw, hRaw] using h
          cases this
      | completions text =>
          have : ⟨RunResult.completions text, st₀⟩ =
              RunOutcome.ok summary st' := by
            simpa [runSummary_matches_raw, hRaw] using h
          cases this
      | err err =>
          have : RunOutcome.err err st₀ = RunOutcome.ok summary st' := by
            simpa [runSummary_matches_raw, hRaw] using h
          cases this

theorem runNormalizedSummary_mergesRight
    (app : AppSpec) (st : State)
    {summary : Spec.Partial.Summary} {st' : State}
    (h : ArgParse.runNormalizedSummary app st = RunOutcome.ok summary st') :
    ∃ payload : Spec.Partial,
      summary = Spec.Partial.toSummary payload ∧
        _root_.ArgParse.Proofs.Partial.mergesRight
          (fun base => Spec.Partial.merge base payload) := by
  classical
  obtain ⟨payload, hSummary, hRaw⟩ :=
    runNormalizedSummary_ok_exists_partial (app := app) (st := st)
      (summary := summary) (st' := st') h
  refine ⟨payload, hSummary, ?_⟩
  exact
    _root_.ArgParse.Proofs.runNormalizedRaw_mergesRight (app := app) (st := st)
      (payload := payload) (st' := st') hRaw

theorem runSummary_mergesRight
    (app : AppSpec) (tokens : Tokens)
    {summary : Spec.Partial.Summary} {st' : State}
    (h : ArgParse.runSummary app tokens = RunOutcome.ok summary st') :
    ∃ payload : Spec.Partial,
      summary = Spec.Partial.toSummary payload ∧
        _root_.ArgParse.Proofs.Partial.mergesRight
          (fun base => Spec.Partial.merge base payload) := by
  classical
  obtain ⟨payload, hSummary, hRaw⟩ :=
    runSummary_ok_exists_partial (app := app) (tokens := tokens)
      (summary := summary) (st' := st') h
  refine ⟨payload, hSummary, ?_⟩
  exact
    _root_.ArgParse.Proofs.runRaw_mergesRight (app := app) (tokens := tokens)
      (payload := payload) (st' := st') hRaw


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

/-- Option annotations reflect the appended values when partial payloads are merged. -/
@[simp] theorem runtimeLinesForSummary_merge_option
    (name : String) (lines : List String)
    (earlier later : Spec.Partial) :
    runtimeLinesForSummary
        (some (Spec.Partial.merge earlier later).toSummary)
        { heading := name, lines := lines, kind := EntryKind.option } =
      if (Spec.Partial.Summary.optionValues (Spec.Partial.toSummary earlier) name ++
          Spec.Partial.Summary.optionValues (Spec.Partial.toSummary later) name).isEmpty then []
      else
        [s!"current: {String.intercalate ", " (
            Spec.Partial.Summary.optionValues (Spec.Partial.toSummary earlier) name ++
            Spec.Partial.Summary.optionValues (Spec.Partial.toSummary later) name)}"] := by
  classical
  have hMerged :=
    Partial.optionValues_merge (earlier := earlier) (later := later) (name := name)
  simp [runtimeLinesForSummary, hMerged, List.isEmpty]

/-- Positional annotations reflect the appended values when partial payloads are merged. -/
@[simp] theorem runtimeLinesForSummary_merge_positional
    (name : String) (lines : List String)
    (earlier later : Spec.Partial) :
    runtimeLinesForSummary
        (some (Spec.Partial.merge earlier later).toSummary)
        { heading := name, lines := lines, kind := EntryKind.positional } =
      if (Spec.Partial.Summary.positionalValues (Spec.Partial.toSummary earlier) name ++
          Spec.Partial.Summary.positionalValues (Spec.Partial.toSummary later) name).isEmpty then []
      else
        [s!"current: {String.intercalate ", " (
            Spec.Partial.Summary.positionalValues (Spec.Partial.toSummary earlier) name ++
            Spec.Partial.Summary.positionalValues (Spec.Partial.toSummary later) name)}"] := by
  classical
  have hMerged :=
    Partial.positionalValues_merge (earlier := earlier) (later := later) (name := name)
  simp [runtimeLinesForSummary, hMerged, List.isEmpty]

/-- Flag paragraphs in the manpage renderer prefer the right-hand payload when partials merge. -/
@[simp] theorem runtimeParagraphs_merge_flag
    (name : String) (lines : List String)
    (earlier later : Spec.Partial) :
    runtimeParagraphs
        (some (Spec.Partial.merge earlier later).toSummary)
        { heading := name, lines := lines, kind := EntryKind.flag } =
      match Spec.Partial.Summary.flagValue? (Spec.Partial.toSummary later) name with
      | some true => [".Pp current: enabled"]
      | some false => [".Pp current: disabled"]
      | none =>
          match Spec.Partial.Summary.flagValue? (Spec.Partial.toSummary earlier) name with
          | some true => [".Pp current: enabled"]
          | some false => [".Pp current: disabled"]
          | none => [] := by
  classical
  cases hLater :
      Spec.Partial.Summary.flagValue? (Spec.Partial.toSummary later) name with
  | none =>
      cases hEarlier :
          Spec.Partial.Summary.flagValue? (Spec.Partial.toSummary earlier) name with
      | none =>
          simp [runtimeParagraphs, hLater, hEarlier,
            Partial.flagValue?_merge (earlier := earlier) (later := later) (name := name)]
      | some prev =>
          cases prev <;>
            simp [runtimeParagraphs, hLater, hEarlier,
              Partial.flagValue?_merge (earlier := earlier) (later := later) (name := name)]
  | some value =>
      cases value <;>
        simp [runtimeParagraphs, hLater,
          Partial.flagValue?_merge (earlier := earlier) (later := later) (name := name)]

/-- Option paragraphs reflect appended values when partials merge. -/
@[simp] theorem runtimeParagraphs_merge_option
    (name : String) (lines : List String)
    (earlier later : Spec.Partial) :
    runtimeParagraphs
        (some (Spec.Partial.merge earlier later).toSummary)
        { heading := name, lines := lines, kind := EntryKind.option } =
      if (Spec.Partial.Summary.optionValues (Spec.Partial.toSummary earlier) name ++
          Spec.Partial.Summary.optionValues (Spec.Partial.toSummary later) name).isEmpty then []
      else
        [s!".Pp current: {String.intercalate ", " (
            Spec.Partial.Summary.optionValues (Spec.Partial.toSummary earlier) name ++
            Spec.Partial.Summary.optionValues (Spec.Partial.toSummary later) name)}"] := by
  classical
  have hMerged :=
    Partial.optionValues_merge (earlier := earlier) (later := later) (name := name)
  simp [runtimeParagraphs, hMerged, List.isEmpty]

/-- Positional paragraphs reflect appended values when partials merge. -/
@[simp] theorem runtimeParagraphs_merge_positional
    (name : String) (lines : List String)
    (earlier later : Spec.Partial) :
    runtimeParagraphs
        (some (Spec.Partial.merge earlier later).toSummary)
        { heading := name, lines := lines, kind := EntryKind.positional } =
      if (Spec.Partial.Summary.positionalValues (Spec.Partial.toSummary earlier) name ++
          Spec.Partial.Summary.positionalValues (Spec.Partial.toSummary later) name).isEmpty then []
      else
        [s!".Pp current: {String.intercalate ", " (
            Spec.Partial.Summary.positionalValues (Spec.Partial.toSummary earlier) name ++
            Spec.Partial.Summary.positionalValues (Spec.Partial.toSummary later) name)}"] := by
  classical
  have hMerged :=
    Partial.positionalValues_merge (earlier := earlier) (later := later) (name := name)
  simp [runtimeParagraphs, hMerged, List.isEmpty]

/-- Completion suggestions retain the union of option/positional terms when partials merge. -/
@[simp] theorem suggestionsWithSummary_merge_values
    (spec : AppSpec)
    (earlier later : Spec.Partial) :
    suggestionsWithSummary spec
        (some (Spec.Partial.merge earlier later).toSummary) =
      ((describeApp spec |>.map (·.heading)) ++
        ((Spec.Partial.merge earlier later).toSummary.flags.map (·.fst) ++
          (Spec.Partial.merge earlier later).toSummary.options.foldr
            (fun entry acc =>
              match entry with
              | (name, values) =>
                  values.foldr (fun value acc' => s!"{name}={value}" :: acc') acc)
            [] ++
          (Spec.Partial.merge earlier later).toSummary.positionals.foldr
            (fun entry acc =>
              match entry with
              | (name, values) =>
                  values.foldr (fun value acc' => s!"{name}:{value}" :: acc') acc)
            [])).eraseDups := by
      classical
      simp [suggestionsWithSummary]

/-- Rendering completion output after merging partial payloads reflects the combined
collector suggestions. -/
@[simp] theorem renderCompletionWithSummary_merge_values
    (spec : AppSpec)
    (earlier later : Spec.Partial) :
    renderCompletionWithSummary spec
        (some (Spec.Partial.merge earlier later).toSummary) =
      String.intercalate "\n"
        (((describeApp spec |>.map (·.heading)) ++
          ((Spec.Partial.merge earlier later).toSummary.flags.map (·.fst) ++
            (Spec.Partial.merge earlier later).toSummary.options.foldr
              (fun entry acc =>
                match entry with
                | (name, values) =>
                    values.foldr (fun value acc' => s!"{name}={value}" :: acc') acc)
              [] ++
            (Spec.Partial.merge earlier later).toSummary.positionals.foldr
              (fun entry acc =>
                match entry with
                | (name, values) =>
                    values.foldr (fun value acc' => s!"{name}:{value}" :: acc') acc)
              [])).eraseDups) := by
  classical
  simp [renderCompletionWithSummary, suggestionsWithSummary_merge_values]

/-- The CLI wrapper inherits the merge-aware completion rendering behaviour. -/
@[simp] theorem CLI.renderCompletionsWithSummary_merge_values
    (spec : AppSpec)
    (earlier later : Spec.Partial) :
    ArgParse.CLI.renderCompletionsWithSummary spec
        (some (Spec.Partial.merge earlier later).toSummary) =
      String.intercalate "\n"
        (((describeApp spec |>.map (·.heading)) ++
          ((Spec.Partial.merge earlier later).toSummary.flags.map (·.fst) ++
            (Spec.Partial.merge earlier later).toSummary.options.foldr
              (fun entry acc =>
                match entry with
                | (name, values) =>
                    values.foldr (fun value acc' => s!"{name}={value}" :: acc') acc)
              [] ++
            (Spec.Partial.merge earlier later).toSummary.positionals.foldr
              (fun entry acc =>
                match entry with
                | (name, values) =>
                    values.foldr (fun value acc' => s!"{name}:{value}" :: acc') acc)
              [])).eraseDups) := by
  classical
  simp [ArgParse.CLI.renderCompletionsWithSummary, renderCompletionWithSummary_merge_values]

@[simp] private def helpRuntimeMerged
    (entry : DocEntry) (earlier later : Spec.Partial) : List String :=
  (runtimeLinesForSummary
      (some (Spec.Partial.merge earlier later).toSummary) entry).map
    (fun line => s!"  {line}")

@[simp] private def helpEntryMerged
    (entry : DocEntry) (earlier later : Spec.Partial) : String :=
  String.intercalate "\n"
    (entry.heading ::
      entry.lines.map (fun line => s!"  {line}") ++
      helpRuntimeMerged entry earlier later)

@[simp] private def manRuntimeMerged
    (entry : DocEntry) (earlier later : Spec.Partial) : List String :=
  runtimeParagraphs
    (some (Spec.Partial.merge earlier later).toSummary) entry

@[simp] private def manSectionMerged
    (entry : DocEntry) (earlier later : Spec.Partial) : String :=
  String.intercalate "\n"
    (s!".Sh {entry.heading}" ::
      entry.lines.map (fun line => s!".Pp {line}") ++
      manRuntimeMerged entry earlier later)

@[simp] theorem renderEntryWithSummary_merge
    (entry : DocEntry)
    (earlier later : Spec.Partial) :
    renderEntryWithSummary entry
        (some (Spec.Partial.merge earlier later).toSummary) =
      helpEntryMerged entry earlier later := by
  classical
  cases entry with
  | mk heading lines kind =>
      cases kind <;>
        simp [renderEntryWithSummary, helpEntryMerged,
              runtimeLinesForSummary_merge_flag,
              runtimeLinesForSummary_merge_option,
              runtimeLinesForSummary_merge_positional,
              runtimeLinesForSummary, List.map]

@[simp] theorem renderSectionWithSummary_merge
    (entry : DocEntry)
    (earlier later : Spec.Partial) :
    renderSectionWithSummary entry
        (some (Spec.Partial.merge earlier later).toSummary) =
      manSectionMerged entry earlier later := by
  classical
  cases entry with
  | mk heading lines kind =>
      cases kind <;>
        simp [renderSectionWithSummary, manSectionMerged,
              runtimeParagraphs_merge_flag,
              runtimeParagraphs_merge_option,
              runtimeParagraphs_merge_positional,
              runtimeParagraphs, List.map]

/-- Help rendering over a merged payload combines the per-entry annotations produced by
`helpEntryMerged`. -/
@[simp] theorem renderHelpWithSummary_merge_values
    (spec : AppSpec)
    (earlier later : Spec.Partial) :
    renderHelpWithSummary spec
        (some (Spec.Partial.merge earlier later).toSummary) =
      String.intercalate "\n\n"
        ((describeApp spec).map
          (fun entry => helpEntryMerged entry earlier later)) := by
  classical
  simp [renderHelpWithSummary, renderEntryWithSummary_merge]

/-- CLI help rendering inherits the merge-aware annotations. -/
@[simp] theorem CLI.renderHelpWithSummary_merge_values
    (spec : AppSpec)
    (earlier later : Spec.Partial) :
    ArgParse.CLI.renderHelpWithSummary spec
        (some (Spec.Partial.merge earlier later).toSummary) =
      String.intercalate "\n\n"
        ((describeApp spec).map
          (fun entry => helpEntryMerged entry earlier later)) := by
  classical
  simp [ArgParse.CLI.renderHelpWithSummary, renderHelpWithSummary_merge_values]

/-- Manpage rendering over a merged payload combines the per-section annotations produced by
`manSectionMerged`. -/
@[simp] theorem renderManWithSummary_merge_values
    (spec : AppSpec)
    (earlier later : Spec.Partial) :
    renderManWithSummary spec
        (some (Spec.Partial.merge earlier later).toSummary) =
      String.intercalate "\n"
        (let header := s!".Dd Generated\n.Dt {spec.name}\n.Os"
         let sections := (describeApp spec).map
            (fun entry => manSectionMerged entry earlier later)
         header :: sections) := by
  classical
  simp [renderManWithSummary, renderSectionWithSummary_merge, manSectionMerged]

/-- CLI manpage rendering inherits the merge-aware annotations. -/
@[simp] theorem CLI.renderManWithSummary_merge_values
    (spec : AppSpec)
    (earlier later : Spec.Partial) :
    ArgParse.CLI.renderManWithSummary spec
        (some (Spec.Partial.merge earlier later).toSummary) =
      String.intercalate "\n"
        (let header := s!".Dd Generated\n.Dt {spec.name}\n.Os"
         let sections := (describeApp spec).map
            (fun entry => manSectionMerged entry earlier later)
         header :: sections) := by
  classical
  simp [ArgParse.CLI.renderManWithSummary, renderManWithSummary_merge_values]
end PartialSummary

end ArgParse.Proofs

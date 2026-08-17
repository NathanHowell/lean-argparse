# Plan

Working notes for `lean-argparse`. This file tracks current state and what's
next; the historical play-by-play lives in git history. `DESIGN.md` is the
design of record.

## Current state (2026-08-17)

All ten roadmap items are closed. Three were bugs rather than missing theorems:
`P.many` truncated bundled flags, `entryRow` let a wide label abut its
description, and `-vn5` did not parse. One roadmap note was wrong --
`String.startsWith` does not block proofs -- and is corrected below.

The paired-applicative migration is complete: all seven layers of `DESIGN.md`
are built, with no `sorry` anywhere and no `partial def` outside `Core`.

- **Layer 1 — runtime** (`Core/`): argv normalization with `--` sentinel
  splitting; flag parsers with short-name bundling; option parsers with
  `--name value`, `--name=value`, inline `-n5` concatenation, and
  `.one`/`.many`/`.some` arities (last value wins for `.one`);
  order-insensitive scanning (`flagScan`, `optionScan`) with `scopedPre`
  restricting a command's scan to the segment before the first subcommand
  name; positional parsers over pre/post streams; subcommand dispatch.
- **Layer 2 — `P`** (`P.lean`): `Doc` plus `Parser`, with `Functor`,
  `Applicative`, and `Alternative` instances that compose `run` and zip `doc`.
  `many` is fuel-bounded; `optional` is spelled as `alt [d, none]`.
- **Layer 3 — builders** (`Builder.lean`): the only place `doc` and `run` are
  zipped. Named arguments replace the `Mod` monoid.
- **Layer 4 — `Cmd`** (`Cmd.lean`): the command tree, with `toParser` and
  `toCmdSpec` walking the same `subs` list, per-node globals, and a
  bundle-expanding pre-pass that splits `-vn5` using the items legal at that
  command.
- **Layer 5 — runner** (`Exec.lean`): `--help` at every level, `--version`,
  `--man`, completion candidates, installable bash/zsh/fish completion scripts,
  usage synopses, and error rendering with nearest-match suggestions.
  Applications contain no help code.
- **Layer 6 — correspondence** (`Correspondence.lean`): item agreement per
  builder; behavioural acceptance for every builder -- flags, the four option
  builders through their shared `optionValues` core, and the three positionals
  -- in both directions, accepting what they claim and declining what they do
  not; verb agreement lifted over the tree (including pointwise dispatch: the
  entry named `foo` runs `foo`'s parser); help coverage; completion agreement;
  soundness of the `unknownLong?` diagnostic.
- **Layer 7 — deriving** (`Deriving.lean`): `deriving ArgParse.Parseable`
  generates a `P` from a structure. Short forms, positionals, and metavars
  travel in field types via `Arg α o`.
- **Runtime proofs** (`Proofs/`): lawful Functor/Applicative for `Parser`; flag
  totality and the collector cursor lemma; determinism; sentinel factorization;
  scan/front-of-stream agreement on syntactically canonical argv, for flags
  (bundles included) as well as options, and across the detached, `=`-form, and
  concatenated token forms; `Doc`
  normalization preserving items and being idempotent; the applicative laws for
  `P` itself, up to normalization; `P.many`'s repetition bound proved slack for
  parsers that progress, with the flag builder shown to be one; completeness --
  how success travels through the applicative, what each builder needs, and a
  closed end-to-end parse through normalization, globals, dispatch, an option,
  and a positional.
- **Tooling**: demo CLI in `Main.lean` (greet/repeat, derived); derived example
  under `Examples/Derived.lean`; unit, integration, and deriving checks;
  docstring/simp lint driver; doc-gen4 setup under `docbuild/`.

## Roadmap

Empty. The ten items are closed, and the bug that closing them turned up --
`-vn5` failing to parse -- is fixed as far as the layering allows; the residue
is recorded as a design note rather than a to-do, with the price of removing it
written down.

Deliberately not on the list: `usageLine`, `renderCommandHelp`, `renderMan`,
`editDistance`, and `nearest?` have no theorems and should not get any. They are
string formatting, where proofs cost a great deal and buy little, and the tests
already pin the behaviour.

## Design notes / decisions pending

- `.one` options use last-value-wins; `.many`/`.some` accumulate chronological
  lists.
- Scanning semantics: flags/options match anywhere within the current command's
  segment (bounded by the first subcommand name and the `--` sentinel);
  positionals stay front-of-stream over the residual tokens. Known ambiguity: a
  detached option value that lexes as a defined flag (`--message -v`) is claimed
  by the flag scan — `--name=value` forces the value reading.
- Help routing (`Cmd.descend`) skips tokens that name no subcommand, so an
  option *value* equal to a verb name can select the wrong help page. It only
  ever chooses which page to print, never how anything parses.
- Builtins are matched as whole tokens, so `-h` bundled into `-vh` is not
  detected: resolving a bundle needs the flag specs of the command the tokens
  belong to, which is not known until dispatch has happened.
- An inline bundle is split by the payload type: `findConcatSplit?` takes the
  longest prefix of the tail that decodes. `-n5v` is `5` plus a residual `-v`
  for a `Nat` option, and `-nfoo` is the whole tail for a `String` one, since
  `String` decodes anything. `findConcatSplit?_split` guarantees the residue is
  a non-empty suffix that concatenates back to the tail, so the re-dashed token
  is never something the user did not type. Tests pin where the boundary falls.
- Bundles that *lead with flags* are split before anything scans, by
  `Core.expandBundles` running in `Cmd.toParser` over the segment each command
  owns. `-vn5` becomes `-v -n5` whichever order the parser sequences its items
  in. The pass is conservative -- it rewrites only a non-empty run of this
  command's flag shorts followed by one of its option shorts -- so `-n5v` still
  reaches the concatenation path, `-vf` still reaches the flag scan's own bundle
  rewrite, and a token with an unknown short is left byte-for-byte alone.
- Bundles that *lead with an option* still depend on sequencing: `-n5v` parses
  only if the flag is sequenced after the option. The split there happens during
  the option's own scan, which pushes the residue `-v` back onto the stream, and
  a flag that already ran cannot see it. Doing that split up front instead would
  need the value's decoder -- `5v` is `5` then `v` for a `Nat` and the whole
  string for a `String` -- and the item list the expansion pass reads is
  type-erased. Carrying a splitter through it means a function field on
  `ItemSpec` or `Doc`, which costs the derived `Repr` and `DecidableEq` that
  `Exec.exec` (`args.contains`) and the renderers depend on. Not worth it while
  `-n5 -v` and `--count=5 -v` work in either order, and the failure is a
  leftover error rather than a wrong parse. Pinned by tests in both directions.
- Suggestion threshold is 2 edits above three characters, which catches
  transpositions (`chidl` → `child`) at the cost of the occasional unhelpful
  but valid neighbour.
- Error precedence: a dispatch failure on a token that does not start with `-`
  outranks any unknown-option finding. A misspelled verb strands every token
  after it -- they were meant for a command never reached -- so reporting one of
  *those* names the wrong token. `ci scop --tier pr` is a misspelling of `scope`,
  not a problem with `--tier`. The guard on `-` matters: dispatch also fails when
  an option appears where a verb belongs (`ci --tier pr`), and there the option
  is the right thing to report. Reported downstream as nsnd-irq0.
- `deriving Parseable` rejects, rather than mistranslates, a default that
  depends on an earlier field and a `Bool` defaulting to `true`.
- Completion scripts are emitted for `eval`, not for autoloading. zsh's `_prog`
  in `$fpath` and fish's `completions/prog.fish` would work, but each needs the
  file in a particular place under a particular name, where
  `eval "$(prog --completion-script zsh)"` is one instruction that reads the
  same for all three shells. The script embeds only the binary name and the
  query flag, so it never goes stale.
- `rw` with a `String.startsWith` lemma usually fails where `simp` succeeds: the
  `ForwardPattern` instance is indexed by the pattern and stops matching once
  the surrounding definitions are unfolded. Prefer `split` over
  `rw [if_pos …]` on a `startsWith` guard, and close the impossible branch with
  `absurd`.
- `String.startsWith` is not the proof obstacle it looked like. It routes
  through `String.Slice.Pattern`, so it does not reduce, but `simp` rewrites it
  to a `List.IsPrefix` claim about `toList`, and list reasoning finishes the
  job. `startsWith_append_eq_false` is the instance that mattered: reaching the
  detached `--name value` branch means first ruling out `--name=`, and the
  ruling-out is a length argument. Note that `rw` with that lemma fails where
  `simp` succeeds -- the `ForwardPattern` instance argument does not match
  syntactically -- so unfold the surrounding `if` first and rewrite into a goal
  that still mentions `Core.longLexeme` unexpanded.
- The repetition bound in `P.many` counts characters, not tokens, because a
  bundled short flag advances the cursor without shortening the stream: `-vvv`
  becomes `-vv`. A token-count bound stops one iteration early there, and
  silently, since exhausting the fuel is indistinguishable from the parser
  declining. `State.budget` charges per token *and* per character;
  `Proofs/Many.lean` proves the result no longer depends on the bound at all,
  for any parser that spends budget when it advances.
- `Doc.normalize` has no caller outside its own recursion: the renderers all
  read `Doc.items`, which is insensitive to nesting, so none of them needs it.
  It earns its place as the equivalence the applicative laws are stated up to
  (`Proofs/PLaws.lean`), not as a rendering pass. Worth remembering before
  anyone deletes it as dead code.
- `P` is lawful only up to `Doc.normalize`, and that is not a shortcoming to be
  fixed later. `pure f <*> x` genuinely documents itself as `seq [none, x.doc]`
  where `f <$> x` documents itself as `x.doc`; the two describe the same parse
  without being the same term. `P.Equiv` -- equal `run`, descriptions that
  normalize alike -- is the honest relation, and `equiv_items` is why it is the
  right one: equivalent parsers print the same help. The `Functor` laws need
  none of this and hold on the nose, so `LawfulFunctor P` is a real instance;
  `LawfulApplicative P` is false and is deliberately not declared.

## Process guardrails

- Always run `lake build; lake test; lake lint` before committing.
- Small, focused commits; keep this file about current state and next steps.

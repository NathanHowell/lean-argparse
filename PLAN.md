# Plan

Working notes for `lean-argparse`. This file tracks current state and what's
next; the historical play-by-play lives in git history. `DESIGN.md` is the
design of record.

## Current state (2026-08-17)

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
  `toCmdSpec` walking the same `subs` list, and per-node globals.
- **Layer 5 — runner** (`Exec.lean`): `--help` at every level, `--version`,
  `--man`, completion, usage synopses, and error rendering with nearest-match
  suggestions. Applications contain no help code.
- **Layer 6 — correspondence** (`Correspondence.lean`): item agreement per
  builder, behavioural acceptance, verb agreement lifted over the tree, help
  coverage, completion agreement.
- **Layer 7 — deriving** (`Deriving.lean`): `deriving ArgParse.Parseable`
  generates a `P` from a structure. Short forms, positionals, and metavars
  travel in field types via `Arg α o`.
- **Runtime proofs** (`Proofs/`): lawful Functor/Applicative; flag totality and
  the collector cursor lemma; determinism; sentinel factorization;
  scan/front-of-stream agreement on syntactically canonical argv.
- **Tooling**: demo CLI in `Main.lean` (greet/repeat, derived); derived example
  under `Examples/Derived.lean`; unit, integration, and deriving checks;
  docstring/simp lint driver; doc-gen4 setup under `docbuild/`.

## Roadmap

Ordered by value rather than by the sequence they were noticed in. The first
three came out of auditing which definitions have no theorem mentioning them at
all, and they outrank most of what follows: one guards against a silent failure,
one covers the headline abstraction, and one closes a hole in a guarantee that
is already advertised.

1. **`Doc.normalize` preserves items** — `items (normalize d) = items d`.
   `P.lean` claims normalization is a rendering-quality concern that "never
   touches parsing". Half of that is free, since `run` is not in scope there.
   The other half -- that it does not change what is *documented* -- is exactly
   what is unproved, and it is the one place a help-loss bug could hide in
   silence: if `flattenSeq` dropped an item, help would quietly stop mentioning
   it and no theorem would fire. Idempotence (`normalize (normalize d) =
   normalize d`) is worth having alongside it.
2. **`P` is lawful** — `LawfulFunctor`/`LawfulApplicative` are proved for
   `Parser` (`Proofs/Laws.lean`) and instantiated there, but not for `P`, which
   is what applications actually compose. The laws cannot hold on the nose:
   `seq [seq [a, b], c]` and `seq [a, seq [b, c]]` are different `Doc` trees, so
   they hold only up to `Doc.normalize`. That makes this depend on item 1, and
   it is the real reason `normalize` exists. Checked concretely: the two
   association trees the law relates are `seq(seq(seq(-,u),v),w)` and
   `seq(u,seq(v,w))`, and `normalize` sends both to `seq(u,v,w)`. State the laws
   with propositional equality -- `Doc` has no `BEq`/`DecidableEq`, because no
   deriving handler covers an inductive nesting through `List`.
3. **Verb agreement relates names to parsers, not just name lists** —
   `toSubcommands_names` proves the dispatch table's names equal the tree's
   names. Nothing proves the entry named `foo` runs `foo`'s parser; a
   `toSubcommands` that paired the first name with the second parser would
   satisfy every theorem currently stated. True by construction and cheap to
   prove, but the guarantee is advertised and not yet earned.
4. **Correspondence for the option builders' behaviour** — the behavioural
   acceptance lemmas cover `flag`. The seven option and positional builders have
   their data agreement proved but not their token-level acceptance.
5. **`P.many` progress** — `many` is bounded by token count and discards a
   non-advancing step. A progress lemma for the builders would let the bound be
   stated rather than assumed.
6. **Scan agreement for flags and bundles** — `Canonical` covers options; the
   analogous syntactic canonicality story for flag scanning (and for `=`-form
   and concatenated option tokens, whose classification the kernel cannot
   evaluate because `String.startsWith` is opaque) is still open.
7. **Completeness** — the missing half of the story: if argv conforms to a
   well-formed command tree, parsing succeeds and yields the expected bindings.
8. **`unknownLong?` soundness** — it should never flag a lexeme the command
   actually accepts, since a spurious "unrecognised `--foo`" is a user-facing
   bug. Provable against `Doc.pathItems`.
9. **Bundle-splitting edge cases** — inline bundles like `-n5v` with
   non-`String` payloads.
10. **Real completion scripts** — `--generate-completions` lists candidates.
    Emitting bash/zsh/fish scripts that call back into it is not done. The only
    feature on this list; everything above is a theorem.

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

## Process guardrails

- Always run `lake build; lake test; lake lint` before committing.
- Small, focused commits; keep this file about current state and next steps.

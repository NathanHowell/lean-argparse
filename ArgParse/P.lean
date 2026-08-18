import ArgParse.Core.Parser
import ArgParse.Doc

/-!
# ArgParse.P

The paired applicative: a runtime parser and a payload-free description of it,
moving through one `Applicative` in lockstep.

`Doc` is the static skeleton of a free applicative with the payloads deleted —
which is everything a renderer ever reads. Help only needs to know which items
exist, how they compose, and which branches alternate; none of that depends on
values already parsed. So `P` is introspectable to exactly the depth help needs
and opaque below it, at the cost of no universe bump and no interpreter.

The description is stored as a `Doc.Normalized` — a document in normal form,
carrying the proof that it is one. Every constructor here normalizes what it
builds, so the invariant holds by construction rather than by convention, and
`P` is lawfully applicative as a consequence: `pure f <*> x` and `f <$> x`
assemble different trees, but a type that admits only normal trees cannot tell
them apart. `Doc.Normalized.seq` is a monoid with `Doc.Normalized.empty`, which
is the whole content of the three descriptive halves of the applicative laws.

The instances never look inside `run` and never let a caller supply a `doc`
independently of one. Layer 3 is the only place the two are zipped, which is
what makes the correspondence between them provable rather than policed.
-/

namespace ArgParse

open ArgParse.Spec

/-- A parser paired with the description of what it parses.

The two fields are only ever constructed together, by Layer 3's builders. -/
structure P (α : Type) where
  /-- Static description read by the help, usage, and completion renderers,
  kept in normal form. -/
  doc : Doc.Normalized
  /-- Runtime parser, an opaque `State → Result α`. -/
  run : Parser α

namespace P

/-- Map the parsed value. The description is untouched: `f` cannot change which
items exist or how they compose. -/
@[inline] def map (f : α → β) (p : P α) : P β :=
  { doc := p.doc, run := Parser.map f p.run }

/-- Succeed without consuming input and without contributing to help. -/
@[inline] def pure (a : α) : P α :=
  { doc := .empty, run := Parser.pure a }

/-- Applicative composition: both sides run, so both sides are documented. -/
@[inline] def seq (pf : P (α → β)) (pa : Unit → P α) : P β :=
  let pa := pa ()
  { doc := pf.doc.seq pa.doc, run := Parser.seq pf.run (fun _ => pa.run) }

/-- Sequence, keeping the left value. -/
@[inline] def seqLeft (pa : P α) (pb : Unit → P β) : P α :=
  seq (map (fun a => fun (_ : β) => a) pa) pb

/-- Sequence, keeping the right value. -/
@[inline] def seqRight (pa : P α) (pb : Unit → P β) : P β :=
  seq (map (fun (_ : α) => id) pa) pb

/-- The parser that always fails, describing no alternatives.

An empty alternation documents nothing, so its normal form is the empty
document — the same one `pure` carries. Nothing downstream reads the difference:
both contribute no items. -/
@[inline] def failure : P α :=
  { doc := .empty, run := Parser.fail Parser.emptyError }

/-- Alternation: one of the two sides participates, so help shows a choice. -/
@[inline] def orElse (pa : P α) (pb : Unit → P α) : P α :=
  let pb := pb ()
  { doc := pa.doc.alt pb.doc, run := Parser.orElse pa.run (fun _ => pb.run) }

/-! ### Repetition

`many` needs a bound. A parser that succeeds without consuming would otherwise
loop forever, and `Parser` carries no proof that it always progresses. The bound
is `State.budget`, the most steps any progressing parser could manage; a step
that succeeds without advancing the cursor ends the repetition and is discarded,
so `many (pure x)` is `[]` rather than divergence.

The budget charges per character as well as per token. Token count alone is too
small: matching a short flag out of a bundle advances the cursor without
shortening the stream, so `many (flag …)` over `-vvv` used to stop one short and
leave `-v` on the stream. `Proofs.Many` shows the budget is now slack — the loop
always exits by failure or stall, never by exhaustion. -/

/-- Repeat `p` until it fails or stops consuming, bounded by `State.budget`. -/
def runMany (p : Parser α) : Parser (List α) := fun st =>
  let rec go : Nat → List α → State → Result (List α)
    | 0, acc, st => .ok acc.reverse st
    | fuel + 1, acc, st =>
        match p st with
        | .err _ => .ok acc.reverse st
        | .ok a st' =>
            if st.cursor < st'.cursor then
              go fuel (a :: acc) st'
            else
              .ok acc.reverse st
  go (st.budget + 1) [] st

/-- Collect zero or more occurrences. -/
@[inline] def many (p : P α) : P (List α) :=
  { doc := p.doc.repeated false, run := runMany p.run }

/-- Collect one or more occurrences, failing when none are present. -/
def some (p : P α) : P (List α) :=
  { doc := p.doc.repeated true
  , run := fun st =>
      match runMany p.run st with
      | .err e => .err e
      | .ok [] _ => .err { kind := .missingValue, context := [], expect := [] }
      | .ok values st' => .ok values st' }

/-- Make a parser optional.

Spelled as alternation with `pure none`, so its document is `alt [d, none]` —
the shape renderers already print as `[…]`. No `Doc` constructor is needed for
optionality. -/
@[inline] def optional (p : P α) : P (Option α) :=
  orElse (map Option.some p) (fun _ => pure Option.none)

/-- Replace the parsed value with a constant, keeping the description. -/
@[inline] def mapConst (b : β) (p : P α) : P β :=
  map (fun _ => b) p

/-- The items this parser can accept, read off its description. -/
@[inline] def items (p : P α) : List ItemSpec :=
  p.doc.items

end P

instance : Functor P where
  map := P.map

instance : Applicative P where
  map := P.map
  pure := P.pure
  seq := P.seq
  seqLeft := P.seqLeft
  seqRight := P.seqRight

instance : Alternative P where
  failure := P.failure
  orElse := P.orElse

end ArgParse

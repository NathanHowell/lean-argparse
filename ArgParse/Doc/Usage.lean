import ArgParse.Spec.Describe

/-!
# ArgParse.Doc.Usage

Usage synopses, and the shared item-label formatting the help and man renderers
both use. Pure functions of the render model.

The synopsis is the one renderer that reads the description *tree* rather than
the flat item list. Everything else about an item — whether it takes a value,
whether it may repeat, whether it may be omitted — travels on the `ItemSpec`.
Mutual exclusion does not: `(-a | -b)` is a fact about how two items compose,
which is exactly what flattening to a list throws away. So `usageLine` walks
`CmdSpec.doc` for alternations, renders each as a choice, and hands everything
outside one to the flat treatment items have always had.
-/

namespace ArgParse.Doc

open ArgParse.Spec

/-- Pad `s` on the right to at least `width` characters. -/
def padTo (width : Nat) (s : String) : String :=
  if s.length < width then s ++ String.ofList (List.replicate (width - s.length) ' ') else s

/-- Bracket `s` when the item it describes may be omitted. -/
@[inline] def bracketIf (optional : Bool) (s : String) : String :=
  if optional then "[" ++ s ++ "]" else s

/-- Ellipsis marker for items that may repeat. -/
@[inline] def repeatMark (arity : Arity) : String :=
  match arity with
  | .many | .some => "..."
  | _ => ""

/-- An item's bare text: its lexeme, and its metavar where it takes a value.
No bracketing and no ellipsis — both of those are decided by what encloses it. -/
def itemText (item : ItemSpec) : String :=
  match item.kind with
  | .flag => item.synopsisLexeme
  | .option => s!"{item.synopsisLexeme} {item.metavar}"
  | .positional => item.metavar

/-- How an item appears inside a usage synopsis, before any bracketing.

Split out because an item inside a choice is bracketed by the choice, not by
itself: `(-a | -b)` rather than `([-a] | [-b])`. -/
def itemSynopsisCore (item : ItemSpec) : String :=
  itemText item ++ repeatMark item.arity

/-- How an item appears inside a usage synopsis. -/
def itemSynopsis (item : ItemSpec) : String :=
  bracketIf (!item.required) (itemSynopsisCore item)

/-! ### Repetition

An item's own `Arity` says whether *it* may be given more than once, which is
what the repeating builders (`options`, `args`) set. A `many` node says the same
thing about whatever it wraps, which is what `P.many` and `P.some` build, and it
is a stronger statement: it fixes the bracketing too, because a `P.many` is
satisfied by nothing and a `P.some` is not. So a `many` above an item overrides
the item's own arity rather than adding to it. -/

/-- How many times the synopsis says something may appear. -/
inductive Repetition where
  /-- Not under a `many`: the item's own arity has the last word. -/
  | once
  /-- Under a `P.many`: any number of times, including none. -/
  | zeroOrMore
  /-- Under a `P.some`: any number of times, but at least one. -/
  | oneOrMore

/-- Descend under a `many` node. Nesting weakens: `P.many (P.some p)` is still
satisfied by nothing, so it reads as zero-or-more. -/
def Repetition.under : Repetition → Bool → Repetition
  | .once, true => .oneOrMore
  | .oneOrMore, true => .oneOrMore
  | _, _ => .zeroOrMore

/-- Wrap rendered text in the brackets and ellipsis a repetition calls for.
`whenOnce` renders the case where nothing above the text repeats it. -/
def Repetition.wrap (rep : Repetition) (text : String) (whenOnce : String) : String :=
  match rep with
  | .once => whenOnce
  | .zeroOrMore => "[" ++ text ++ "...]"
  | .oneOrMore => text ++ "..."

/-! ### Choices

An `alt` node with two or more branches that document something is a choice the
synopsis shows as such. An `alt` with one is the optionality spelling —
`alt [d, none]`, what `P.optional` and the defaulted builders produce — and its
item is bracketed by `ItemSpec.required` like any other, exactly as before.

A `none` branch alongside two real ones means the whole choice may be omitted:
`optional (a <|> b)` normalizes to `alt [a, b, none]` and reads `[-a | -b]`.

Optionality is therefore spelled twice — `ItemSpec.required` and the shape — and
the two are read in different places: `required` for a lone item, the shape only
once an alternation has two or more real branches. They cannot contradict each
other here, but a builder that set one without the other would be a bug no type
catches. Layer 3 sets both from the same argument, which is what keeps them
together. -/

/-- An alternation the synopsis renders as a choice between branches. -/
structure Choice where
  /-- Whether the alternation had a `none` branch, making the choice omissible. -/
  omissible : Bool
  /-- How often the whole group may appear, from the `many` nodes above it.
  Repetition of a single item can travel on its `Arity`; this cannot, because
  it is a property of the group rather than of any item in it. -/
  repetition : Repetition
  /-- The items of each branch that documents something, in order. -/
  branches : List (List ItemSpec)

/-- The branches of an alternation that document something. -/
@[inline] def realBranches (ds : List Doc) : List Doc :=
  ds.filter (fun d => !d.isNone)

mutual

/-- Every choice in a document, in order, carrying whether a `many` above it
makes the group repeat. An alternation with fewer than two real branches is not
a choice: it is how optionality is spelled. -/
def choicesFrom (rep : Repetition) : Doc → List Choice
  | .item _ => []
  | .none => []
  | .many d atLeastOne => choicesFrom (rep.under atLeastOne) d
  | .seq ds => choicesListFrom rep ds
  | .alt ds =>
      let real := realBranches ds
      if real.length < 2 then choicesListFrom rep ds
      else [{ omissible := real.length != ds.length
            , repetition := rep
            , branches := real.map Doc.items }]

/-- `choicesFrom` over a list of documents. -/
def choicesListFrom (rep : Repetition) : List Doc → List Choice
  | [] => []
  | d :: rest => choicesFrom rep d ++ choicesListFrom rep rest

end

/-- Every choice in a document, in order. -/
@[inline] def choices (d : Doc) : List Choice := choicesFrom .once d

/-- An item the synopsis renders on its own, with the repetition the `many`
nodes above it give it. -/
structure Loose where
  /-- The item itself. -/
  item : ItemSpec
  /-- How often it may appear, from the `many` nodes above it. -/
  repetition : Repetition

mutual

/-- The items the synopsis renders one at a time: everything a choice does not
already account for, each carrying what encloses it. -/
def looseFrom (rep : Repetition) : Doc → List Loose
  | .item i => [{ item := i, repetition := rep }]
  | .none => []
  | .many d atLeastOne => looseFrom (rep.under atLeastOne) d
  | .seq ds => looseListFrom rep ds
  | .alt ds =>
      let real := realBranches ds
      if real.length < 2 then looseListFrom rep ds else []

/-- `looseFrom` over a list of documents. -/
def looseListFrom (rep : Repetition) : List Doc → List Loose
  | [] => []
  | d :: rest => looseFrom rep d ++ looseListFrom rep rest

end

/-- Every item the synopsis renders on its own, in order. -/
@[inline] def loose (d : Doc) : List Loose := looseFrom .once d

/-- How a loose item appears. A `many` above it decides both its ellipsis and
its bracketing; without one, its own arity and `required` flag do. -/
def looseSynopsis (l : Loose) : String :=
  l.repetition.wrap (itemText l.item) (itemSynopsis l.item)

/-- One branch of a choice: its visible items, space-joined and unbracketed. -/
def branchSynopsis (items : List ItemSpec) : String :=
  String.intercalate " " ((visibleItems items).map itemSynopsisCore)

/-- A choice, bracketed when it may be omitted and parenthesised when it may
not. A branch whose items are all hidden contributes nothing, so a choice that
is entirely hidden renders as nothing at all. -/
def choiceSynopsis (c : Choice) : String :=
  match (c.branches.map branchSynopsis).filter (fun s => s != "") with
  | [] => ""
  | [only] => c.repetition.wrap only (bracketIf c.omissible only)
  | parts =>
      let body := "(" ++ String.intercalate " | " parts ++ ")"
      c.repetition.wrap body
        (if c.omissible then "[" ++ String.intercalate " | " parts ++ "]" else body)

/-- The label shown in the left column of a help or man entry: every accepted
lexeme, with the metavar where the item takes a value. -/
def itemLabel (item : ItemSpec) : String :=
  let names := String.intercalate ", " item.displayLexemes
  match item.kind with
  | .flag => names
  | .option => s!"{names} {item.metavar}"
  | .positional => item.metavar

/-- Trailing notes for an item: its default and its admissible values. -/
def itemNotes (item : ItemSpec) : String :=
  let parts :=
    (item.defaultText.toList.map fun d => s!"[default: {d}]") ++
      (item.choices?.toList.map fun cs => s!"[choices: {String.intercalate "|" cs}]")
  match parts with
  | [] => ""
  | _ => " " ++ String.intercalate " " parts

/-- Usage synopsis for one command, reached by `path`.

Options are summarised as `[OPTIONS]` once there are more than three of them,
which is the usual convention and keeps the line readable. -/
def usageLine (path : List String) (cmd : CmdSpec) : String :=
  let visible := (loose cmd.doc).filter (fun l => !l.item.hidden)
  let positionals := visible.filter (fun l => l.item.kind == .positional)
  let switches := visible.filter (fun l => l.item.kind != .positional)
  let switchPart :=
    if switches.length > 3 then ["[OPTIONS]"]
    else switches.map looseSynopsis
  let choicePart := ((choices cmd.doc).map choiceSynopsis).filter (fun s => s != "")
  let verbPart := if cmd.subs.isEmpty then [] else ["<COMMAND>"]
  let words :=
    path ++ switchPart ++ choicePart ++ positionals.map looseSynopsis ++ verbPart
  "  " ++ String.intercalate " " words

end ArgParse.Doc

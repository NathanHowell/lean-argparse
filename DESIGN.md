# lean-argparse — Target Design

This document describes the desired end state of the library. It is not a
migration plan; it is the shape the library converges to. Present tense
throughout, even where the code does not yet match.

## The one-sentence design

The runtime parser stays an opaque function; every public combinator pairs it
with an untyped doc structure that describes it, and the pair moves through one
`Applicative` in lockstep — so `--help`, usage, and completion are rendered by
the library from data that cannot drift from behavior, because the two sides
are only ever zipped together inside the builders.

## Why this shape

optparse-applicative gets library-owned help by making the parser itself
traversable data (a free applicative) and erasing the types when it walks it.
The load-bearing property is not "the parser is data" — it is "the thing help
reads is glued to the thing that parses, at the definition site, with no
second declaration to keep in sync."

That property is reachable without a free structure. Help only ever reads the
*static skeleton* of the parser: which items exist, how they compose, which
branches alternate. A product of two applicatives — the proven function parser
on one side, a payload-free doc tree on the other — carries exactly that
skeleton. It costs no universe bump, no interpreter, and no restatement of the
proof suite, which remains stated over `State → Result α`.

The dynamic traversal a free applicative would add corresponds to structure
that depends on already-parsed values — the `BindP` case, where even Haskell's
help degrades. Nothing in this library wants it.

## The layers

### Layer 1 — runtime core (`ArgParse.Core`)

```lean
abbrev Parser (α : Type) := State → Result α
```

The function parser, the order-insensitive scanning layer, and the entire
proof suite (progress, scan/front-of-stream agreement, merge soundness) live
here, stated over the function type. This layer is the foundation and is
deliberately boring: nothing above it changes its statements.

`Core.Subcommand` and `Core.subcommand` remain the dispatch primitive that
`Cmd.toParser` (Layer 4) is built from.

### Layer 2 — the paired applicative (`ArgParse.P`)

The heart of the library, and the only carrier applications touch for items:

```lean
inductive Doc where
  | item (i : ItemSpec)   -- one flag / option / positional
  | seq  (ds : List Doc)  -- applicative composition
  | alt  (ds : List Doc)  -- <|> ; renders as (-a | -b)
  | many (d : Doc)        -- repetition
  | none                  -- pure — contributes nothing to help

structure P (α : Type) where
  doc : Doc
  run : Parser α
```

Instances are one line each and total:

- `Functor`: maps `run`, leaves `doc` untouched.
- `Applicative`: composes `run`; `Doc.seq`s the docs.
- `Alternative`: alternates `run`; `Doc.alt`s the docs.
- `P.many` / `P.optional`: wrap `run` with the corresponding scanner
  combinator; wrap `doc` in `Doc.many` / mark the item optional.

`Doc` is the static skeleton of a free applicative with the payloads deleted —
which is everything the renderers ever read. `P` is introspectable to exactly
the depth help needs and opaque below that.

`Doc` normalization (flattening nested `seq`s, dropping `none`) is a function
on `Doc` alone, so rendering quality never touches parsing.

### Layer 3 — builders (`ArgParse.Builder`)

The only place `doc` and `run` are zipped together. Named and default
arguments replace Haskell's `Mod` monoid — the `Mod` machinery exists only
because Haskell lacks keyword arguments, and Lean doesn't:

```lean
def strOption (long : String) (short : Option Char := none)
    (metavar : String := "ARG") (help : String := "")
    (default? : Option String := none) : P String

def option [ArgRead α] (long : String) (short : Option Char := none)
    (metavar : String := "ARG") (help : String := "")
    (default? : Option α := none) : P α

def flag (long : String) (short : Option Char := none)
    (help : String := "") : P Bool

def positional (name : String) (metavar : String := name)
    (help : String := "") : P String
```

Each builder constructs its `ItemSpec` and its scanning parser *from the same
arguments in one body*. Divergence between what help says and what the scanner
accepts is expressible in exactly one file — this one — where it is proven
away (Layer 6), not policed at call sites.

Typed values come from an `ArgRead α` class (`String → Except String α`) with
instances for the obvious types; user types opt in with one instance.

### Layer 4 — the command tree (`ArgParse.Cmd`)

Commands are first-token dispatch over named alternatives, so the tree is
data, walkable to arbitrary depth, with opaque `P` leaves:

```lean
inductive Cmd (α : Type) where
  | leaf (name : String) (meta : Meta) (p : P α)
  | node (name : String) (meta : Meta)
         (globals : P (α → α)) (subs : List (Cmd α))
```

- `Cmd.leaf name meta (Config.mk <$> …)` is `command name (info parser
  (progDesc …))`: name, description, and typed payload in one expression.
  There is no second declaration and nothing to keep in sync.
- `node` carries its own `P (α → α)` so interior commands can own global
  flags (`tool --verbose sub …`); the parsed globals are applied to the
  leaf's result. A node with no globals passes `pure id`.
- Typed verbs are ordinary `Functor` use: a `Cmd AppCommand` whose leaves map
  into the constructors of the application's own inductive. No stringly
  recovery step exists anywhere.

Two total functions interpret the tree:

```lean
def Cmd.toParser : Cmd α → Parser α     -- built from Core.subcommand
def Cmd.toCmdSpec : Cmd α → CmdSpec     -- feeds the doc renderers
```

`toParser` reuses the proven Layer-1 combinators; `toCmdSpec` erases the
payload type and hands the renderers the same recursive `CmdSpec` they already
consume.

### Layer 5 — the runner (`ArgParse.Exec`)

The `execParser` equivalent, and the reason Layers 2 and 4 must be data:

```lean
def exec (app : Cmd α) (argv : List String) : ExecResult α
-- ExecResult: parsed value | help text to print | error + usage
```

The runner — not the application — owns:

- `-h` / `--help` at every level, injected into the scan itself; on hit it
  descends the `Cmd` tree along the matched path and renders that node's help
  from its `Doc` / `CmdSpec`.
- `--version`.
- Usage synopsis and error rendering ("unknown option `--frob`; did you mean
  `--from`?"), derived from the same data.
- Shell completion, derived by walking `Cmd` + `Doc`: complete verb names at
  nodes, item keys at leaves.

Applications contain zero help code. That is the acceptance criterion for the
whole design: if an application needs to render its own help, a layer below
has failed.

### Layer 6 — the theorems (`ArgParse.Correspondence`)

What was a sync guard becomes a lemma. Because `doc` and `run` are constructed
together, the correspondence is provable per builder and lifted by induction
over `Doc` and `Cmd`:

- **Item agreement.** Every key the scanner of `p.run` accepts appears as an
  item in `p.doc`, and every non-hidden item in `p.doc` is accepted — proven
  once per builder in Layer 3, preserved by the `P` instances.
- **Verb agreement.** `(Cmd.toCmdSpec c)` lists exactly the names on which
  `(Cmd.toParser c)` dispatches, at every depth.
- **Help totality.** Rendering is total on every constructible `Cmd`/`Doc`.

These sit on top of, and do not disturb, the existing Layer-1 suite.

### Layer 7 — the macro front end (optional, last)

A `deriving`-style handler or small DSL that takes an application's

```lean
structure GreetConfig where
  /-- Name to greet. -/
  name : String
  /-- Shout the greeting. -/
  loud : Bool := false
```

and generates the `P GreetConfig` — field names to long options, doc-strings
to help text, defaults carried over. It generates *calls to Layer 3*, never
bypassing it, so everything it emits inherits the correspondence theorems.
This is the single-declaration front end no Haskell design can offer; it is
sugar over the design, not part of it.

## What the design deliberately excludes

- **A free-applicative `Parser`.** Universe bump, an interpreter, and a full
  restatement of the proof suite, to gain dynamic introspection that no
  renderer reads. The `Doc` skeleton is the free applicative's shadow, at the
  price of a page.
- **Spec-elaboration as the front door.** `AppSpec → Parser Partial` inverts
  the dependency (data elaborated into behavior) but returns a stringly,
  verb-blind carrier. A dependently-typed repair (`parse : (s : CmdSpec) →
  Parser (Interp s)`) is possible and Lean-native, but the computed
  nested-product result types are hostile to consume; the paired design
  reaches the same no-drift guarantee with ordinary applicative ergonomics.
  `CmdSpec` survives — demoted from source of truth to *render model*,
  produced by `Cmd.toCmdSpec`, never written by hand.
- **A monadic layer.** `P` is applicative-only. Structure hidden behind
  functions cannot be documented; the library refuses to offer the
  constructor that would create the blind spot.

## What an application looks like

```lean
inductive AppCommand where
  | greet  (cfg : GreetConfig)
  | repeat (cfg : RepeatConfig)

def app : Cmd AppCommand :=
  .node "myapp" { descr := "Demo CLI" } (pure id)
    [ .leaf "greet" { descr := "Greet someone" }
        (AppCommand.greet <$> greetP)
    , .leaf "repeat" { descr := "Repeat a phrase" }
        (AppCommand.repeat <$> repeatP) ]

def main (argv : List String) : IO UInt32 :=
  ArgParse.run app argv fun
    | .greet cfg  => …
    | .repeat cfg => …
```

Each verb appears exactly once. Help, usage, completion, and version handling
are nowhere in the file. The compiler checks the payloads; the library proves
the help.

---
name: lean
description: Lean 4 gotchas hit in this repo, pinned to toolchain v4.32.2 - termination failures on recursive structures, deriving-handler gaps, String.Slice return types, metaprogramming around structure defaults, and instance-resolution transparency. Use when a Lean build fails with a confusing or misleading error, when writing recursive functions over tree-shaped data, when writing a deriving handler or macro, or when designing a type wrapper meant to be dispatched on by instance resolution.
---

# Lean 4 gotchas (toolchain v4.32.2)

Scars, not a language reference. Each entry is a symptom, the real cause, and
the fix that worked here. Version-specific entries are marked; this repo pins
`leanprover/lean4:v4.32.2` in `lean-toolchain`, so re-check them on a bump.

## First move when stuck: probe, don't guess

Every entry below was settled by writing a throwaway file and running it, which
beats reasoning about what Lean *should* do:

```sh
cat > Probe.lean <<'PROBE'
import ArgParse
example : ... := by ...
PROBE
lake env lean Probe.lean
```

For anything about the environment - what a constant's type or value really is,
what fields or defaults a structure has - use `run_cmd` and print it:

```lean
run_cmd do
  let env ← getEnv
  logInfo m!"{(env.find? `Foo.bar).get!.type}"
```

Delete `Probe.lean` when done; it is not in `.gitignore`.

## Termination

**`fail to show termination` / "Could not find a decreasing measure" on a
function over a tree.** A `structure` whose field recurses through
`List Self` admits no structural measure, so every function over it wants to be
`partial`. Make it an `inductive` with one constructor and hand-write the
projections:

```lean
inductive CmdSpec where
  | mk (name : String) (args : List ItemSpec) (subs : List CmdSpec)
```

Then **match on the constructor, not on projections**. `cmd.subs` in a recursive
call is opaque to the termination checker; `| .mk _ _ subs => ... f subs` is
structural. This bit twice - once on the type, once on a renderer that still
used `cmd.subs` after the type was fixed.

**Two arguments where neither always decreases.** Recursion that either descends
a tree *or* consumes a token, depending on the branch, has no single structural
measure. Recurse on explicit fuel bounded by the input length rather than
fighting it - it is total, honest, and one line.

**Structural recursion inside `do`-notation.** When a structurally recursive
function lives in a monad (returning `Except`, say), `do` around the recursive
call can hide the decreasing argument. Elaborate the `match` manually, or supply
`termination_by`.

## Deriving

**"None of the deriving handlers for class `DecidableEq` applied".** No handler
covers an inductive that nests through `List`. `Repr` and `Inhabited` work;
`DecidableEq` and `BEq` do not. Either write the instance by hand or state the
properties you need over a derived list (`Doc.items`) instead of over the type.

**Structure defaults are stored wrapped.** `Foo.field._default` holds `id true`,
not `true`. Any check on a default's value must `whnf` first, or it will silently
not match.

**Defaults that depend on earlier fields have function type.** `b : Nat := a + 1`
gives `Foo.b._default : Nat → Nat`, not `Nat`. Check `.type.isForall` and reject
or handle it; do not assume a default is a value.

**[v4.32.2] Referencing a `_default` constant in generated code crashes the
backend.** Emitting `Foo.field._default` into a compiled term panics with
`ExplicitBoxing ... unknown join point`. Delaborate the *value* instead:

```lean
let valueStx ← liftTermElabM (PrettyPrinter.delab value)
```

## Instances

**A wrapper meant to be dispatched on must be a `structure`, not an `abbrev`.**
Instance resolution matches up to *reducible* transparency. An `abbrev` unfolds
and becomes invisible to it; a `structure` stays opaque, so you can write
instances that dispatch on the wrapper and read its phantom parameters:

```lean
structure Arg (α : Type) (o : Opts) where val : α   -- dispatchable
abbrev  Arg (α : Type) (o : Opts) := α              -- vanishes
```

The cost is real unwrapping at use sites (`x.val`); pass `Repr`, `DecidableEq`,
`ToString`, `Inhabited`, and a `CoeOut` through to soften it.

**Dependent return types do not reduce through a plain `def`.** A function whose
result type is `match spec.arity with ...` only computes when `spec` is
reducible. Mark test fixtures `abbrev`, or expose a non-dependent API
(`collectOptionScanValues`) for callers that need to pattern-match the result.

## Syntax and stdlib

**`unexpected token 'section'; expected identifier`.** `section` is reserved and
cannot name a `def`. Same family: `end`, `open`, `where`, `deriving`.

**[v4.32.2] Several `String` methods return `String.Slice`, not `String`.**
`String.drop` and `String.trimAsciiEnd` among them - append `.toString`.
Deprecations in this toolchain: `String.mk` → `String.ofList`,
`String.trimRight` → `String.trimAsciiEnd`.

**`|>.` inside parentheses parses badly.** `(text.splitOn s |>.length == 2)`
fails; write `((text.splitOn s).length == 2)`.

**`expectTrue (a = b)` type errors in `Bool` position.** `=` is `Prop`. Use `==`
where a `Bool` is wanted.

## Before committing

```sh
lake build && lake test && lake lint
```

All three, every time. `lake build` succeeding does not mean the test executable
still compiles - it is a separate target and will go stale silently.

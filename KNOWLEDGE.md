🦾 Lean4 — ultra-condensed ref

🧠 Core: dep-TT (CIC); pure/strict FP; Curry–Howard; Sorts `Prop`/`Type u`/`Sort u`; Prop impredicative & proof-irrelevant; pipeline parse→macro→elab(cmd/term/tac)→kernel→IR/native; InfoTree/InfoView; Pratt parser; term-style vs `by` tactic-style.

🔤 Univ: levels u v w; `max/imax/succ`; cumulativity (Sort u↪Sort u+1); `Prop = Sort 0`; `Type u = Sort (u+1)`; Π typing→`Sort (imax i j)`; universe params `.{u}`; defeq β δ ζ ι η.

⛳ Binders & implicits: `(x:α)` explicit; `{x}` implicit; `⦃x⦄` strict-implicit; `[x:α]` instance-implicit; `_`/`?m` holes; casts `(e : T)`; `@f` exposes implicits; section/`variable` params auto-insert; implicit lambdas & named args.

🏗 Decls & recursion: `def`/`abbrev`/`opaque`/`theorem`/`lemma`/`example`/`axiom`; `unsafe`/`partial`/`noncomputable`; pattern-matching; structural & well-founded (`WellFounded.fix`) recursion; `mutual`; equation compiler; `termination_by`/`decreasing_by`/`wfRel`; `where` locals.

📦 Modules & scope: file `A/B/C.lean`≡`A.B.C`; `prelude`; `import`; `namespace`/`section`; `_root_`/`protected`/`private` (hashed `_private`)/`export`/`alias`; `open`/`open scoped`/`localized`; `initialize`/`run_cmd`; `set_option …` & `trace.*` (e.g., `trace.class_instances`, `trace.simp`).

✍️ Terms: `λ`/`Π`/`∀`; `let`/`where`; `if`/`match`/`dite`; numerals via `OfNat`/`OfInt`/`OfScientific`; mutual defs; dot-notation & leading `.ctor`; `show` annotations.

🧱 Inductives & records: inductive families/enums; recursors `casesOn`/`rec`; positivity; nested/mutual; `structure`→ctor/projections/recursor; deriving `Repr` `DecidableEq` `Inhabited` `BEq` `Hashable` `Ord`; object literal `{ … }`; record update `{ s with fld := v }`.

📚 Types & data: `Nat` `Int` `UInt*` `USize` `BitVec` `Float` `Char` `String`; `Prod` (`α × β`, `.fst/.snd`) `Sum` `Sigma` `Subtype`; `Option` `Except` `List` `Array` `RBMap` `HashMap` `Fin`.

🧩 Typeclasses & coercions: `class`/`instance`/`deriving instance`; search via `inferInstance`; `local instance`; priorities; numeric/ordering via classes (`OfNat`, `HAdd`, …); `Coe`/`CoeFun`/`CoeSort`/`CoeHead`/`CoeTail`/`CoeTrans`/`CoeSub`.

⚖ Eq & rewrite: `=`/`rfl`; `Eq.refl/symm/trans`; `congrArg`; `Eq.rec`/`Eq.subst`; `HEq`; `rw`/`rw ←`/`▸`; `calc`; `Subtype {x // p}`; `Sigma ⟨v,p⟩`.

🔢 Logic: `True`/`False`; `¬ ∧ ∨ → ↔`; `∀`/`∃` (`Exists.intro`/`cases`); `Decidable p`; constructive core; classical via `open Classical`; `noncomputable` when extracting data.

🛠 Tactics: `intro[s]/refine/apply/exact/have/rename/generalize/subst`; `constructor/cases/cases’/induction/split/left/right/exists/use/by_cases/by_contra`; `rw`/`nth_rewrite`; `simp`/`simp_all`/`dsimp`; `change/convert/symm/trans`; `injection`; `ac_nf`; `solve_by_elim`; `contrapose/exfalso/trivial/contradiction`; control `first|` `all_goals` `any_goals` `focus` `repeat` `try` `<;>` `case`; `conv`; `calc`.

🧹 Simplifier: `[simp]`-driven; `simp`/`simp at *`; `simp [defs]`/`simp only [l*]`/`simp [← l]`; `[local simp]`; `simp?`; tracing & configs (e.g., `contextual:=true`); avoid loops via priorities; propositional simp (∧/∨/↔).

🔣 Syntax & macros: `syntax`/`notation`/mixfix; `macro`/`macro_rules`; `elab` (term/command/tactic); scoped/localized notation; quoting `` `(…) `` with antiquotations; hygiene; pretty-printing.

📦 Monads & `do`: `Functor`/`Applicative`/`Monad`; transformers `ReaderT` `StateT` `ExceptT` `OptionT` `WriterT`; `Alternative`/`ForIn`; `do` sugar (`let`/`match`/`if`/`for`/`while`/`try`/`catch`/`finally`); `guard`; nested actions `← e`; early return; `mut`.

🖥 IO & conc: `IO`/`EIO`; `def main : IO Unit` (or `IO UInt32`); FS `IO.FS`/`System.FilePath`; `IO.getStdin/Stdout/Stderr`; `Process.spawn`; `Task`/`Ref`/`MVar`; `IO.asTask/await/sleep`; `IO.toEIO`.

⚙ Runtime/FFI/build: RC+boxing (scalars unboxed); multi-threaded; `@[extern]`/`@[export]`/`@[inline|noinline]`; Lake (`package`/`lean_lib`/`lean_exe`/`lean_shared`; `require`; `build`/`update`/`exe`/`env`/`test`); Elan toolchains.

🏷 Attr: `[simp]` `[ext]` `[reassoc]` `[congr]` `[inline|noinline]` `[reducible|semireducible|irreducible]` `[priority:=n]` `[deprecated]` `[export]`.

🧪 Commands & tracing: `#check` `#print` `#eval` (VM) `#reduce` (kernel) `#simp`/`#simp?` `#synth` `#guard`/`#guard_hyp` `#guard_msgs` `#help`; `unfold`; `decide` to discharge decidable goals; `set_option`/`trace.*` for pp/instances/simp/unification.

🧭 Debug & errors: message classes parse/scope/elab/kernel/termination/macro/tactic; editor goal panes; `#print` artifacts; `#guard_msgs` to assert diagnostics.

✅ Tips: use `abbrev` for aliases; `opaque` to hide impl; keep instances local/scoped; tune priorities sparingly; directional `[simp]` lemmas; add `termination_by` early; prefer class-based numerals; keep term-style when simple; isolate classical with `local open Classical`; prefer `Fin`/subtypes for bounds; safe indexing `a[i]?`/`a[i]!`/`a[i]'h`.

➕ Extras: `conv` mode (navigate/lhs/rhs/`congr`/`intro`/pattern-rewrite); axioms `funext` `propext` `Choice` & `Quot`; `noncomputable` data via choice; `#reduce` vs `#eval` differences (casts/extensionality can block kernel eval); Unicode input via `\alpha`, `→`.
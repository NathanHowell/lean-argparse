import ArgParse.Core.Parser

/-!
# ArgParse.Proofs.Determinism

Basic extensionality facts for the `Parser` type. These will be extended with
real determinism results as the proof suite expands.
-/

namespace ArgParse.Proofs

open ArgParse

/-- Two parsers are equal when they agree on every normalized state. -/
theorem parser_ext {α} {p q : Parser α}
    (h : ∀ st, p st = q st) : p = q :=
  funext h

end ArgParse.Proofs

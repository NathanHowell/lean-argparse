import Argparse.Core.Parser

/-!
# ArgParse.Proofs.Laws

Placeholder statements for Functor/Applicative/Alternative laws.
-/

namespace ArgParse.Proofs

open ArgParse

/-- Placeholder: parser `map id = id`. -/
theorem parser_map_id_placeholder {α} (p : Parser α) : p = p := by
  rfl

/-- Placeholder: parser `pure` is left identity for `seq`. -/
theorem parser_seq_pure_placeholder {α β} (f : α → β) (pa : Parser α) :
    Parser.seq (Parser.pure f) (fun _ => pa) = Parser.map f pa := by
  rfl

end ArgParse.Proofs

namespace Argparse
namespace Native

/-- Classifies the kind of parser failure encountered by the native interpreter. -/
inductive ErrorCode where
  | missing
  | invalid
  | unexpected
  deriving DecidableEq, Repr

/-- Proof-oriented error payload separate from human rendering. -/
structure Error where
  code : ErrorCode
  subject? : Option String := none
  detail? : Option String := none
  deriving DecidableEq, Repr

end Native
end Argparse

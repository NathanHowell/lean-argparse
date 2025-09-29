import ArgParse.Core.Normalize

/-!
# ArgParse.Proofs.Sentinel

Basic properties of the normalization pass with respect to the `--` sentinel.
-/

namespace ArgParse.Proofs

open ArgParse
open ArgParse.Core

/-- If a sentinel occurs in the raw tokens, normalization exposes it as the
boundary between `pre` and `post`. -/
theorem sentinel_present_normalize {tokens : Tokens}
    (h : "--" ∈ tokens) :
    tokens = (normalize tokens).pre ++ "--" :: (normalize tokens).post := by
  simpa using ArgParse.Core.normalize_sentinel_factor (tokens := tokens) h

/-- When no sentinel is present, normalization leaves the `post` portion empty. -/
theorem sentinel_absent_post_nil {tokens : Tokens}
    (h : "--" ∉ tokens) : (normalize tokens).post = [] := by
  simpa using ArgParse.Core.normalize_post_nil_of_not_mem (tokens := tokens) h

end ArgParse.Proofs

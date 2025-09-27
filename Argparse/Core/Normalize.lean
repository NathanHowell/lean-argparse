/-!
# ArgParse.Core.Normalize

Normalization pass that splits tokens on the first `--` sentinel.
-/

import Argparse.Core.Types

namespace ArgParse.Core

open ArgParse

/-- Auxiliary helper: split the token stream at the first `--` sentinel. -/
private def splitOnSentinel : Tokens → Tokens × Tokens
  | [] => ([], [])
  | "--" :: rest => ([], rest)
  | token :: rest =>
      let (pre, post) := splitOnSentinel rest
      (token :: pre, post)

/-- Build the initial parser state from raw argv tokens. -/
def normalize (tokens : Tokens) : State :=
  let (pre, post) := splitOnSentinel tokens
  { pre := pre, post := post, cursor := 0 }

end ArgParse.Core

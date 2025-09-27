import Argparse.CLI.Print
import Argparse.Examples.GitLike

/-!
# ArgParse.Tests.Golden

Placeholder golden tests for rendering helpers.
-/

namespace ArgParse.Tests

open ArgParse.CLI
open ArgParse.Examples

#guard (renderMan GitLike.spec |>.isEmpty = false)
#guard (renderCompletions GitLike.spec |>.isEmpty = false)

end ArgParse.Tests

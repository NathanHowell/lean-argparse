/-!
# ArgParse.Tests.Unit

Placeholder unit tests targeting the new scaffolds.
-/

import Argparse.CLI.Print
import Argparse.Examples.GitLike
import Argparse.Examples.Xargs0

namespace ArgParse.Tests

open ArgParse.Examples
open ArgParse.CLI

#guard (Xargs0.help.isEmpty = false)
#guard (GitLike.help.isEmpty = false)

end ArgParse.Tests

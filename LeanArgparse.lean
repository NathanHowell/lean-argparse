-- This module serves as the root of the `LeanArgparse` library.
-- Import modules here that should be built as part of the library.
import LeanArgparse.Basic

namespace Argparse

export LeanArgparse
  ( Parser ParseError ParseErrorKind ParserFailure ParserResult ParserInfo
    OptionSpec FlagSpec ArgumentSpec Subcommand SubcommandSpec ValueReader
    option optionWith strOption natOption intOption flag flag' switch argument rawArgument subcommand )

export LeanArgparse.Parser (optional withDefault failure orElse many many1 some some1 choice)

export LeanArgparse.ParserInfo (exec renderHelp renderFailure)
export LeanArgparse.ParserInfo (renderBashCompletion renderZshCompletion renderFishCompletion)

export LeanArgparse.ValueReader (map nat int)

namespace OptionSpec
  export LeanArgparse.OptionSpec (Mod base build applyMods long short setMetavar help default showDefault)
end OptionSpec

namespace FlagSpec
  export LeanArgparse.FlagSpec (Mod base build applyMods long short help)
end FlagSpec

namespace ParserInfo
  export LeanArgparse.ParserInfo (InfoMod withProgName withHeader withProgDesc withFooter applyMods build)
  namespace Completion
    export LeanArgparse.ParserInfo.Completion (Module renderWithModule)
  end Completion
  export LeanArgparse.ParserInfo (bashModule zshModule fishModule allCompletionModules)
end ParserInfo

end Argparse

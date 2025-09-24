-- This module serves as the root of the `LeanArgparse` library.
-- Import modules here that should be built as part of the library.
import LeanArgparse.Basic

namespace Argparse

export LeanArgparse
  ( Parser ParseError ParseErrorKind ParserFailure ParserResult ParserInfo
    OptionSpec FlagSpec ArgumentSpec Subcommand SubcommandSpec ValueReader
    option flag switch argument rawArgument subcommand )

export LeanArgparse.Parser (optional withDefault)

export LeanArgparse.ParserInfo (exec renderHelp renderFailure)

export LeanArgparse.ValueReader (map nat int)

end Argparse

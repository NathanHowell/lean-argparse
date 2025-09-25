import Lake
open Lake DSL

package «lean-argparse» where
  version := v!"0.1.0"

lean_lib Argparse

@[default_target]
lean_exe «lean-argparse» where
  root := `Main

@[test_driver]
lean_exe «lean-argparse-tests» where
  root := `Tests.Main
  supportInterpreter := false

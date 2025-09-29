import ArgParse

/-- Temporary executable while the CLI stabilises. -/
def main : IO Unit := do
  -- Touch the root module so Lake produces .olean files for linting.
  let _ := (())
  IO.println "argparse stubs ready"

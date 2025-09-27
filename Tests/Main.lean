import Argparse

open Argparse
open Argparse.Native
open Argparse.Native.Token
open Argparse.Native.TokenCursor
open Argparse.Native.Interpreter

namespace ArgparseTests

open ParsedName

#guard (
  let classified := Argparse.Native.classify ["--verbose"]
  classified.options.toList =
    [{ name := ParsedName.long "verbose", original := "--verbose", inlineValue? := none }]
    ∧ classified.positionals.isEmpty)

#guard (
  let classified := Argparse.Native.classify ["-n=3"]
  classified.options.toList =
    [{ name := ParsedName.short 'n', original := "-n=3", inlineValue? := some "3" }]
    ∧ classified.positionals.isEmpty)

#guard (
  let classified := Argparse.Native.classify ["--", "--count"]
  classified.options.isEmpty ∧ classified.positionals.toList = ["--count"])

#guard (
  let classified := Argparse.Native.classify ["-n5", "FILE"]
  classified.options.toList =
    [{ name := ParsedName.short 'n', original := "-n5", inlineValue? := some "5" }]
    ∧ classified.positionals.toList = ["FILE"])

private def containsSubstring (haystack needle : String) : Bool :=
  if needle.isEmpty then
    true
  else
    let target := needle.data
    let rec loop : List Char → Bool
      | [] => false
      | chars@(_ :: rest) =>
          if target.isPrefixOf chars then
            true
          else
            loop rest
    loop haystack.data

private structure NativeCfg where
  verbose : Bool
  count : Option String
  name : String
  deriving Repr, DecidableEq

private def verboseDoc : OptionDoc :=
  { long? := some "verbose", short? := none, help? := some "Enable verbose output", required := false }

private def countDoc : OptionDoc :=
  { long? := some "count", short? := none, metavar? := some "COUNT", help? := some "Number of repetitions", required := false }

private def nameDoc : PositionalDoc :=
  { metavar := "NAME", help? := none, required := true }

private def tokensOf (args : List String) : TokenCursor :=
  TokenCursor.fromArgv args

private def evalNative {α} (parser : Interpreter α) (args : List String) : Result α :=
  Interpreter.eval parser (tokensOf args)

private def nativeExample : Interpreter NativeCfg :=
  Interpreter.pure NativeCfg.mk
    <*> Interpreter.flag verboseDoc "verbose"
    <*> Interpreter.option countDoc "count"
    <*> Interpreter.positional nameDoc

#guard (match evalNative nativeExample ["Alice"] with
  | .ok cfg => decide (cfg = { verbose := false, count := none, name := "Alice" })
  | _ => False)

#guard (match evalNative nativeExample ["--verbose", "--count=3", "Bob"] with
  | .ok cfg => decide (cfg = { verbose := true, count := some "3", name := "Bob" })
  | _ => False)

#guard (match evalNative nativeExample ["--count=1", "--count=7", "Carol"] with
  | .ok cfg => decide (cfg = { verbose := false, count := some "7", name := "Carol" })
  | _ => False)

#guard (match evalNative nativeExample ["--count", "--flag"] with
  | .error err => err.code = .missing ∧ containsSubstring (err.subject?.getD "") "--count"
  | _ => False)

#guard (match evalNative (Interpreter.flag verboseDoc "verbose") ["--verbose=1"] with
  | .error err => err.code = .invalid
  | _ => False)

#guard (match evalNative (Interpreter.optional (Interpreter.positional nameDoc)) [] with
  | .ok Option.none => True
  | _ => False)

#guard (match evalNative (Interpreter.withDefault (Interpreter.positional nameDoc) "anon") [] with
  | .ok name => name = "anon"
  | _ => False)

end ArgparseTests

def main : IO Unit :=
  pure ()

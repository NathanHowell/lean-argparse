import Argparse

open Argparse
open Argparse.OptionSpec
open Argparse.FlagSpec
open Argparse.Completion
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

#guard
  let cursor := TokenCursor.fromArgv ["-n", "FILE"]
  let expected := TokenCursor.fromArgv ["-n"]
  match TokenCursor.takePositional? cursor with
  | Option.some (value, rest) =>
      value = "FILE"
        ∧ rest.options.toList = expected.options.toList
        ∧ rest.positionals.toList = expected.positionals.toList
  | Option.none => False

#guard
  let cursor := TokenCursor.fromArgv ["--verbose", "-x", "FILE"]
  let expected := TokenCursor.fromArgv ["-x", "FILE"]
  match TokenCursor.consumeFlag "verbose" cursor with
  | .ok (present, rest) =>
      present
        ∧ rest.options.toList = expected.options.toList
        ∧ rest.positionals.toList = expected.positionals.toList
  | _ => False

#guard
  let cursor := TokenCursor.fromArgv ["--verbose=1"]
  match TokenCursor.consumeFlag "verbose" cursor with
  | .error err => err.code = ErrorCode.invalid
  | _ => False

#guard
  let cursor := TokenCursor.fromArgv ["--count=1", "--count", "3", "FILE"]
  let expected := TokenCursor.fromArgv ["FILE"]
  match TokenCursor.consumeValue "count" cursor with
  | .ok (Option.some value, rest) =>
      value = "3"
        ∧ rest.options.toList = expected.options.toList
        ∧ rest.positionals.toList = expected.positionals.toList
  | _ => False

#guard
  let cursor := TokenCursor.fromArgv ["--count", "--other"]
  match TokenCursor.consumeValue "count" cursor with
  | .error err => err.code = ErrorCode.missing
  | _ => False

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

private def cursorDone (cursor : TokenCursor) : Bool :=
  cursor.options.isEmpty && cursor.positionals.isEmpty

private structure ExampleCfg where
  verbose : Bool
  count : Nat
  name : String
  deriving Repr, DecidableEq

private def verboseDoc : OptionDoc :=
  { long? := some "verbose", short? := some 'v', help? := some "Enable verbose output", required := false }

private def countDoc : OptionDoc :=
  { long? := some "count", short? := some 'n', metavar? := some "COUNT", help? := some "Number of repetitions", required := false }

private def nameDoc : PositionalDoc :=
  { metavar := "NAME", help? := none, required := true }

private def optionSubject (doc : OptionDoc) : Option String :=
  match doc.long?, doc.short? with
  | Option.some longName, _ => Option.some s!"--{longName}"
  | Option.none, Option.some shortName => Option.some s!"-{String.mk [shortName]}"
  | Option.none, Option.none => doc.metavar?

private def flagLongShort (longName : String) (shortName : Char) (doc : OptionDoc) : Interpreter Bool :=
  {
    grammar := Grammar.flag doc,
    eval := fun stream =>
      match TokenCursor.consumeFlag longName stream with
      | .ok (longPresent, stream') =>
        match TokenCursor.consumeFlag shortName stream' with
        | .ok (shortPresent, stream'') => .ok (longPresent || shortPresent) stream''
        | .error err => .error err
      | .error err => .error err
  }

private def optionLongShortNat (longName : String) (shortName : Char) (doc : OptionDoc) : Interpreter Nat :=
  {
    grammar := { usage := (Grammar.option doc).usage },
    eval := fun stream =>
      match TokenCursor.consumeValue longName stream with
      | .ok (longValue?, stream') =>
        match TokenCursor.consumeValue shortName stream' with
        | .ok (shortValue?, stream'') =>
            let value? := shortValue?.orElse fun _ => longValue?
            match value? with
            | Option.some raw =>
                match raw.toNat? with
                | Option.some n => .ok n stream''
                | Option.none => .error {
                    code := .invalid,
                    subject? := some s!"--{longName}",
                    detail? := some s!"Expected a natural number for {longName}, got '{raw}'"
                  }
            | Option.none =>
                .error {
                  code := .missing,
                  subject? := optionSubject doc
                }
        | .error err => .error err
      | .error err => .error err
  }

/--
Demonstrates how downstream parsers can migrate onto the native interpreter by
combining primitive parsers with the `Applicative`/`Alternative` helpers.
-/
private def nativeExample : Interpreter ExampleCfg :=
  let verbose := flagLongShort "verbose" 'v' verboseDoc
  let count := Interpreter.withDefault (optionLongShortNat "count" 'n' countDoc) 1
  let name := Interpreter.positional nameDoc
  Interpreter.pure ExampleCfg.mk
    <*> verbose
    <*> count
    <*> name

private def tokensOf (args : List String) : TokenCursor :=
  TokenCursor.fromArgv args

private def evalNative {α} (parser : Interpreter α) (args : List String) : Result α :=
  Interpreter.eval parser (tokensOf args)

#guard (match evalNative nativeExample ["Alice"] with
  | .ok cfg rest => decide (cfg = { verbose := false, count := 1, name := "Alice" })
      && cursorDone rest
  | _ => False)

#guard (match evalNative nativeExample ["--verbose", "-n", "3", "Bob"] with
  | .ok cfg rest => decide (cfg = { verbose := true, count := 3, name := "Bob" })
      && cursorDone rest
  | _ => False)

#guard (match evalNative nativeExample ["--count", "five", "Bob"] with
  | .error err => err.code = .invalid ∧
      containsSubstring (err.detail?.getD "") "Expected a natural number"
  | _ => False)

#guard (match evalNative nativeExample ["--count", "5"] with
  | .error err => err.code = .missing
  | _ => False)

#guard (match evalNative (Interpreter.positional { metavar := "NAME", help? := none, required := true }) ["Grace"] with
  | .ok name rest => decide (name = "Grace") && cursorDone rest
  | _ => False)

#guard (match evalNative (Interpreter.positional { metavar := "NAME", help? := none, required := true }) [] with
  | .error err => err.code = .missing
  | _ => False)

#guard (match evalNative (Interpreter.flag verboseDoc "verbose") ["--verbose", "--verbose"] with
  | .ok present rest => decide (present = true) && cursorDone rest
  | _ => False)

#guard (match evalNative (Interpreter.option countDoc "count") ["--count=9", "--count", "7"] with
  | .ok (Option.some value) rest => decide (value = "7") && cursorDone rest
  | _ => False)

#guard (match evalNative (Interpreter.option countDoc "count") [] with
  | .ok Option.none rest => cursorDone rest
  | _ => False)

#guard (match evalNative (Interpreter.many (Interpreter.positional { metavar := "ITEM", help? := none, required := false })) ["one", "two"] with
  | .ok values rest => decide (values = ["one", "two"]) && cursorDone rest
  | _ => False)

#guard (match evalNative (Interpreter.some (Interpreter.positional { metavar := "ITEM", help? := none, required := false })) [] with
  | .error err => err.code = .missing
  | _ => False)

#guard (match evalNative (Interpreter.optional (Interpreter.positional { metavar := "ITEM", help? := none, required := false })) [] with
  | .ok Option.none rest => cursorDone rest
  | _ => False)

private def exampleParser : Parser ExampleCfg :=
  (pure ExampleCfg.mk : Parser (Bool → Nat → String → ExampleCfg))
    <*> switch "verbose" (short? := some 'v')
    <*> Parser.withDefault
          (option {
            long? := some "count",
            short? := some 'n',
            metavar := "COUNT",
            reader := Argparse.ValueReader.nat,
            help? := some "Number of repetitions"
          })
          1
    <*> rawArgument "NAME"

private def exampleInfo : ParserInfo ExampleCfg := {
  progName := "example",
  parser := exampleParser
}

#guard (match Argparse.ParserInfo.exec exampleInfo ["Alice"] with
  | .success cfg => cfg = { verbose := false, count := 1, name := "Alice" }
  | _ => False)

#guard (match Argparse.ParserInfo.exec exampleInfo ["--verbose", "--count", "3", "Bob"] with
  | .success cfg => cfg = { verbose := true, count := 3, name := "Bob" }
  | _ => False)

#guard (match Argparse.ParserInfo.exec exampleInfo ["--count", "5"] with
  | .failure err => err.error.kind = .missing
  | _ => False)

end ArgparseTests

def main : IO Unit :=
  pure ()

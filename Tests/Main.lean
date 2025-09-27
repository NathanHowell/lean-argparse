import Argparse

open Argparse
open Argparse.OptionSpec
open Argparse.FlagSpec
open Argparse.Completion
open Argparse.Native
open Argparse.Native.Token
open Argparse.Native.TokenStream
open Argparse.Native.Interpreter

namespace ArgparseTests

#guard (Argparse.Native.classify ["--verbose"] =
  [ParsedToken.option { name := ParsedName.long "verbose", original := "--verbose", inlineValue? := none }])

#guard (Argparse.Native.classify ["-n=3"] =
  [ParsedToken.option { name := ParsedName.short 'n', original := "-n=3", inlineValue? := some "3" }])

#guard (Argparse.Native.classify ["--", "--count"] =
  [ParsedToken.positional "--count"])

#guard (Argparse.Native.classify ["-n5", "FILE"] =
  [ParsedToken.option { name := ParsedName.short 'n', original := "-n5", inlineValue? := some "5" },
   ParsedToken.positional "FILE"])

#guard
  let tokens := Argparse.Native.classify ["-n", "FILE"]
  let expected := Argparse.Native.classify ["-n"]
  match TokenStream.takePositional? (TokenStream.ofList tokens) with
  | Option.some (value, rest) => value = "FILE" ∧ rest.toList = expected
  | Option.none => False

#guard
  let tokens := Argparse.Native.classify ["--verbose", "-x", "FILE"]
  let expected := Argparse.Native.classify ["-x", "FILE"]
  match TokenStream.consumeFlag "verbose" (TokenStream.ofList tokens) with
  | .ok (present, rest) => present && rest.toList = expected
  | _ => False

#guard
  let tokens := Argparse.Native.classify ["--verbose=1"]
  match TokenStream.consumeFlag "verbose" (TokenStream.ofList tokens) with
  | .error err => err.code = ErrorCode.invalid
  | _ => False

#guard
  let tokens := Argparse.Native.classify ["--count=1", "--count", "3", "FILE"]
  let expected := Argparse.Native.classify ["FILE"]
  match TokenStream.consumeValue "count" (TokenStream.ofList tokens) with
  | .ok (Option.some value, rest) => value = "3" ∧ rest.toList = expected
  | _ => False

#guard
  let tokens := Argparse.Native.classify ["--count", "--other"]
  match TokenStream.consumeValue "count" (TokenStream.ofList tokens) with
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
      match TokenStream.consumeFlag longName stream with
      | .ok (longPresent, stream') =>
        match TokenStream.consumeFlag shortName stream' with
        | .ok (shortPresent, stream'') => .ok (longPresent || shortPresent) stream''
        | .error err => .error err
      | .error err => .error err
  }

private def optionLongShortNat (longName : String) (shortName : Char) (doc : OptionDoc) : Interpreter Nat :=
  {
    grammar := { usage := (Grammar.option doc).usage },
    eval := fun stream =>
      match TokenStream.consumeValue longName stream with
      | .ok (longValue?, stream') =>
        match TokenStream.consumeValue shortName stream' with
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

private def tokensOf (args : List String) : TokenStream :=
  TokenStream.ofList (Argparse.Native.classify args)

private def evalNative {α} (parser : Interpreter α) (args : List String) : Result α :=
  Interpreter.eval parser (tokensOf args)

#guard (match evalNative nativeExample ["Alice"] with
  | .ok cfg rest => cfg = { verbose := false, count := 1, name := "Alice" }
      ∧ rest.toList = []
  | _ => False)

#guard (match evalNative nativeExample ["--verbose", "-n", "3", "Bob"] with
  | .ok cfg rest => cfg = { verbose := true, count := 3, name := "Bob" }
      ∧ rest.toList = []
  | _ => False)

#guard (match evalNative nativeExample ["--count", "five", "Bob"] with
  | .error err => err.code = .invalid ∧
      containsSubstring (err.detail?.getD "") "Expected a natural number"
  | _ => False)

#guard (match evalNative nativeExample ["--count", "5"] with
  | .error err => err.code = .missing
  | _ => False)

#guard (match evalNative (Interpreter.positional { metavar := "NAME", help? := none, required := true }) ["Grace"] with
  | .ok name rest => name = "Grace" ∧ rest.toList = []
  | _ => False)

#guard (match evalNative (Interpreter.positional { metavar := "NAME", help? := none, required := true }) [] with
  | .error err => err.code = .missing
  | _ => False)

#guard (match evalNative (Interpreter.flag verboseDoc "verbose") ["--verbose", "--verbose"] with
  | .ok present rest => present ∧ rest.toList = []
  | _ => False)

#guard (match evalNative (Interpreter.option countDoc "count") ["--count=9", "--count", "7"] with
  | .ok (Option.some value) rest => value = "7" ∧ rest.toList = []
  | _ => False)

#guard (match evalNative (Interpreter.option countDoc "count") [] with
  | .ok Option.none rest => rest.toList = []
  | _ => False)

#guard (match evalNative (Interpreter.many (Interpreter.positional { metavar := "ITEM", help? := none, required := false })) ["one", "two"] with
  | .ok values rest => values = ["one", "two"] ∧ rest.toList = []
  | _ => False)

#guard (match evalNative (Interpreter.some (Interpreter.positional { metavar := "ITEM", help? := none, required := false })) [] with
  | .error err => err.code = .missing
  | _ => False)

#guard (match evalNative (Interpreter.optional (Interpreter.positional { metavar := "ITEM", help? := none, required := false })) [] with
  | .ok Option.none rest => rest.toList = []
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

import Argparse

open Argparse
open Argparse.OptionSpec
open Argparse.FlagSpec
open Argparse.Completion
open Argparse.Native
open Argparse.Native.Interpreter
open Argparse.Native.Consumer
open Argparse.Native.Token
open Argparse.Native.TokenStream

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

private def verboseDoc : OptionDoc :=
  { long? := some "verbose", short? := some 'v', help? := some "Enable verbose output", required := false }

private def countDoc : OptionDoc :=
  { long? := some "count", short? := some 'n', metavar? := some "COUNT", help? := some "Number of repetitions", required := false }

private def nameDoc : PositionalDoc :=
  { metavar := "NAME", help? := none, required := true }

private def flagLongShort (longName : String) (shortName : Char) (doc : OptionDoc) : Interpreter Bool :=
  {
    grammar := Grammar.flag doc,
    eval := fun stream =>
      match Consumer.consumeFlag longName stream with
      | .ok (longPresent, stream') =>
        match Consumer.consumeFlag shortName stream' with
        | .ok (shortPresent, stream'') => .ok (longPresent || shortPresent) stream''
        | .error err => .error err
      | .error err => .error err
  }

private def optionSubject (doc : OptionDoc) : Option String :=
  match doc.long?, doc.short? with
  | Option.some longName, _ => Option.some s!"--{longName}"
  | Option.none, Option.some shortName => Option.some s!"-{String.mk [shortName]}"
  | Option.none, Option.none => doc.metavar?

private def optionLongShortNat (longName : String) (shortName : Char) (doc : OptionDoc) : Interpreter Nat :=
  let attempt
      (usage : Argparse.Usage)
      (first second : ArgStream → Except Error (Option String × ArgStream))
      (combine : Option String → Option String → Option String)
      : Interpreter Nat :=
    {
      grammar := { usage := usage },
      eval := fun stream =>
        match first stream with
        | .ok (firstValue?, stream') =>
          match second stream' with
          | .ok (secondValue?, stream'') =>
            let value? := combine firstValue? secondValue?
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
  let longThenShort :=
    attempt (Grammar.option doc).usage
      (Consumer.consumeValue longName)
      (Consumer.consumeValue shortName)
      (fun longValue? shortValue? => shortValue?.orElse fun _ => longValue?)
  let shortThenLong :=
    attempt Argparse.Usage.empty
      (Consumer.consumeValue shortName)
      (Consumer.consumeValue longName)
      (fun shortValue? longValue? => shortValue?.orElse fun _ => longValue?)
  longThenShort <|> shortThenLong

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

private def runNativeExample (args : List String) :=
  Interpreter.eval nativeExample (ArgStream.ofList args)

/-- Split raw CLI tokens into the `ArgStream` front section and post-`--` tail. -/
private def splitArgs : List String → List String × List String
  | [] => ([], [])
  | "--" :: rest => ([], rest)
  | tok :: rest =>
      let (front, tail) := splitArgs rest
      (tok :: front, tail)

/-- Reassemble CLI tokens from explicit front/tail segments. -/
private def assembleArgs (front tail : List String) : List String :=
  if tail = [] then
    front
  else
    front ++ "--" :: tail

/-- Drop the first `--` sentinel from a token list (if present). -/
private def dropSentinel : List String → List String
  | [] => []
  | tok :: rest =>
      if tok = "--" then
        rest
      else
        tok :: dropSentinel rest

/-- Insert `x` at position `idx` within `xs`. Assumes `idx ≤ xs.length`. -/
private def insertAt (xs : List α) : Nat → α → List α
  | 0, x => x :: xs
  | Nat.succ idx, x =>
      match xs with
      | [] => [x]
      | y :: ys => y :: insertAt ys idx x

/-- Produce all lists formed by inserting `x` at every position in `xs`. -/
private def insertEverywhere (x : α) (xs : List α) : List (List α) :=
  (List.range (xs.length + 1)).map (fun idx => insertAt xs idx x)

/-- `List`-style `bind` specialised for finite enumeration. -/
private def concatMap (xs : List α) (f : α → List β) : List β :=
  xs.foldr (fun x acc => f x ++ acc) []

/-- Enumerate all sequences of length `len` drawn from `alphabet` (with repetition). -/
private def sequences (alphabet : List α) : Nat → List (List α)
  | 0 => [[]]
  | Nat.succ n =>
      concatMap (sequences alphabet n) fun seq => alphabet.map (fun tok => seq ++ [tok])

/-- Remove the first occurrence of the long flag `--name` from the front tokens. -/
private def removeLongFlag (name : String) : List String → Bool × List String
  | [] => (false, [])
  | tok :: rest =>
      match parseLong? tok with
      | Option.some (found, Option.none) =>
          if found = name then
            (true, rest)
          else
            let (present, remainder) := removeLongFlag name rest
            (present, tok :: remainder)
      | _ =>
          let (present, remainder) := removeLongFlag name rest
          (present, tok :: remainder)

/-- Does the front section contain `--name=<value>`? -/
private def hasInlineLongValue (name : String) : List String → Bool
  | [] => false
  | tok :: rest =>
      match parseLong? tok with
      | Option.some (found, Option.some _) =>
          if found = name then true else hasInlineLongValue name rest
      | _ => hasInlineLongValue name rest

/-- Expected outcome for consuming `--name` with a value from the front/tail. -/
private def expectedLongValue (name : String) (tokens : List String)
    : Except Error (Option String × List String) :=
  let (front, tail) := splitArgs tokens
  let rec go (processed : List String) : List String → Except Error (Option String × List String)
    | [] =>
        let newFront := processed.reverse
        .ok (Option.none, assembleArgs newFront tail)
    | tok :: rest =>
        match parseLong? tok with
        | Option.some (found, value?) =>
            if found = name then
              match value? with
              | Option.some value =>
                  let newFront := processed.reverse ++ rest
                  .ok (Option.some value, assembleArgs newFront tail)
              | Option.none =>
                  match rest with
                  | next :: restTail =>
                      let newFront := processed.reverse ++ restTail
                      .ok (Option.some next, assembleArgs newFront tail)
                  | [] =>
                      match tail with
                      | next :: tailRest =>
                          let newFront := processed.reverse
                          .ok (Option.some next, assembleArgs newFront tailRest)
                      | [] =>
                          .error {
                            code := .missing
                            , subject? := some s!"--{name}"
                          }
            else
              go (tok :: processed) rest
        | Option.none =>
            go (tok :: processed) rest
  go [] front

/-- Does the front section contain `-name=<value>` or `-name<value>`? -/
private def hasInlineShortValue (name : Char) : List String → Bool
  | [] => false
  | tok :: rest =>
      match parseShort? tok with
      | Option.some (found, Option.some _) =>
          if found = name then true else hasInlineShortValue name rest
      | _ => hasInlineShortValue name rest

/-- Remove the first occurrence of `-name` (without an inline value) from the front tokens. -/
private def removeShortFlag (name : Char) : List String → Bool × List String
  | [] => (false, [])
  | tok :: rest =>
      match parseShort? tok with
      | Option.some (found, Option.none) =>
          if found = name then
            (true, rest)
          else
            let (present, remainder) := removeShortFlag name rest
            (present, tok :: remainder)
      | _ =>
          let (present, remainder) := removeShortFlag name rest
          (present, tok :: remainder)

/-- Expected outcome for consuming `-name` with a value from the front/tail. -/
private def expectedShortValue (name : Char) (tokens : List String)
    : Except Error (Option String × List String) :=
  let (front, tail) := splitArgs tokens
  let rec go (processed : List String) : List String → Except Error (Option String × List String)
    | [] =>
        let newFront := processed.reverse
        .ok (Option.none, assembleArgs newFront tail)
    | tok :: rest =>
        match parseShort? tok with
        | Option.some (found, value?) =>
            if found = name then
              match value? with
              | Option.some value =>
                  let newFront := processed.reverse ++ rest
                  .ok (Option.some value, assembleArgs newFront tail)
              | Option.none =>
                  match rest with
                  | next :: restTail =>
                      let newFront := processed.reverse ++ restTail
                      .ok (Option.some next, assembleArgs newFront tail)
                  | [] =>
                      match tail with
                      | next :: tailRest =>
                          let newFront := processed.reverse
                          .ok (Option.some next, assembleArgs newFront tailRest)
                      | [] =>
                          .error {
                            code := .missing
                            , subject? := some s!"-{String.mk [name]}"
                          }
            else
              go (tok :: processed) rest
        | Option.none =>
            go (tok :: processed) rest
  go [] front

/-- Base tokens used to synthesise randomised flag/value scenarios. -/
private def frontAlphabet : List String := ["foo", "bar", "--other", "-q", "13"]

private def tailAlphabet : List String := ["tail", "extra", "--verbose", "--count", "value", "13"]

private def lengthChoices : List Nat := [0, 1, 2]

private def lengthChoicesSmall : List Nat := [0, 1]

private def frontScenarios : List (List String) :=
  concatMap lengthChoices fun len => sequences frontAlphabet len

private def tailScenarios : List (List String) :=
  concatMap lengthChoices fun len => sequences tailAlphabet len

private def insertSequence (base : List String) (tokens : List String) : List (List String) :=
  tokens.foldl
    (fun variants tok => concatMap variants fun seq => insertEverywhere tok seq)
    [base]

private def subsetsUpTo {α} : List α → Nat → List (List α)
  | _, 0 => [[]]
  | [], _ => [[]]
  | x :: xs, Nat.succ k =>
      let without := subsetsUpTo xs (Nat.succ k)
      let withSubsets := (subsetsUpTo xs k).map (List.cons x)
      without ++ withSubsets

private def synthesizeScenariosUpTo (tokens : List String) (maxInsert : Nat) : List (List String) :=
  concatMap frontScenarios fun base =>
    concatMap (subsetsUpTo tokens maxInsert) fun selection => insertSequence base selection

private def synthesizeScenarios (tokens : List String) : List (List String) :=
  synthesizeScenariosUpTo tokens 1

private def flagTokens : List String :=
  ["--verbose", "--verbose=value", "-v", "-v=on", "-v1"]

private def optionTokens : List String :=
  ["--count", "--count=13", "-n", "-n13", "-n=13"]

private def mixedFlagValueTokens : List String :=
  ["--verbose", "-v", "--count", "-n", "--count=13"]

private def mixedFrontScenarios : List (List String) :=
  concatMap lengthChoicesSmall fun len => sequences frontAlphabet len

private def mixedTailScenarios : List (List String) :=
  concatMap lengthChoicesSmall fun len => sequences tailAlphabet len

private def flagInputs : List (List String) :=
  concatMap (synthesizeScenarios flagTokens) fun front =>
    tailScenarios.map fun tail => assembleArgs front tail

private def optionInputs : List (List String) :=
  concatMap (synthesizeScenarios optionTokens) fun front =>
    tailScenarios.map fun tail => assembleArgs front tail

private def mixedFlagValueInputs : List (List String) :=
  let raw :=
    concatMap mixedFrontScenarios fun base =>
      let variants :=
        concatMap (subsetsUpTo mixedFlagValueTokens 2) fun selection => insertSequence base selection
      concatMap variants fun front =>
        mixedTailScenarios.map fun tail => assembleArgs front tail
  raw.filter fun tokens =>
    let (front, tail) := splitArgs tokens
    let frontHas := front.any fun tok => !isOptionLike tok
    let tailHas := ¬ tail.isEmpty
    frontHas || tailHas

private def errorKindToCode : ParseErrorKind → ErrorCode
  | .missing => ErrorCode.missing
  | .invalid => ErrorCode.invalid
  | .unexpected => ErrorCode.unexpected

private def positionalInputs : List (List String) :=
  concatMap frontScenarios fun front =>
    tailScenarios.map fun tail => assembleArgs front tail
#guard (match Interpreter.eval (Interpreter.many (Interpreter.positional { metavar := "ITEM", help? := none, required := false }))
    (ArgStream.ofList ["one", "two"]) with
  | .ok values rest => decide (values = ["one", "two"] ∧ ArgStream.remaining rest = [])
  | _ => False)

#guard (flagInputs.all fun tokens =>
  let stream := ArgStream.ofList tokens
  let (front, tail) := splitArgs tokens
  let inline := hasInlineLongValue "verbose" front
  match Consumer.consumeLongFlag "verbose" stream with
  | .error err => decide (inline ∧ err.code = ErrorCode.invalid)
  | .ok (present, restStream) =>
      if inline then
        False
      else
        let (expectedPresent, newFront) := removeLongFlag "verbose" front
        let expectedTokens := assembleArgs newFront tail
        decide (present = expectedPresent ∧ ArgStream.remaining restStream = expectedTokens))

#guard (flagInputs.all fun tokens =>
  let stream := ArgStream.ofList tokens
  let (front, tail) := splitArgs tokens
  let inline := hasInlineShortValue 'v' front
  match Consumer.consumeShortFlag 'v' stream with
  | .error err => decide (inline ∧ err.code = ErrorCode.invalid)
  | .ok (present, restStream) =>
      if inline then
        False
      else
        let (expectedPresent, newFront) := removeShortFlag 'v' front
        let expectedTokens := assembleArgs newFront tail
        decide (present = expectedPresent ∧ ArgStream.remaining restStream = expectedTokens))

#guard (optionInputs.all fun tokens =>
  let expected := expectedLongValue "count" tokens
  let actual := Consumer.consumeLongValue "count" (ArgStream.ofList tokens)
  match expected, actual with
  | .ok (expectedValue, expectedTokens), .ok (value, restStream) =>
      decide (value = expectedValue ∧ ArgStream.remaining restStream = expectedTokens)
  | .error expectedErr, .error actualErr => decide (actualErr.code = expectedErr.code)
  | _, _ => False)

#guard (optionInputs.all fun tokens =>
  let expected := expectedShortValue 'n' tokens
  let actual := Consumer.consumeShortValue 'n' (ArgStream.ofList tokens)
  match expected, actual with
  | .ok (expectedValue, expectedTokens), .ok (value, restStream) =>
      decide (value = expectedValue ∧ ArgStream.remaining restStream = expectedTokens)
  | .error expectedErr, .error actualErr => decide (actualErr.code = expectedErr.code)
  | _, _ => False)

private def missingCountError : Error :=
  { code := .missing, subject? := optionSubject countDoc }

private def expectedLongThenShort (tokens : List String)
    : Except Error (Option String × List String) :=
  match expectedLongValue "count" tokens with
  | .error err => .error err
  | .ok (longValue?, tokensAfterLong) =>
      match expectedShortValue 'n' tokensAfterLong with
      | .error err => .error err
      | .ok (shortValue?, tokensAfterShort) =>
          let value? := shortValue?.orElse fun _ => longValue?
          match value? with
          | Option.some value => .ok (Option.some value, tokensAfterShort)
          | Option.none => .error missingCountError

private def expectedShortThenLong (tokens : List String)
    : Except Error (Option String × List String) :=
  match expectedShortValue 'n' tokens with
  | .error err => .error err
  | .ok (shortValue?, tokensAfterShort) =>
      match expectedLongValue "count" tokensAfterShort with
      | .error err => .error err
      | .ok (longValue?, tokensAfterLong) =>
          let value? := shortValue?.orElse fun _ => longValue?
          match value? with
          | Option.some value => .ok (Option.some value, tokensAfterLong)
          | Option.none => .error missingCountError

private def expectedOptionValue (tokens : List String)
    : Except Error (Option String × List String) :=
  match expectedLongThenShort tokens with
  | .ok result => .ok result
  | .error _ => expectedShortThenLong tokens

private def expectedOptionNat (tokens : List String)
    : Except Error (Nat × List String) :=
  match expectedOptionValue tokens with
  | .error err => .error err
  | .ok (value?, tokensAfter) =>
      match value? with
      | Option.some raw =>
          match raw.toNat? with
          | Option.some n => .ok (n, tokensAfter)
          | Option.none =>
              .error {
                code := .invalid,
                subject? := some "--count",
                detail? := some s!"Expected a natural number for count, got '{raw}'"
              }
      | Option.none => .error missingCountError

private def expectedFlagResult (tokens : List String)
    : Except Error (Bool × List String × List String) :=
  let (front, tail) := splitArgs tokens
  if hasInlineLongValue "verbose" front then
    .error { code := .invalid, subject? := some "--verbose" }
  else if hasInlineShortValue 'v' front then
    .error { code := .invalid, subject? := some "-v" }
  else
    let (longPresent, front') := removeLongFlag "verbose" front
    let (shortPresent, front'') := removeShortFlag 'v' front'
    .ok (longPresent || shortPresent, front'', tail)

#guard (mixedFlagValueInputs.all fun tokens =>
  let stream := ArgStream.ofList tokens
  let flagEval := Interpreter.eval (flagLongShort "verbose" 'v' verboseDoc) stream
  match expectedFlagResult tokens, flagEval with
  | .error expectedErr, .error actualErr => decide (actualErr.code = expectedErr.code)
  | .error _, _ => False
  | .ok (expectedFlag, frontAfterFlag, tailAfterFlag), .ok actualFlag streamAfterFlag =>
      let expectedStreamTokens := assembleArgs frontAfterFlag tailAfterFlag
      if decide (actualFlag = expectedFlag ∧ ArgStream.remaining streamAfterFlag = expectedStreamTokens) then
        match expectedOptionNat expectedStreamTokens, Interpreter.eval (optionLongShortNat "count" 'n' countDoc) streamAfterFlag with
        | .error expectedErr, .error actualErr => decide (actualErr.code = expectedErr.code)
        | .ok (expectedCount, remainingTokens), .ok actualCount streamAfterOption =>
            decide (actualCount = expectedCount ∧ ArgStream.remaining streamAfterOption = remainingTokens)
        | _, _ => False
      else
        False
  | _, _ => False)

private def propertyItemDoc : PositionalDoc :=
  { metavar := "ITEM", help? := none, required := false }

#guard (positionalInputs.all fun tokens =>
  match Interpreter.eval (Interpreter.many (Interpreter.positional propertyItemDoc))
      (ArgStream.ofList tokens) with
  | .ok values rest => decide (values = dropSentinel tokens ∧ ArgStream.remaining rest = [])
  | _ => False)

private def nonEmptyPositionalInputs :=
  positionalInputs.filter fun tokens => dropSentinel tokens ≠ []

private def emptyPositionalInputs :=
  positionalInputs.filter fun tokens => dropSentinel tokens = []

#guard (nonEmptyPositionalInputs.all fun tokens =>
  match Interpreter.eval (Interpreter.some (Interpreter.positional propertyItemDoc))
      (ArgStream.ofList tokens) with
  | .ok values rest =>
      decide (values = dropSentinel tokens ∧ ArgStream.remaining rest = [])
  | _ => False)

#guard (emptyPositionalInputs.all fun tokens =>
  match Interpreter.eval (Interpreter.some (Interpreter.positional propertyItemDoc))
      (ArgStream.ofList tokens) with
  | .error err => decide (err.code = ErrorCode.missing)
  | _ => False)

#guard (match Interpreter.eval (Interpreter.many (Interpreter.positional { metavar := "ITEM", help? := none, required := false }))
    (ArgStream.ofList []) with
  | .ok values rest => decide (values = [] ∧ ArgStream.remaining rest = [])
  | _ => False)

#guard (match Interpreter.eval (Interpreter.some (Interpreter.positional { metavar := "ITEM", help? := none, required := false }))
    (ArgStream.ofList []) with
  | .error err => decide (err.code = .missing)
  | _ => False)

#guard (match Interpreter.eval
    (Interpreter.optional (Interpreter.some (Interpreter.positional { metavar := "ITEM", help? := none, required := false })))
    (ArgStream.ofList ["one", "two"]) with
  | .ok (Option.some values) rest => decide (values = ["one", "two"] ∧ ArgStream.remaining rest = [])
  | _ => False)

#guard (match Interpreter.eval
    (Interpreter.optional (Interpreter.some (Interpreter.positional { metavar := "ITEM", help? := none, required := false })))
    (ArgStream.ofList []) with
  | .ok Option.none rest => decide (ArgStream.remaining rest = [])
  | _ => False)

#guard (match Interpreter.eval
    (Interpreter.optional (Interpreter.fail (α := String) { code := .invalid, detail? := Option.some "boom" }))
    (ArgStream.ofList []) with
  | .error err => decide (err.code = ErrorCode.invalid)
  | _ => False)

#guard (match Interpreter.eval
    (Interpreter.choice
      [ Interpreter.some (Interpreter.positional { metavar := "ITEM", help? := none, required := false })
      , Interpreter.many (Interpreter.positional { metavar := "ITEM", help? := none, required := false }) ])
    (ArgStream.ofList ["alpha", "beta"]) with
  | .ok values rest => decide (values = ["alpha", "beta"] ∧ ArgStream.remaining rest = [])
  | _ => False)

#guard (match Interpreter.eval
    (Interpreter.choice
      [ Interpreter.some (Interpreter.positional { metavar := "ITEM", help? := none, required := false })
      , Interpreter.many (Interpreter.positional { metavar := "ITEM", help? := none, required := false }) ])
    (ArgStream.ofList []) with
  | .ok values rest => decide (values = [] ∧ ArgStream.remaining rest = [])
  | _ => False)

#guard (match Interpreter.eval
    (Interpreter.withDefault (Interpreter.some (Interpreter.positional { metavar := "ITEM", help? := none, required := false })) ["default"]) 
    (ArgStream.ofList []) with
  | .ok values rest => decide (values = ["default"] ∧ ArgStream.remaining rest = [])
  | _ => False)

#guard (match Interpreter.eval
    (Interpreter.withDefault (Interpreter.some (Interpreter.positional { metavar := "ITEM", help? := none, required := false })) ["default"]) 
    (ArgStream.ofList ["uno"]) with
  | .ok values rest => decide (values = ["uno"] ∧ ArgStream.remaining rest = [])
  | _ => False)

#guard (match Consumer.consumeLongFlag "verbose" (ArgStream.ofList ["--verbose"]) with
  | .ok (present, rest) => decide (present && ArgStream.remaining rest = [])
  | _ => False)

#guard (match Consumer.consumeShortFlag 'v' (ArgStream.ofList ["-v"]) with
  | .ok (present, rest) => decide (present && ArgStream.remaining rest = [])
  | _ => False)

#guard (match Consumer.consumeLongValue "count" (ArgStream.ofList ["--count", "5"]) with
  | .ok (value?, rest) => decide (value? = some "5" && ArgStream.remaining rest = [])
  | _ => False)

#guard (match Consumer.consumeShortValue 'n' (ArgStream.ofList ["-n5"]) with
  | .ok (value?, rest) => decide (value? = some "5" && ArgStream.remaining rest = [])
  | _ => False)

#guard (match Consumer.consumeLongValue "count" (ArgStream.ofList ["--count"]) with
  | .error err => decide (err.code = .missing)
  | _ => False)

#guard (match Argparse.ParserInfo.exec exampleInfo ["Alice"] with
  | .success cfg => decide (cfg = { verbose := false, count := 1, name := "Alice" })
  | _ => False)

#guard (match Argparse.ParserInfo.exec exampleInfo ["--verbose", "--count", "3", "Bob"] with
  | .success cfg => decide (cfg = { verbose := true, count := 3, name := "Bob" })
  | _ => False)

#guard (match Argparse.ParserInfo.exec exampleInfo ["--count", "5"] with
  | .failure err => decide (err.error.kind = .missing)
  | _ => False)

#guard (match runNativeExample ["Alice"] with
  | .ok cfg rest =>
      decide (cfg = { verbose := false, count := 1, name := "Alice" }
        ∧ ArgStream.remaining rest = [])
  | _ => False)

#guard (match runNativeExample ["--verbose", "-n", "3", "Bob"] with
  | .ok cfg rest =>
      decide (cfg = { verbose := true, count := 3, name := "Bob" }
        ∧ ArgStream.remaining rest = [])
  | _ => False)

#guard (match runNativeExample ["--count", "five", "Bob"] with
  | .error err => decide (err.code = .invalid)
  | _ => False)

#guard (match runNativeExample ["--count", "5"] with
  | .error err => decide (err.code = .missing)
  | _ => False)

private structure CommandResult where
  tag : String
  target? : Option String
  deriving Repr, DecidableEq

private def commandParser : Parser CommandResult :=
  subcommand {
    metavar := "CMD",
    commands := [
      {
        name := "hello",
        description? := some "Say hello",
        parser := (pure { tag := "hello", target? := none } : Parser CommandResult)
      },
      {
        name := "run",
        description? := some "Run against a target",
        parser := (pure (fun target => { tag := "run", target? := some target })
          : Parser (String → CommandResult)) <*> rawArgument "TARGET"
      }
    ]
  }

#guard (match Argparse.ParserInfo.exec { progName := "cmd", parser := commandParser } ["hello"] with
  | .success cfg => decide (cfg = { tag := "hello", target? := none })
  | _ => False)

#guard (match Argparse.ParserInfo.exec { progName := "cmd", parser := commandParser } ["run", "tests"] with
  | .success cfg => decide (cfg = { tag := "run", target? := some "tests" })
  | _ => False)

#guard (match Argparse.ParserInfo.exec { progName := "cmd", parser := commandParser } ["unknown"] with
  | .failure err => decide (err.error.kind = .invalid)
  | _ => False)

#guard (match Argparse.ParserInfo.exec exampleInfo ["--help"] with
  | .showHelp => True
  | _ => False)

private def repeatedArgs : Parser (List String) :=
  Parser.many (rawArgument "ITEM")

#guard (match Argparse.ParserInfo.exec { progName := "items", parser := repeatedArgs } ["one", "two", "three"] with
  | .success items => decide (items = ["one", "two", "three"])
  | _ => False)

#guard (match Argparse.ParserInfo.exec { progName := "items", parser := repeatedArgs } [] with
  | .success items => decide (items = [])
  | _ => False)

#guard (match Argparse.ParserInfo.exec { progName := "items", parser := Parser.some (rawArgument "ITEM") } [] with
  | .failure err => decide (err.error.kind = .missing)
  | _ => False)

private def requiredFlag : Parser Bool :=
  flag' <|
    FlagSpec.build false true [
      FlagSpec.long "loud",
      FlagSpec.short 'L',
      FlagSpec.help "Enable loud mode"
    ]

#guard (match Argparse.ParserInfo.exec { progName := "flags", parser := requiredFlag } ["--loud"] with
  | .success value => decide (value = true)
  | _ => False)

#guard (match Argparse.ParserInfo.exec { progName := "flags", parser := requiredFlag } [] with
  | .failure err => decide (err.error.kind = .missing)
  | _ => False)

private def choiceParser : Parser String :=
  Parser.choice [
    strOption [OptionSpec.long "name", OptionSpec.help "Primary name"],
    strOption [OptionSpec.long "alias", OptionSpec.help "Alias"]
  ]

#guard (match Argparse.ParserInfo.exec { progName := "choice", parser := choiceParser } ["--alias", "Bob"] with
  | .success value => decide (value = "Bob")
  | _ => False)

#guard (match Argparse.ParserInfo.exec { progName := "choice", parser := choiceParser } [] with
  | .failure err => decide (err.error.kind = .missing)
  | _ => False)

#guard (containsSubstring (Argparse.ParserInfo.renderBashCompletion exampleInfo) "--count")
#guard (containsSubstring (Argparse.ParserInfo.renderZshCompletion exampleInfo) "_arguments")
#guard (containsSubstring (Argparse.ParserInfo.renderFishCompletion exampleInfo) "complete -c")
#guard (containsSubstring (Argparse.ParserInfo.renderManpage exampleInfo) ".SH OPTIONS")

private def completionOnlyParser : Parser Shell :=
  defaultShellOption

private def completionOnlyInfo : ParserInfo Shell := {
  progName := "complete-demo",
  parser := completionOnlyParser
}

#guard (match Argparse.ParserInfo.exec completionOnlyInfo ["--completions", "bash"] with
  | .success shell => decide (Shell.name shell = "bash")
  | _ => False)

#guard (match Argparse.ParserInfo.exec completionOnlyInfo ["--completions", "FISH"] with
  | .success shell => decide (Shell.name shell = "fish")
  | _ => False)

#guard (match Argparse.ParserInfo.exec completionOnlyInfo ["--completions", "unknown"] with
  | .failure err => decide (err.error.kind = .invalid)
  | _ => False)

private def optionalCompletionParser : Parser (Option Shell) :=
  defaultOptionalShellOption

#guard (match Argparse.ParserInfo.exec { progName := "opt-complete", parser := optionalCompletionParser } [] with
  | .success none => True
  | _ => False)

#guard (containsSubstring (Argparse.ParserInfo.renderCompletionFor (Option.get! (Shell.ofString? "bash")) completionOnlyInfo) "--completions")

end ArgparseTests

/-- Trivial entry point for the test executable. -/
def main : IO Unit :=
  pure ()

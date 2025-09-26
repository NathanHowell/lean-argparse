import Argparse

open Argparse
open Argparse.OptionSpec
open Argparse.FlagSpec
open Argparse.Completion
open Argparse.Native
open Argparse.Native.Interpreter
open Argparse.Native.Consumer
open Argparse.Native.Token

namespace ArgparseTests

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
  pure ExampleCfg.mk
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

private def optionLongShortNat (longName : String) (shortName : Char) (doc : OptionDoc) (default : Nat) : Interpreter Nat :=
  {
    grammar := { usage := (Grammar.option doc).usage },
    eval := fun stream =>
      match Consumer.consumeValue longName stream with
      | .ok (longValue?, stream') =>
        match Consumer.consumeValue shortName stream' with
        | .ok (shortValue?, stream'') =>
          let value? := shortValue?.orElse fun _ => longValue?
          match value? with
          | Option.none => .ok default stream''
          | Option.some raw =>
            match raw.toNat? with
            | Option.some n => .ok n stream''
            | Option.none => .error {
                code := .invalid,
                subject? := some s!"--{longName}",
                detail? := some s!"Expected a natural number for {longName}, got '{raw}'"
              }
        | .error err => .error err
      | .error err => .error err
  }

private def nativeExample : Interpreter ExampleCfg :=
  let verbose := flagLongShort "verbose" 'v' verboseDoc
  let count := optionLongShortNat "count" 'n' countDoc 1
  let name := Interpreter.positional nameDoc
  {
    grammar := {
      usage := Argparse.Usage.append verbose.grammar.usage
        (Argparse.Usage.append count.grammar.usage name.grammar.usage)
    },
    eval := fun stream =>
      match verbose.eval stream with
      | .ok verboseVal stream1 =>
        match count.eval stream1 with
        | .ok countVal stream2 =>
          match name.eval stream2 with
          | .ok nameVal stream3 =>
              .ok { verbose := verboseVal, count := countVal, name := nameVal } stream3
          | .error err => .error err
        | .error err => .error err
      | .error err => .error err
  }

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

/-- Base tokens used to synthesise randomised flag/value scenarios. -/
private def frontAlphabet : List String := ["foo", "bar", "--other", "-q"]

private def tailAlphabet : List String := ["tail", "extra", "--verbose", "--count", "value"]

private def lengthChoices : List Nat := [0, 1, 2]

private def frontScenarios : List (List String) :=
  concatMap lengthChoices fun len => sequences frontAlphabet len

private def tailScenarios : List (List String) :=
  concatMap lengthChoices fun len => sequences tailAlphabet len

private def synthesizeScenarios (primary inline : String) : List (List String) :=
  concatMap frontScenarios fun base =>
    let withPrimary := insertEverywhere primary base
    let withInline := insertEverywhere inline base
    base :: (withPrimary ++ withInline)

private def flagInputs : List (List String) :=
  concatMap (synthesizeScenarios "--verbose" "--verbose=value") fun front =>
    tailScenarios.map fun tail => assembleArgs front tail

private def optionInputs : List (List String) :=
  concatMap (synthesizeScenarios "--count" "--count=13") fun front =>
    tailScenarios.map fun tail => assembleArgs front tail

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

#guard (optionInputs.all fun tokens =>
  let expected := expectedLongValue "count" tokens
  let actual := Consumer.consumeLongValue "count" (ArgStream.ofList tokens)
  match expected, actual with
  | .ok (expectedValue, expectedTokens), .ok (value, restStream) =>
      decide (value = expectedValue ∧ ArgStream.remaining restStream = expectedTokens)
  | .error expectedErr, .error actualErr => decide (actualErr.code = expectedErr.code)
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
        parser := pure { tag := "hello", target? := none }
      },
      {
        name := "run",
        description? := some "Run against a target",
        parser := pure (fun target => { tag := "run", target? := some target }) <*> rawArgument "TARGET"
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

import Std
import LeanArgparse.Basic.Specs
import LeanArgparse.Basic.Parser
import LeanArgparse.Basic.ParseState
import LeanArgparse.Basic.Docs

namespace LeanArgparse

open Std
open Usage

private def optionDoc (spec : OptionSpec α) : OptionDoc := {
  long? := spec.long?,
  short? := spec.short?,
  metavar? := some spec.metavar,
  help? := spec.help?,
  required := spec.default?.isNone,
  default? := spec.showDefault?
}

private def flagDoc (spec : FlagSpec α) (required : Bool := false) : OptionDoc := {
  long? := spec.long?,
  short? := spec.short?,
  metavar? := none,
  help? := spec.help?,
  required := required,
  default? := none
}

private def positionalDoc (spec : ArgumentSpec α) (required : Bool := true) : PositionalDoc := {
  metavar := spec.metavar,
  help? := spec.help?,
  required := required
}

def option {α} (spec : OptionSpec α) : Parser α :=
  let longName := spec.long?
  let shortName := spec.short?
  let doc := optionDoc spec
  let contextName :=
    match longName, shortName with
    | some l, _ => s!"--{l}"
    | none, some s => s!"-{String.mk [s]}"
    | none, none => "<option>"
  {
    run := fun s => do
      let (longVal?, s) ← match longName with
        | some long => ParseState.consumeLongValue s long
        | none => .ok (none, s)
      let (value?, s) ← match longVal? with
        | some _ => .ok (longVal?, s)
        | none =>
          match shortName with
          | some short => ParseState.consumeShortValue s short
          | none => .ok (none, s)
      match value? with
      | some raw =>
        match spec.reader.run raw with
        | .ok value => return (value, s)
        | .error msg =>
          .error {
            kind := .invalid,
            message := s!"Invalid value for {contextName}: {msg}",
            context? := some contextName
          }
      | none =>
        match spec.default? with
        | some value => return (value, s)
        | none =>
          .error {
            kind := .missing,
            message := s!"Missing required option {contextName}",
            context? := some contextName
          }
    , usage := Usage.mergeOption doc Usage.empty
  }

def optionWith {α} (reader : ValueReader α) (mods : List (OptionSpec.Mod α)) : Parser α :=
  option (OptionSpec.build reader mods)

def strOption (mods : List (OptionSpec.Mod String)) : Parser String :=
  optionWith ValueReader.id mods

def natOption (mods : List (OptionSpec.Mod Nat)) : Parser Nat :=
  optionWith ValueReader.nat mods

def intOption (mods : List (OptionSpec.Mod Int)) : Parser Int :=
  optionWith ValueReader.int mods

def flag {α} (spec : FlagSpec α) : Parser α :=
  {
    run := fun s => do
      let (found, s) ← match spec.long? with
        | some long => ParseState.consumeLongFlag s long
        | none => .ok (false, s)
      if found then
        return (spec.active, s)
      else
        let (shortFound, s) ← match spec.short? with
          | some short => ParseState.consumeShortFlag s short
          | none => .ok (false, s)
        return ((if shortFound then spec.active else spec.default), s)
    , usage := Usage.mergeOption (flagDoc spec) Usage.empty
  }

def flag' {α} (spec : FlagSpec α) : Parser α :=
  let contextName :=
    match spec.long?, spec.short? with
    | some l, _ => s!"--{l}"
    | none, some s => s!"-{String.mk [s]}"
    | none, none => "<flag>"
  {
    run := fun s => do
      let (found, s) ← match spec.long? with
        | some long => ParseState.consumeLongFlag s long
        | none => .ok (false, s)
      if found then
        return (spec.active, s)
      else
        let (shortFound, s) ← match spec.short? with
          | some short => ParseState.consumeShortFlag s short
          | none => .ok (false, s)
        if shortFound then
          return (spec.active, s)
        else
          .error {
            kind := .missing,
            message := s!"Missing required flag {contextName}",
            context? := some contextName
          }
    , usage := Usage.mergeOption (flagDoc spec true) Usage.empty
  }

def switch (long : String) (short? : Option Char := none) (help? : Option String := none) : Parser Bool :=
  flag <|
    FlagSpec.build false true (
      FlagSpec.long long ::
      (match short? with | some c => [FlagSpec.short c] | none => []) ++
      (match help? with | some h => [FlagSpec.help h] | none => [])
    )

def argument {α} (spec : ArgumentSpec α) : Parser α :=
  {
    run := fun s =>
      match ParseState.takePositional? s with
      | some (raw, s) =>
        match spec.reader.run raw with
        | .ok value => return (value, s)
        | .error msg =>
          .error {
            kind := .invalid,
            message := s!"Invalid value for {spec.metavar}: {msg}",
            context? := some spec.metavar
          }
      | none =>
        .error {
          kind := .missing,
          message := s!"Missing required argument {spec.metavar}",
          context? := some spec.metavar
        }
    , usage := Usage.mergePositional (positionalDoc spec) Usage.empty
  }

def rawArgument (metavar : String) (help? : Option String := none) : Parser String :=
  argument {
    metavar,
    help?,
    reader := ValueReader.id
  }

def subcommand {α} (spec : SubcommandSpec α) : Parser α :=
  let cmdNames := spec.commands.map (·.name)
  let usageCommands := spec.commands.map (fun c => {
    name := c.name,
    description? := c.description?,
    usage := c.parser.usage
  })
  let usage := Usage.mergePositional {
    metavar := spec.metavar,
    help? := spec.help?,
    required := true
  } { Usage.empty with commands := usageCommands }
  {
    run := fun s =>
      match ParseState.takePositional? s with
      | none =>
        .error {
          kind := .missing,
          message := s!"Expected {spec.metavar}",
          context? := some spec.metavar,
          notes := if cmdNames.isEmpty then [] else ["Available: " ++ String.intercalate ", " cmdNames]
        }
      | some (name, s) =>
        match spec.commands.find? (·.name = name) with
        | none =>
          .error {
            kind := .invalid,
            message := s!"Unknown command {name}",
            context? := some spec.metavar,
            notes := if cmdNames.isEmpty then [] else ["Available: " ++ String.intercalate ", " cmdNames]
          }
        | some cmd => do
          match cmd.parser.run s with
          | .ok (value, s') => return (value, s')
          | .error err => .error err
    , usage
  }

end LeanArgparse

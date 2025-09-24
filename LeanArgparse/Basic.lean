import Std

namespace LeanArgparse

open Std

/-- Classifies parser errors for richer downstream handling. -/
inductive ParseErrorKind
  | missing
  | invalid
  | unexpected
  deriving DecidableEq, Repr

/-- Structured error used throughout the parser pipeline. -/
structure ParseError where
  kind : ParseErrorKind
  message : String
  context? : Option String := none
  notes : List String := []
  deriving Repr

/-- Documentation for a single option-style argument. -/
structure OptionDoc where
  long? : Option String := none
  short? : Option Char := none
  metavar? : Option String := none
  help? : Option String := none
  required : Bool := true
  default? : Option String := none
  deriving Repr

/-- Documentation for a positional argument. -/
structure PositionalDoc where
  metavar : String
  help? : Option String := none
  required : Bool := true
  deriving Repr

mutual
  /-- Describes usage information for a parser. -/
  structure Usage where
    options : List OptionDoc := []
    positionals : List PositionalDoc := []
    commands : List CommandDoc := []
    deriving Repr

  /-- Documentation for a single subcommand. -/
  structure CommandDoc where
    name : String
    description? : Option String := none
    usage : Usage
    deriving Repr
end

namespace Usage

def empty : Usage := { options := [], positionals := [], commands := [] }

def append (a b : Usage) : Usage := {
  options := a.options ++ b.options,
  positionals := a.positionals ++ b.positionals,
  commands := a.commands ++ b.commands
}

def optional (u : Usage) : Usage := {
  u with
    options := u.options.map (fun opt => { opt with required := false }),
    positionals := u.positionals.map (fun pos => { pos with required := false })
}

def mergeOption (opt : OptionDoc) (u : Usage) : Usage := {
  u with options := u.options ++ [opt]
}

def mergePositional (pos : PositionalDoc) (u : Usage) : Usage := {
  u with positionals := u.positionals ++ [pos]
}

def mergeCommand (cmd : CommandDoc) (u : Usage) : Usage := {
  u with commands := u.commands ++ [cmd]
}

end Usage

/-- Parser state separates pre and post `--` segments. -/
structure ParseState where
  front : List String
  tail : List String
  deriving Repr

namespace ParseState

def ofList (args : List String) : ParseState :=
  match args.span (· ≠ "--") with
  | (front, []) => { front := front, tail := [] }
  | (front, _ :: tail) => { front := front, tail := tail }

def withFront (s : ParseState) (front : List String) : ParseState := { s with front }

def withTail (s : ParseState) (tail : List String) : ParseState := { s with tail }

def remaining (s : ParseState) : List String :=
  match s.tail with
  | [] => s.front
  | tail => s.front ++ "--" :: tail

private def splitOnFirst (sep : Char) (xs : List Char) : List Char × Option (List Char) :=
  let rec loop (acc : List Char) : List Char → List Char × Option (List Char)
    | [] => (acc.reverse, none)
    | c :: cs =>
      if c = sep then
        (acc.reverse, some cs)
      else
        loop (c :: acc) cs
  loop [] xs

private def parseLongToken (tok : String) : Option (String × Option String) :=
  match tok.data with
  | '-' :: '-' :: rest =>
    let (nameChars, valueChars?) := splitOnFirst '=' rest
    let name := String.mk nameChars
    if name.isEmpty then none else
    some (name, valueChars?.map String.mk)
  | _ => none

private def parseShortToken (tok : String) : Option (Char × Option String) :=
  match tok.data with
  | '-' :: [] => none
  | '-' :: '-' :: _ => none
  | '-' :: c :: rest =>
    match rest with
    | [] => some (c, none)
    | '=' :: rest' => some (c, some (String.mk rest'))
    | _ => some (c, some (String.mk rest))
  | _ => none

private def isOptionLike (tok : String) : Bool :=
  match tok.data with
  | '-' :: '-' :: _ => true
  | '-' :: [] => false
  | '-' :: c :: _ => c.isAlpha
  | _ => false

def consumeLongFlag (s : ParseState) (name : String) : Except ParseError (Bool × ParseState) :=
  let rec loop (processed : List String) : List String → Except ParseError (Bool × List String)
    | [] => pure (false, (processed.reverse))
    | tok :: rest =>
      match parseLongToken tok with
      | some (found, value?) =>
        if found = name then
          match value? with
          | some _ =>
            throw <| ParseError.mk .invalid s!"Flag --{name} does not accept a value" (some s!"--{name}") []
          | none =>
            let newFront := processed.reverse ++ rest
            pure (true, newFront)
        else
          loop (tok :: processed) rest
      | none =>
        loop (tok :: processed) rest
  match loop [] s.front with
  | .ok (found, front) => pure (found, s.withFront front)
  | .error err => .error err

def consumeShortFlag (s : ParseState) (name : Char) : Except ParseError (Bool × ParseState) :=
  let rec loop (processed : List String) : List String → Except ParseError (Bool × List String)
    | [] => pure (false, processed.reverse)
    | tok :: rest =>
      match parseShortToken tok with
      | some (found, value?) =>
        if found = name then
          match value? with
          | some _ =>
            throw <| ParseError.mk .invalid s!"Flag -{String.mk [name]} does not accept a value" (some s!"-{String.mk [name]}") []
          | none =>
            let newFront := processed.reverse ++ rest
            pure (true, newFront)
        else
          loop (tok :: processed) rest
      | none =>
        loop (tok :: processed) rest
  match loop [] s.front with
  | .ok (found, front) => pure (found, s.withFront front)
  | .error err => .error err

def consumeLongValue (s : ParseState) (name : String) : Except ParseError (Option String × ParseState) :=
  let rec loop (processed : List String) : List String → Except ParseError (Option (String × List String))
    | [] => pure none
    | tok :: rest =>
      match parseLongToken tok with
      | some (found, value?) =>
        if found = name then
          match value? with
          | some v =>
            let newFront := processed.reverse ++ rest
            pure (some (v, newFront))
          | none =>
            match rest with
            | valueTok :: restTail =>
              let newFront := processed.reverse ++ restTail
              pure (some (valueTok, newFront))
            | [] =>
              throw <| ParseError.mk .missing s!"Option --{name} expects a value" (some s!"--{name}") []
        else
          loop (tok :: processed) rest
      | none => loop (tok :: processed) rest
  match loop [] s.front with
  | .ok none => pure (none, s)
  | .ok (some (value, front)) => pure (some value, s.withFront front)
  | .error err => .error err

def consumeShortValue (s : ParseState) (name : Char) : Except ParseError (Option String × ParseState) :=
  let rec loop (processed : List String) : List String → Except ParseError (Option (String × List String))
    | [] => pure none
    | tok :: rest =>
      match parseShortToken tok with
      | some (found, value?) =>
        if found = name then
          match value? with
          | some v =>
            let newFront := processed.reverse ++ rest
            pure (some (v, newFront))
          | none =>
            match rest with
            | valueTok :: restTail =>
              let newFront := processed.reverse ++ restTail
              pure (some (valueTok, newFront))
            | [] =>
              throw <| ParseError.mk .missing s!"Option -{String.mk [name]} expects a value" (some s!"-{String.mk [name]}") []
        else
          loop (tok :: processed) rest
      | none => loop (tok :: processed) rest
  match loop [] s.front with
  | .ok none => pure (none, s)
  | .ok (some (value, front)) => pure (some value, s.withFront front)
  | .error err => .error err

def takePositional? (s : ParseState) : Option (String × ParseState) :=
  let rec loop (before : List String) : List String → Option (String × List String)
    | [] => none
    | tok :: rest =>
      if isOptionLike tok then
        loop (tok :: before) rest
      else
        let newFront := before.reverse ++ rest
        some (tok, newFront)
  match loop [] s.front with
  | some (tok, front) => some (tok, s.withFront front)
  | none =>
    match s.tail with
    | [] => none
    | tok :: tail => some (tok, s.withTail tail)

end ParseState

/-- Core parser type with usage metadata. -/
structure Parser (α : Type u) where
  run : ParseState → Except ParseError (α × ParseState)
  usage : Usage

namespace Parser

def map {α β} (f : α → β) (p : Parser α) : Parser β := {
  run := fun s => do
    let (a, s') ← p.run s
    return (f a, s'),
  usage := p.usage
}

def pure {α} (x : α) : Parser α := {
  run := fun s => .ok (x, s),
  usage := Usage.empty
}

def seqCore {α β} (pf : Parser (α → β)) (pa : Unit → Parser α) : Parser β :=
  let pa' := pa ()
  {
    run := fun s => do
      let (f, s') ← pf.run s
      let (a, s'') ← pa'.run s'
      return (f a, s''),
    usage := Usage.append pf.usage pa'.usage
  }

def seqLeftCore {α β} (pa : Parser α) (pb : Unit → Parser β) : Parser α :=
  let pb' := pb ()
  {
    run := fun s => do
      let (a, s') ← pa.run s
      let (_, s'') ← pb'.run s'
      return (a, s''),
    usage := Usage.append pa.usage pb'.usage
  }

def seqRightCore {α β} (pa : Parser α) (pb : Unit → Parser β) : Parser β :=
  let pb' := pb ()
  {
    run := fun s => do
      let (_, s') ← pa.run s
      let (b, s'') ← pb'.run s'
      return (b, s''),
    usage := Usage.append pa.usage pb'.usage
  }

instance : Functor Parser where
  map := fun f p => map f p

instance : Pure Parser := ⟨pure⟩

instance : Seq Parser where
  seq := seqCore

instance : SeqLeft Parser where
  seqLeft := seqLeftCore

instance : SeqRight Parser where
  seqRight := seqRightCore

instance : Applicative Parser where
  pure := pure
  map := fun f p => map f p
  seq := Seq.seq
  seqLeft := SeqLeft.seqLeft
  seqRight := SeqRight.seqRight

def optional {α} (p : Parser α) : Parser (Option α) := {
  run := fun s =>
    match p.run s with
    | .ok (a, s') => .ok (some a, s')
    | .error err =>
      if err.kind = .missing then
        .ok (none, s)
      else
        .error err,
  usage := Usage.optional p.usage
}

def withDefault {α} (p : Parser α) (value : α) : Parser α :=
  map (fun opt => opt.getD value) (optional p)

end Parser

structure ValueReader (α : Type u) where
  run : String → Except String α

def ValueReader.id : ValueReader String := ⟨fun s => .ok s⟩

namespace ValueReader

def map {α β} (r : ValueReader α) (f : α → Except String β) : ValueReader β :=
  ⟨fun s => do
    let a ← r.run s
    f a⟩

def nat : ValueReader Nat := ⟨fun s =>
  match s.toNat? with
  | some n => .ok n
  | none => .error s!"expected a natural number, found '{s}'"⟩

def int : ValueReader Int := ⟨fun s =>
  match s.toInt? with
  | some n => .ok n
  | none => .error s!"expected an integer, found '{s}'"⟩

end ValueReader

structure OptionSpec (α : Type u) where
  long? : Option String := none
  short? : Option Char := none
  metavar : String := "VALUE"
  help? : Option String := none
  reader : ValueReader α
  default? : Option α := none
  showDefault? : Option String := none

structure FlagSpec (α : Type u) where
  long? : Option String := none
  short? : Option Char := none
  help? : Option String := none
  default : α
  active : α

structure ArgumentSpec (α : Type u) where
  metavar : String
  help? : Option String := none
  reader : ValueReader α

structure Subcommand (α : Type u) where
  name : String
  description? : Option String := none
  parser : Parser α

structure SubcommandSpec (α : Type u) where
  metavar : String := "COMMAND"
  help? : Option String := none
  commands : List (Subcommand α)

private def renderOptionNames (long? : Option String) (short? : Option Char) : String :=
  let parts :=
    (match short? with
     | some c => ["-" ++ String.mk [c]]
     | none => []) ++
    (match long? with
     | some s => ["--" ++ s]
     | none => [])
  parts.foldl (fun acc part => if acc.isEmpty then part else acc ++ ", " ++ part) ""

private def optionDoc (spec : OptionSpec α) : OptionDoc := {
  long? := spec.long?,
  short? := spec.short?,
  metavar? := some spec.metavar,
  help? := spec.help?,
  required := spec.default?.isNone,
  default? := spec.showDefault?
}

private def flagDoc (spec : FlagSpec α) : OptionDoc := {
  long? := spec.long?,
  short? := spec.short?,
  metavar? := none,
  help? := spec.help?,
  required := false,
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

def switch (long : String) (short? : Option Char := none) (help? : Option String := none) : Parser Bool :=
  flag {
    long? := some long,
    short? := short?,
    help?,
    default := false,
    active := true
  }

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

structure ParserFailure where
  error : ParseError
  usage : Usage
  leftovers : List String := []
  deriving Repr

inductive ParserResult (α : Type u)
  | success (value : α)
  | showHelp
  | failure (err : ParserFailure)
  deriving Repr

structure ParserInfo (α : Type u) where
  progName : String
  parser : Parser α
  header? : Option String := none
  progDesc? : Option String := none
  footer? : Option String := none

namespace ParserInfo

private def synopsisElement (o : OptionDoc) : String :=
  let name := renderOptionNames o.long? o.short?
  let base :=
    match o.metavar? with
    | some mv => if name.isEmpty then mv else name ++ " " ++ mv
    | none => name
  if base.isEmpty then ""
  else if o.required then base else "[" ++ base ++ "]"

private def positionalElement (p : PositionalDoc) : String :=
  if p.required then p.metavar else "[" ++ p.metavar ++ "]"

private def synopsis (u : Usage) : String :=
  let optionParts := u.options.map synopsisElement |>.filter (· ≠ "")
  let positionalParts := u.positionals.map positionalElement
  String.intercalate " " (optionParts ++ positionalParts)

private def formatOptionHeading (o : OptionDoc) : String :=
  let base := renderOptionNames o.long? o.short?
  match o.metavar? with
  | some mv => if base.isEmpty then mv else base ++ " " ++ mv
  | none => base

private def spaces (n : Nat) : String := String.mk (List.replicate n ' ')

private def renderSection (title : String) (rows : List (String × Option String)) : List String :=
  if rows.isEmpty then []
  else
    let leftWidth := rows.foldl (fun acc (name, _) => max acc name.length) 0
    let formatted := rows.map (fun (name, help?) =>
      let pad := leftWidth - name.length
      let padding := spaces pad
      match help? with
      | some text => s!"  {name}{padding}  {text}"
      | none => s!"  {name}{padding}"
    )
    (title :: formatted)

def renderHelp (info : ParserInfo α) (includeHelpOption : Bool := true) : String :=
  let usage := info.parser.usage
  let usageLine :=
    let synopsisStr := synopsis usage
    if synopsisStr.isEmpty then s!"Usage: {info.progName}" else s!"Usage: {info.progName} {synopsisStr}"
  let header := info.header?.toList
  let desc := info.progDesc?.toList
  let optionsDocs :=
    let opts := usage.options
    let extra :=
      if includeHelpOption then
        [{ long? := some "help", short? := some 'h', help? := some "Show this help message", required := false : OptionDoc }]
      else []
    let all := extra ++ opts
    renderSection "Options:" (all.map (fun opt => (formatOptionHeading opt, opt.help?)))
  let argsDocs := renderSection "Arguments:" (usage.positionals.map fun arg => (arg.metavar, arg.help?))
  let commandsDocs :=
    if usage.commands.isEmpty then []
    else
      let rows := usage.commands.map fun cmd => (cmd.name, cmd.description?)
      renderSection "Commands:" rows
  String.intercalate "\n" <|
    header ++ [usageLine] ++ desc ++ optionsDocs ++ argsDocs ++ commandsDocs ++ info.footer?.toList

private def containsHelp (args : List String) : Bool :=
  args.any fun arg => arg = "--help" || arg = "-h"

def exec (info : ParserInfo α) (argv : List String) : ParserResult α :=
  if containsHelp argv then
    ParserResult.showHelp
  else
    let init := ParseState.ofList argv
    match info.parser.run init with
    | .error err =>
      ParserResult.failure { error := err, usage := info.parser.usage }
    | .ok (value, state) =>
      match state.remaining with
      | [] => ParserResult.success value
      | leftovers =>
        let message :=
          s!"Unrecognized arguments: {String.intercalate " " leftovers}"
        let err := {
          kind := .unexpected,
          message,
          context? := none
        }
        ParserResult.failure { error := err, usage := info.parser.usage, leftovers }

def renderFailure (info : ParserInfo α) (failure : ParserFailure) : String :=
  let ctx := failure.error.context?.map (fun c => s!"[{c}] ") |>.getD ""
  let primary := s!"{ctx}{failure.error.message}"
  let notes := failure.error.notes.map (fun n => s!"  note: {n}")
  let details :=
    if failure.leftovers.isEmpty then []
    else [s!"  leftovers: {String.intercalate " " failure.leftovers}"]
  String.intercalate "\n" <|
    primary :: (notes ++ details ++ [renderHelp info false])

end ParserInfo

end LeanArgparse

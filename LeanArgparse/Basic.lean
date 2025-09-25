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

def size (s : ParseState) : Nat :=
  s.front.length + s.tail.length

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

def failure {α} (msg : String := "empty parser") : Parser α :=
  {
    run := fun _ => .error {
      kind := .missing,
      message := msg
    },
    usage := Usage.empty
  }

def orElseCore {α} (p : Parser α) (q : Unit → Parser α) : Parser α :=
  let q' := q ()
  {
    run := fun s =>
      match p.run s with
      | .ok result => .ok result
      | .error err =>
        if err.kind = .missing then
          q'.run s
        else
          .error err,
    usage := Usage.optional (Usage.append p.usage q'.usage)
  }

def orElse {α} (p q : Parser α) : Parser α :=
  orElseCore p (fun _ => q)

instance : Applicative Parser where
  pure := pure
  map := fun f p => map f p
  seq := Seq.seq
  seqLeft := SeqLeft.seqLeft
  seqRight := SeqRight.seqRight

instance : Alternative Parser where
  failure := failure
  orElse := fun p q => orElseCore p q

def many {α} (p : Parser α) : Parser (List α) :=
  {
    run := fun s =>
      let rec loop : Nat → ParseState → List α → Except ParseError (List α × ParseState)
        | 0, state, acc => .ok (acc.reverse, state)
        | Nat.succ fuel, state, acc =>
          match p.run state with
          | .ok (value, state') => loop fuel state' (value :: acc)
          | .error err =>
            if err.kind = .missing then
              .ok (acc.reverse, state)
            else
              .error err
      loop (ParseState.size s) s []
    , usage := Usage.optional p.usage
  }

def some {α} (p : Parser α) : Parser (List α) := {
  run := fun s => do
    let (head, s') ← p.run s
    let (tail, s'') ← (many p).run s'
    return (head :: tail, s'')
  , usage := Usage.append p.usage (many p).usage
}

def many1 {α} (p : Parser α) : Parser (List α) := some p

def some1 {α} (p : Parser α) : Parser (List α) := some p

def optionalOrElse {α} (p : Parser α) (backup : Unit → Parser α) : Parser α :=
  orElseCore p backup

def choice {α} : List (Parser α) → Parser α
  | [] => failure "empty choice"
  | p :: ps => ps.foldl (fun acc next => orElse acc next) p

def optional {α} (p : Parser α) : Parser (Option α) := {
  run := fun s =>
    match p.run s with
    | .ok (a, s') => .ok (Option.some a, s')
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

namespace OptionSpec

abbrev Mod (α : Type u) := OptionSpec α → OptionSpec α

def base (reader : ValueReader α) : OptionSpec α := { reader := reader }

def applyMods (mods : List (Mod α)) (spec : OptionSpec α) : OptionSpec α :=
  mods.foldl (fun acc f => f acc) spec

def build (reader : ValueReader α) (mods : List (Mod α)) : OptionSpec α :=
  applyMods mods (base reader)

def long (name : String) : Mod α := fun spec => { spec with long? := some name }

def short (c : Char) : Mod α := fun spec => { spec with short? := some c }

def setMetavar (name : String) : Mod α := fun spec => { spec with metavar := name }

def help (text : String) : Mod α := fun spec => { spec with help? := some text }

def default (value : α) : Mod α := fun spec => { spec with default? := some value }

def showDefault (value : String) : Mod α := fun spec => { spec with showDefault? := some value }

end OptionSpec

structure FlagSpec (α : Type u) where
  long? : Option String := none
  short? : Option Char := none
  help? : Option String := none
  default : α
  active : α

namespace FlagSpec

abbrev Mod (α : Type u) := FlagSpec α → FlagSpec α

def base (default active : α) : FlagSpec α :=
  { default := default, active := active }

def applyMods (mods : List (Mod α)) (spec : FlagSpec α) : FlagSpec α :=
  mods.foldl (fun acc f => f acc) spec

def build (default active : α) (mods : List (Mod α)) : FlagSpec α :=
  applyMods mods (base default active)

def long (name : String) : Mod α := fun spec => { spec with long? := some name }

def short (c : Char) : Mod α := fun spec => { spec with short? := some c }

def help (text : String) : Mod α := fun spec => { spec with help? := some text }

end FlagSpec

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

abbrev InfoMod (α : Type u) := ParserInfo α → ParserInfo α

def withProgName (name : String) (info : ParserInfo α) : ParserInfo α :=
  { info with progName := name }

def withHeader (text : String) (info : ParserInfo α) : ParserInfo α :=
  { info with header? := some text }

def withProgDesc (text : String) (info : ParserInfo α) : ParserInfo α :=
  { info with progDesc? := some text }

def withFooter (text : String) (info : ParserInfo α) : ParserInfo α :=
  { info with footer? := some text }

def applyMods (mods : List (InfoMod α)) (info : ParserInfo α) : ParserInfo α :=
  mods.foldl (fun acc f => f acc) info

def build (parser : Parser α) (mods : List (InfoMod α)) : ParserInfo α :=
  applyMods mods { progName := "", parser }

structure CompletionOption where
  long? : Option String := none
  short? : Option Char := none
  metavar? : Option String := none
  takesValue : Bool := false
  help? : Option String := none
  deriving Repr

structure CompletionData where
  progName : String
  options : List CompletionOption := []
  commands : List (String × Option String) := []
  deriving Repr

namespace Completion

open Std

structure OptionKey where
  long? : Option String
  short? : Option Char
  takesValue : Bool
  deriving BEq, Hashable

structure Module where
  name : String
  render : CompletionData → String

private def fromDoc (doc : OptionDoc) : CompletionOption :=
  {
    long? := doc.long?,
    short? := doc.short?,
    metavar? := doc.metavar?,
    takesValue := doc.metavar?.isSome,
    help? := doc.help?
  }

private def usageOptions (u : Usage) : List CompletionOption :=
  let docs := u.options
  let initial : Array CompletionOption × HashSet OptionKey :=
    (Array.mkEmpty docs.length, HashSet.emptyWithCapacity docs.length)
  let (acc, _) := docs.foldl
    (fun (acc, seen) doc =>
      let opt := fromDoc doc
      let key : OptionKey := {
        long? := opt.long?,
        short? := opt.short?,
        takesValue := opt.takesValue
      }
      if seen.contains key then
        (acc, seen)
      else
        (acc.push opt, seen.insert key))
    initial
  acc.toList

private def usageCommands (u : Usage) : List (String × Option String) :=
  let cmds := u.commands
  let initial : Array (String × Option String) × HashSet String :=
    (Array.mkEmpty cmds.length, HashSet.emptyWithCapacity cmds.length)
  let (acc, _) := cmds.foldl
    (fun (acc, seen) cmd =>
      if seen.contains cmd.name then
        (acc, seen)
      else
        (acc.push (cmd.name, cmd.description?), seen.insert cmd.name))
    initial
  acc.toList

private def escapeSingleQuotes (s : String) : String :=
  s.replace "'" "'\\''"

private def sanitizeIdentifier (s : String) : String :=
  let chars := s.data.map fun c => if c.isAlphanum || c = '_' then c else '_'
  let result := String.mk chars
  if result.isEmpty then "program" else result

private def ensureHelpOption (opts : List CompletionOption) : List CompletionOption :=
  if opts.any (fun opt => opt.long? = some "help" || opt.short? = some 'h') then
    opts
  else
    opts ++ [{
      long? := some "help",
      short? := some 'h',
      metavar? := none,
      takesValue := false,
      help? := some "Show this help message"
    }]

private def buildData (info : ParserInfo α) : CompletionData :=
  let usage := info.parser.usage
  {
    progName := if info.progName.isEmpty then "program" else info.progName,
    options := ensureHelpOption (usageOptions usage),
    commands := usageCommands usage
  }

private def optionLong (opt : CompletionOption) : Option String :=
  opt.long?.map fun n => s!"--{n}"

private def optionShort (opt : CompletionOption) : Option String :=
  opt.short?.map fun c => s!"-{String.mk [c]}"

private def optionFlags (opt : CompletionOption) : List String :=
  let longs := match optionLong opt with
    | some flag => [flag]
    | none => []
  let shorts := match optionShort opt with
    | some flag => [flag]
    | none => []
  longs ++ shorts

def Module.renderInfo (self : Module) (info : ParserInfo α) : String :=
  self.render (buildData info)

private def renderBash (d : CompletionData) : String :=
  let ident := sanitizeIdentifier d.progName
  let functionName := s!"_{ident}_completion"
  let longOpts := d.options.filterMap optionLong
  let shortOpts := d.options.filterMap optionShort
  let commands := d.commands.map Prod.fst
  let valuedLong := d.options.filterMap (fun opt => if opt.takesValue then optionLong opt else none)
  let valuedShort := d.options.filterMap (fun opt => if opt.takesValue then optionShort opt else none)
  let joinWords (ws : List String) := String.intercalate " " ws
  let longWords := joinWords longOpts
  let shortWords := joinWords shortOpts
  let commandWords := joinWords commands
  let valuedWords := joinWords (valuedLong ++ valuedShort)
  let combinedWords := joinWords ((longOpts ++ shortOpts ++ commands).eraseDups)
  let startLine := functionName ++ "() {"
  let lines : List String :=
    [startLine,
     "  local cur prev",
     "  COMPREPLY=()",
     "  cur=\"${COMP_WORDS[COMP_CWORD]}\"",
     "  prev=\"${COMP_WORDS[COMP_CWORD-1]}\"",
     s!"  local opts_long=\"{longWords}\"",
     s!"  local opts_short=\"{shortWords}\"",
     s!"  local commands=\"{commandWords}\"",
     s!"  local all_words=\"{combinedWords}\"",
     s!"  local value_opts=\"{valuedWords}\"",
     "  for opt in $value_opts; do",
     "    if [[ $prev == $opt ]]; then",
     "      return 0",
     "    fi",
     "  done",
     "  if [[ $cur == --* ]]; then",
     "    COMPREPLY=( $(compgen -W \"$opts_long\" -- \"$cur\") )",
     "    return 0",
     "  fi",
     "  if [[ $cur == -* ]]; then",
     "    COMPREPLY=( $(compgen -W \"$opts_short\" -- \"$cur\") )",
     "    return 0",
     "  fi",
     "  COMPREPLY=( $(compgen -W \"$all_words\" -- \"$cur\") )",
     "  return 0",
     "}",
     s!"complete -F {functionName} {d.progName}"]
  String.intercalate "\n" lines

private def renderZsh (d : CompletionData) : String :=
  let optionEntries := d.options.foldr
    (fun opt acc =>
      let flags := optionFlags opt
      let entries := flags.map fun flag =>
        let desc := opt.help?.map escapeSingleQuotes |>.getD ""
        let descPart := if desc.isEmpty then "" else s!"[{desc}]"
        let metavar := opt.metavar?.map escapeSingleQuotes |>.getD "VALUE"
        let argPart := if opt.takesValue then s!":{metavar}:" else ""
        s!"'{flag}{descPart}{argPart}'"
      entries ++ acc)
    []
  let argumentLine :=
    String.intercalate " " (optionEntries ++ ["'*::arg:->args'"])
  let baseLines : List String :=
    [s!"#compdef {d.progName}",
     "local context state state_descr line",
     "typeset -A opt_args",
     s!"_arguments -s {argumentLine}"]
  let commandLines :=
    if d.commands.isEmpty then []
    else
      let entries := d.commands.map fun (name, desc?) =>
        let desc := desc?.map escapeSingleQuotes |>.getD ""
        if desc.isEmpty then s!"{name}" else s!"{name}:{desc}"
      ["case $state in",
       "  args)",
       s!"    _values 'commands' {String.intercalate " " entries}",
       "  ;;",
       "esac"]
  String.intercalate "\n" (baseLines ++ commandLines)

private def renderFish (d : CompletionData) : String :=
  let optionLines := d.options.map fun opt =>
    let base := s!"complete -c {d.progName}"
    let base := match opt.long? with
      | some long => s!"{base} -l {long}"
      | none => base
    let base := match opt.short? with
      | some c => s!"{base} -s {c}"
      | none => base
    let base := if opt.takesValue then s!"{base} -r" else base
    let base := match opt.help? with
      | some desc => s!"{base} -d '{escapeSingleQuotes desc}'"
      | none => base
    base
  let commandLines := d.commands.map fun (name, desc?) =>
    let descPart := desc?.map escapeSingleQuotes |>.map (fun d => s!" -d '{d}'") |>.getD ""
    s!"complete -c {d.progName} -n '__fish_use_subcommand' -f -a '{escapeSingleQuotes name}'{descPart}"
  String.intercalate "\n" (optionLines ++ commandLines)

def renderWithModule (mod : Module) (info : ParserInfo α) : String :=
  mod.render (buildData info)

end Completion

open Completion

def bashModule : Completion.Module := { name := "bash", render := renderBash }

def zshModule : Completion.Module := { name := "zsh", render := renderZsh }

def fishModule : Completion.Module := { name := "fish", render := renderFish }

def allCompletionModules : List Completion.Module := [bashModule, zshModule, fishModule]

def renderBashCompletion (info : ParserInfo α) : String :=
  renderWithModule bashModule info

def renderZshCompletion (info : ParserInfo α) : String :=
  renderWithModule zshModule info

def renderFishCompletion (info : ParserInfo α) : String :=
  renderWithModule fishModule info

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

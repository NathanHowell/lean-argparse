import ArgParse.Cmd
import ArgParse.Doc.Help
import ArgParse.Doc.Man
import ArgParse.Doc.Completion
import ArgParse.Core.Normalize

/-!
# ArgParse.Exec

The runner. It owns everything an application would otherwise have to write for
itself: `--help` at every level, `--version`, `--man`, completion, the usage
synopsis, and error text with suggestions.

Applications contain zero help code. That is the acceptance criterion for the
whole design: if an application needs to render its own help, a layer below has
failed.
-/

namespace ArgParse

open ArgParse.Spec

/-- What running an application produced. -/
inductive ExecResult (α : Type) where
  /-- A parsed payload. -/
  | ok (value : α)
  /-- Text the runner produced on the user's behalf: help, version, man, or
  completions. Belongs on stdout with a success exit code. -/
  | output (text : String)
  /-- A failure, already rendered with usage. Belongs on stderr. -/
  | error (text : String)
  deriving Repr

namespace Exec

/-- Runner settings. The lexemes are configurable because an application may
already use `-h` or `--version` for something of its own. -/
structure Config where
  /-- Version string reported by `--version`; the flag is absent when this is. -/
  version? : Option String := none
  /-- Text appended to the end of every help page. -/
  epilog? : Option String := none
  /-- Lexemes that request help. -/
  helpFlags : List String := ["-h", "--help"]
  /-- Lexemes that request the version. -/
  versionFlags : List String := ["--version"]
  /-- Lexemes that request a man page; empty disables it. -/
  manFlags : List String := ["--man"]
  /-- Lexemes that request completion candidates; empty disables them. -/
  completionFlags : List String := ["--generate-completions"]

/-- Items the runner contributes to every options table. They are documented
here and implemented here, so they cannot drift from each other either. -/
def runnerItems (cfg : Config) : List ItemSpec :=
  let ofFlags (flags : List String) (name descr : String) : List ItemSpec :=
    match flags with
    | [] => []
    | _ =>
        [{ kind := .flag
         , name := name
         , short? := flags.findSome? (fun f =>
             if f.length == 2 && f.startsWith "-" && !f.startsWith "--" then f.toList[1]? else none)
         , long? := flags.findSome? (fun f =>
             if f.startsWith "--" then some (f.drop 2).toString else none)
         , help? := some descr
         , arity := .zero
         , required := false }]
  ofFlags cfg.helpFlags "help" "Show this help text and exit."
    ++ (match cfg.version? with
        | none => []
        | some _ => ofFlags cfg.versionFlags "version" "Show the version and exit.")
    ++ ofFlags cfg.manFlags "man" "Print a man page and exit."
    ++ ofFlags cfg.completionFlags "generate-completions" "List completion candidates and exit."

/-! ### Suggestions

An unrecognised lexeme is much more often a typo than a novelty, so the error
text names the nearest thing the command does accept. -/

/-- One row of the Levenshtein DP: distances from `ca` against each prefix of
`bs`, given the previous row and this row's leading value. -/
private def rowStep (ca : Char) : List Char → List Nat → Nat → List Nat
  | [], _, cur => [cur]
  | cb :: bt, prev, cur =>
      match prev with
      | prevDiag :: prevRest =>
          let prevHere := prevRest.headD (prevDiag + 1)
          let cost := if ca == cb then 0 else 1
          cur :: rowStep ca bt prevRest (min (min (prevHere + 1) (cur + 1)) (prevDiag + cost))
      | [] => [cur]

/-- Fold `rowStep` over the characters of the first string. -/
private def rowFold (bs : List Char) : List Char → Nat → List Nat → List Nat
  | [], _, prev => prev
  | ca :: rest, i, prev => rowFold bs rest (i + 1) (rowStep ca bs prev (i + 1))

/-- Levenshtein distance between two strings. -/
def editDistance (a b : String) : Nat :=
  let bs := b.toList
  (rowFold bs a.toList 0 (List.range (bs.length + 1))).getLast?.getD 0

/-- The candidate nearest `token`, when one is near enough to be worth naming.

The threshold is two for anything longer than three characters. That is wider
than it looks necessary: a transposition -- `chidl` for `child`, the single most
common typo -- costs two under plain Levenshtein, so a threshold of one misses
exactly the case suggestions exist for. Below four characters two edits reach a
genuinely different word, so the threshold drops to one. -/
def nearest? (token : String) (candidates : List String) : Option String :=
  let threshold := if token.length ≤ 3 then 1 else 2
  let scored := candidates.map (fun c => (editDistance token c, c))
  let best := scored.foldl
    (fun best entry =>
      match best with
      | Option.none => Option.some entry
      | Option.some current => if entry.fst < current.fst then Option.some entry else best)
    Option.none
  match best with
  | Option.some (distance, candidate) => if distance ≤ threshold then Option.some candidate else Option.none
  | Option.none => Option.none

/-- `; did you mean X?`, or nothing. -/
def didYouMean (token : String) (candidates : List String) : String :=
  match nearest? token candidates with
  | Option.some candidate => s!"; did you mean `{candidate}`?"
  | Option.none => ""

/-! ### Error rendering -/

/-- Human-readable form of a parser expectation. -/
def renderExpect : Expect → String
  | .flag short? long? =>
      let names := (long?.toList.map (fun n => "--" ++ n)) ++
        (short?.toList.map (fun c => "-" ++ String.singleton c))
      String.intercalate " or " names
  | .optionVal name => s!"a value for `--{name}`"
  | .positional name => s!"the argument {name}"
  | .subcommand name => s!"the command `{name}`"
  | .endOfInput => "no further arguments"

/-- The thing an expectation is about, for use where the sentence already
supplies the verb. -/
def expectTarget : Expect → String
  | .flag short? long? =>
      match long?, short? with
      | some name, _ => s!"`--{name}`"
      | none, some c => s!"`-{c}`"
      | none, none => "the flag"
  | .optionVal name => s!"`--{name}`"
  | .positional name => name
  | .subcommand name => s!"`{name}`"
  | .endOfInput => "the end of the arguments"

/-- Render a structured parse error against the command it came from. -/
def renderError (path : List String) (cmd : CmdSpec) (candidates : List String)
    (err : Error) : String :=
  let token? := err.context.head?
  let headline :=
    match err.kind with
    | .unknownLong | .unknownShort =>
        match token? with
        | Option.some token => s!"unrecognised `{token}`{didYouMean token candidates}"
        | Option.none => "unrecognised argument"
    | .missingValue =>
        match err.expect with
        | [] => "missing a required argument"
        | expects => s!"missing {String.intercalate ", " (expects.map renderExpect)}"
    | .leftover =>
        match token? with
        | Option.some token => s!"unexpected `{token}`{didYouMean token candidates}"
        | Option.none => "unexpected extra arguments"
    | .conflict => "conflicting arguments"
    | .custom =>
        match token?, err.expect with
        | Option.some token, expect :: _ => s!"invalid value `{token}` for {expectTarget expect}"
        | Option.some token, [] => s!"invalid value `{token}`"
        | Option.none, _ => "invalid argument"
  let expectLine :=
    match err.kind, err.expect with
    | .missingValue, _ => []
    | .custom, _ => []
    | _, [] => []
    | _, expects => [s!"expected {String.intercalate ", " (expects.map renderExpect)}"]
  String.intercalate "\n"
    ([s!"error: {headline}"] ++ expectLine
      ++ ["", "Usage:", Doc.usageLine path cmd, ""
         , s!"For more information, try `{String.intercalate " " path} --help`."])

/-! ### Running -/

/-- Whether any of `flags` appears in the pre-sentinel stream.

Only whole tokens count: a builtin bundled into `-vh` is not detected, because
resolving a bundle needs the flag specs of the command the tokens belong to,
which is not known until dispatch has happened. Spelling it `-h` works. -/
@[inline] def requested (flags : List String) (tokens : List String) : Bool :=
  flags.any tokens.contains

/-- Tokens following the first occurrence of any of `flags`. -/
def wordsAfter (flags : List String) : List String → List String
  | [] => []
  | token :: rest => if flags.contains token then rest else wordsAfter flags rest

/-- The first `--name` in the stream that no item on the path accepts.

Scanning leaves an unrecognised option in place, and a positional will then
happily consume it -- so `greet --coutn 2 Alice` would otherwise be reported as
an unexpected `2`, naming the wrong token entirely. Checking the stream against
the items actually legal here recovers the diagnosis the user needs.

Only long forms are checked. A short token may be a bundle (`-vf`) or a negative
number standing in as a value (`-5`), and telling those apart needs the arity
information that dispatch has not established yet at this point. -/
def unknownLong? (known : List String) (tokens : List String) : Option String :=
  tokens.findSome? fun token =>
    if token.startsWith "--" && token != "--" then
      let name := (token.splitOn "=").headD token
      if known.contains name then Option.none else Option.some name
    else
      Option.none

/-- Whether this error is dispatch complaining about a token that was meant to
be a verb.

A misspelled verb makes every token after it illegal, because they were meant
for a command that was never reached. Diagnosing one of *those* instead names
the wrong token: `ci scop --tier pr` is a misspelling of `scope`, not a problem
with `--tier`. So a dispatch failure on a token that does not look like an
option outranks any unknown-option finding.

The token must not start with `-`: dispatch also fails when an option appears
where a verb belongs (`ci --tier pr`), and there the option is genuinely the
thing to report. -/
def isUnknownVerb (err : Error) : Bool :=
  err.expect.any (fun e => match e with | .subcommand _ => true | _ => false)
    && (match err.context.head? with
        | Option.some token => !token.startsWith "-"
        | Option.none => false)

/-- Everything the command at `path` accepts, for suggestion purposes. -/
def candidatesAt (cmd : CmdSpec) (cfg : Config) : List String :=
  Doc.candidatesFor cmd ++ (runnerItems cfg).flatMap (·.lexemes)

/-- Run an application against raw argv.

Builtins are checked against the pre-sentinel tokens before parsing, and help is
rendered for the deepest command the tokens name, so `app sub --help` documents
`sub` rather than the root. -/
def exec (app : Cmd α) (argv : List String) (cfg : Config := {}) : ExecResult α :=
  let st := Core.normalize argv
  let spec := app.toAppSpec cfg.version? cfg.epilog?
  let (path, here) := app.descend st.pre
  let hereSpec := here.toCmdSpec
  if requested cfg.helpFlags st.pre then
    let inherited := (Doc.pathItems spec.root st.pre).filter (fun i => !hereSpec.args.contains i)
    .output (Doc.renderCommandHelp path hereSpec (runnerItems cfg)
      (if path.length == 1 then spec.about? else none) cfg.epilog? inherited)
  else
    match cfg.version? with
    | some version =>
        if requested cfg.versionFlags st.pre then
          .output s!"{app.name} {version}"
        else
          execParse app spec cfg st path hereSpec
    | none => execParse app spec cfg st path hereSpec
where
  /-- The non-help path: man and completions, then the application's own parse. -/
  execParse (app : Cmd α) (spec : AppSpec) (cfg : Config) (st : State)
      (path : List String) (hereSpec : CmdSpec) : ExecResult α :=
    if requested cfg.manFlags st.pre then
      .output (Doc.renderMan spec)
    else if requested cfg.completionFlags st.pre then
      .output (Doc.renderCompletion spec (wordsAfter cfg.completionFlags st.pre))
    else
      let candidates := candidatesAt hereSpec cfg
      let legal :=
        (Doc.pathItems spec.root st.pre).flatMap (·.lexemes)
          ++ (runnerItems cfg).flatMap (·.lexemes)
      let fail (err : Error) : ExecResult α :=
        if isUnknownVerb err then
          -- The earliest wrong token wins: report the verb, not the options it
          -- stranded.
          .error (renderError path hereSpec candidates err)
        else
          match unknownLong? legal st.pre with
          | Option.some name =>
              .error (renderError path hereSpec candidates
                { kind := .unknownLong, context := [name], expect := [] })
          | Option.none => .error (renderError path hereSpec candidates err)
      match app.toParser st with
      | .err err => fail err
      | .ok value st' =>
          let leftover := st'.pre ++ st'.post
          if leftover.isEmpty then
            .ok value
          else
            fail { kind := .leftover, context := leftover, expect := [.endOfInput] }

end Exec

/-- Run an application and dispatch on the result, the shape `main` wants.

Output goes to stdout with exit code 0, errors to stderr with exit code 2, and a
parsed payload to the supplied handler. -/
def run (app : Cmd α) (argv : List String) (handler : α → IO UInt32)
    (cfg : Exec.Config := {}) : IO UInt32 :=
  match Exec.exec app argv cfg with
  | .ok value => handler value
  | .output text => do IO.println text; pure 0
  | .error text => do IO.eprintln text; pure 2

end ArgParse

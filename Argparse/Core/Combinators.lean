import Argparse.Core.Parser
import Argparse.Core.Value
import Argparse.Core.Normalize
import Argparse.Spec.AST

/-!
# ArgParse.Core.Combinators

Baseline flag/option/positional parsers operating on the core `State`.
-/

namespace ArgParse.Core

open ArgParse
open ArgParse.Spec

/-!
Helpers
-/

namespace State

/-- Remove the next token from `pre`, updating the cursor. -/
def consumePre? (st : State) : Option (String × State) :=
  match st.pre with
  | [] => none
  | tok :: rest =>
      let st' : State := { st with pre := rest, cursor := st.cursor + 1 }
      some (tok, st')

/-- Remove the next token from `post`, updating the cursor. -/
def consumePost? (st : State) : Option (String × State) :=
  match st.post with
  | [] => none
  | tok :: rest =>
      let st' : State := { st with post := rest, cursor := st.cursor + 1 }
      some (tok, st')

end State

private def shortLexeme (short : Short) : String :=
  "-" ++ String.singleton short.c

private def longLexeme (name : String) : String :=
  "--" ++ name

private def expectFlag (spec : FlagSpec) : Expect :=
  .flag (spec.short?.map (·.c)) spec.long?

private def expectOption (spec : OptSpec α) : Expect :=
  .optionVal spec.meta.name

private def missingValueError (token : String) (expect : Expect) : Error :=
  { kind := .missingValue, context := [token], expect := [expect] }

private def invalidValueError (token msg : String) (expect : Expect) : Error :=
  { kind := .custom, context := [token], expect := [expect] }

/-- Determine whether the token matches the flag specification. -/
private def matchFlagToken (spec : FlagSpec) (token : String) : Bool :=
  let shortMatch := spec.short?.map (fun s => token = shortLexeme s) |>.getD False
  let longMatch := spec.long?.map (fun name => token = longLexeme name) |>.getD False
  shortMatch || longMatch

/-- Parser for boolean flags; returns `true` when the next token matches. -/
def flag (spec : FlagSpec) : Parser Bool := fun st =>
  match st.pre with
  | token :: rest =>
      if matchFlagToken spec token then
        let st' : State := { st with pre := rest, cursor := st.cursor + 1 }
        .ok true st'
      else
        .ok false st
  | [] => .ok false st

/-- Attempt to pull an option value when the current token matches the long form. -/
private def parseLongOption
    {α} [FromArg α] (spec : OptSpec α) (token : String) (st : State) :
    Except Error (Option α × State) :=
  match spec.long? with
  | none => .ok (none, st)
  | some name =>
      let prefix := longLexeme name
      let expect := expectOption spec
      if token = prefix then
        match State.consumePre? st with
        | some (valueTok, st') =>
            match FromArg.run valueTok with
            | .ok value => .ok (some value, st')
            | .error msg => .error (invalidValueError valueTok msg expect)
        | none => .error (missingValueError token expect)
      else if spec.eqVal? then
        let eqPrefix := prefix ++ "="
        if token.startsWith eqPrefix then
          let raw := token.drop eqPrefix.length
          match FromArg.run raw with
          | .ok value => .ok (some value, { st with cursor := st.cursor + 1 })
          | .error msg => .error (invalidValueError raw msg expect)
        else
          .ok (none, st)
      else
        .ok (none, st)

/-- Attempt to pull an option value when the current token matches the short form. -/
private def parseShortOption
    {α} [FromArg α] (spec : OptSpec α) (token : String) (st : State) :
    Except Error (Option α × State) :=
  match spec.short? with
  | none => .ok (none, st)
  | some short =>
      let prefix := shortLexeme short
      let expect := expectOption spec
      if token = prefix then
        match State.consumePre? st with
        | some (valueTok, st') =>
            match FromArg.run valueTok with
            | .ok value => .ok (some value, st')
            | .error msg => .error (invalidValueError valueTok msg expect)
        | none => .error (missingValueError token expect)
      else if spec.concatVal? ∧ token.startsWith prefix then
        let raw := token.drop prefix.length
        match FromArg.run raw with
        | .ok value => .ok (some value, { st with cursor := st.cursor + 1 })
        | .error msg => .error (invalidValueError raw msg expect)
      else
        .ok (none, st)

/-- Parser for single-valued options. Returns `some` when the next token matches; otherwise `none`. -/
def option {α} [FromArg α] (spec : OptSpec α) : Parser (Option α) := fun st =>
  match State.consumePre? st with
  | none => .ok none st
  | some (token, stAfterFlag) =>
      let attemptLong := parseLongOption spec token stAfterFlag
      match attemptLong with
      | .ok (some value, st') => .ok (some value) st'
      | .ok (none, st') =>
          -- Long form did not match; attempt short form without consuming additional tokens.
          match parseShortOption spec token stAfterFlag with
          | .ok (some value, st'') => .ok (some value) st''
          | .ok (none, _) =>
              -- Token did not match option spec; restore original state.
              .ok none st
          | .error err => .err err
      | .error err => .err err

/-- Parser for positional arguments: consumes from `pre` first, then `post`. -/
def positional {α} [FromArg α] (spec : PosSpec α) : Parser (Option α) := fun st =>
  let expect := Expect.positional spec.meta.name
  match State.consumePre? st with
  | some (token, st') =>
      match FromArg.run token with
      | .ok value => .ok (some value) st'
      | .error msg =>
          .err (invalidValueError token msg expect)
  | none =>
      match State.consumePost? st with
      | some (token, st') =>
          match FromArg.run token with
          | .ok value => .ok (some value) st'
          | .error msg =>
              .err (invalidValueError token msg expect)
      | none => .ok none st

end ArgParse.Core

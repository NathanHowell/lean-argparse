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

private def missingOptionError (spec : OptSpec α) : Error :=
  { kind := .missingValue, context := [], expect := [expectOption spec] }

/-- Determine whether the token matches the flag specification. -/
inductive FlagMatch
  | none
  | short
  | shortBundled (rest : String)
  | long

private def matchFlagToken (spec : FlagSpec) (token : String) : FlagMatch :=
  match spec.long? with
  | some name =>
      if token = longLexeme name then
        FlagMatch.long
      else
        match spec.short? with
        | some short =>
            let prefix := shortLexeme short
            if token = prefix then
              FlagMatch.short
            else if token.startsWith prefix then
              let rest := token.drop prefix.length
              if rest.isEmpty then
                FlagMatch.short
              else if token.startsWith "--" then
                FlagMatch.none
              else
                FlagMatch.shortBundled rest
            else
              FlagMatch.none
        | none => FlagMatch.none
  | none =>
      match spec.short? with
      | some short =>
          let prefix := shortLexeme short
          if token = prefix then
            FlagMatch.short
          else if token.startsWith prefix then
            let rest := token.drop prefix.length
            if rest.isEmpty then
              FlagMatch.short
            else if token.startsWith "--" then
              FlagMatch.none
            else
              FlagMatch.shortBundled rest
          else
            FlagMatch.none
      | none => FlagMatch.none

/-- Parser for boolean flags; returns `true` when the next token matches. -/
def flag (spec : FlagSpec) : Parser Bool := fun st =>
  match st.pre with
  | token :: rest =>
      match matchFlagToken spec token with
      | .none => .ok false st
      | .short =>
          let st' : State := { st with pre := rest, cursor := st.cursor + 1 }
          .ok true st'
      | .long =>
          let st' : State := { st with pre := rest, cursor := st.cursor + 1 }
          .ok true st'
      | .shortBundled tail =>
          let remainder := "-" ++ tail
          let st' : State := { st with pre := remainder :: rest, cursor := st.cursor + 1 }
          .ok true st'
  | [] => .ok false st

private def takeOptionValue?
    {α} [FromArg α] (spec : OptSpec α) (st : State) :
    Except Error (Option α × State) :=
  match st.pre with
  | [] => .ok (none, st)
  | token :: rest =>
      let expect := expectOption spec
      match spec.long? with
      | some name =>
          let prefix := longLexeme name
          let stAfter := { st with pre := rest, cursor := st.cursor + 1 }
          let eqPrefix := prefix ++ "="
          if spec.eqVal? ∧ token.startsWith eqPrefix then
            let raw := token.drop eqPrefix.length
            if raw.isEmpty then
              .error (missingValueError token expect)
            else
              match FromArg.run raw with
              | .ok value => .ok (some value, stAfter)
              | .error msg => .error (invalidValueError raw msg expect)
          else if token = prefix then
            match rest with
            | valueTok :: restTail =>
                match FromArg.run valueTok with
                | .ok value =>
                    let st' : State := { st with pre := restTail, cursor := st.cursor + 2 }
                    .ok (some value, st')
                | .error msg => .error (invalidValueError valueTok msg expect)
            | [] => .error (missingValueError token expect)
          else
            -- fall through to short handling
            match spec.short? with
            | some short =>
                let prefixShort := shortLexeme short
                if token = prefixShort then
                  match rest with
                  | valueTok :: restTail =>
                      match FromArg.run valueTok with
                      | .ok value =>
                          let st' : State := { st with pre := restTail, cursor := st.cursor + 2 }
                          .ok (some value, st')
                      | .error msg => .error (invalidValueError valueTok msg expect)
                  | [] => .error (missingValueError token expect)
                else if spec.concatVal? ∧ token.startsWith prefixShort then
                  let raw := token.drop prefixShort.length
                  if raw.isEmpty then
                    .error (missingValueError token expect)
                  else
                    match FromArg.run raw with
                    | .ok value => .ok (some value, stAfter)
                    | .error msg => .error (invalidValueError raw msg expect)
                else
                  .ok (none, st)
            | none => .ok (none, st)
      | none =>
          match spec.short? with
          | some short =>
              let prefix := shortLexeme short
              let stAfter := { st with pre := rest, cursor := st.cursor + 1 }
              if token = prefix then
                match rest with
                | valueTok :: restTail =>
                    match FromArg.run valueTok with
                    | .ok value =>
                        let st' : State := { st with pre := restTail, cursor := st.cursor + 2 }
                        .ok (some value, st')
                    | .error msg => .error (invalidValueError valueTok msg expect)
                | [] => .error (missingValueError token expect)
              else if spec.concatVal? ∧ token.startsWith prefix then
                let raw := token.drop prefix.length
                if raw.isEmpty then
                  .error (missingValueError token expect)
                else
                  match FromArg.run raw with
                  | .ok value => .ok (some value, stAfter)
                  | .error msg => .error (invalidValueError raw msg expect)
              else
                .ok (none, st)
          | none => .ok (none, st)

private def collectOptionValues
    {α} [FromArg α] (spec : OptSpec α) (st : State) : Except Error (List α × State) :=
  let rec loop (acc : List α) (curr : State) : Except Error (List α × State) :=
    match takeOptionValue? spec curr with
    | .error err => .error err
    | .ok (some value, nextState) => loop (value :: acc) nextState
    | .ok (none, nextState) => .ok (acc.reverse, nextState)
  loop [] st

/-- Parser for options supporting `.one`/`.many`/`.some` arities. -/
def option {α} [FromArg α] (spec : OptSpec α) :
    Parser (match spec.arity with
      | .zero => PUnit
      | .one  => Option α
      | .many => List α
      | .some => List α) := fun st =>
  match spec.arity with
  | .zero => .ok PUnit.unit st
  | .one =>
      match takeOptionValue? spec st with
      | .error err => .err err
      | .ok (value?, st') => .ok value? st'
  | .many =>
      match collectOptionValues spec st with
      | .error err => .err err
      | .ok (values, st') => .ok values st'
  | .some =>
      match collectOptionValues spec st with
      | .error err => .err err
      | .ok (values, st') =>
          match values with
          | [] => .err (missingOptionError spec)
          | _ => .ok values st'

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

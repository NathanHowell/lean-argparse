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

private def expectPositional (spec : PosSpec α) : Expect :=
  .positional spec.meta.name

private def stringTake (s : String) (n : Nat) : String :=
  String.mk (s.data.take n)

private def stringDrop (s : String) (n : Nat) : String :=
  String.mk (s.data.drop n)

private def findConcatSplit? {α} [FromArg α] (raw : String) : Option (α × String) :=
  let candidates := ((List.range raw.length).drop 1).reverse
  let rec loop : List Nat → Option (α × String)
    | [] => none
    | idx :: rest =>
        let prefix := stringTake raw idx
        let suffix := stringDrop raw idx
        if suffix.isEmpty then
          loop rest
        else
          match FromArg.run prefix with
          | .ok value => some (value, suffix)
          | .error _ => loop rest
  loop candidates

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

@[inline] def parseConcatValue
    {α} [FromArg α] (spec : OptSpec α) (token raw : String)
    (pending : List String) (st : State) (expect : Expect) :
    Except Error (Option α × State) :=
  if raw = "" then
    .error (missingValueError token expect)
  else
    let stAfter := { st with pre := pending, cursor := st.cursor + 1 }
    match FromArg.run raw with
    | .ok value => .ok (some value, stAfter)
    | .error msg =>
        match findConcatSplit? (raw := raw) with
        | some (value, remainder) =>
            let newState :=
              { stAfter with pre := ("-" ++ remainder) :: pending }
            .ok (some value, newState)
        | none => .error (invalidValueError raw msg expect)

@[inline] def takeOptionValue?
    {α} [FromArg α] (spec : OptSpec α) (st : State) :
    Except Error (Option α × State) :=
  match st.pre with
  | [] => .ok (none, st)
  | token :: rest =>
      let expect := expectOption spec
      match spec.long? with
      | some name =>
          let prefix := longLexeme name
          let eqPrefix := prefix ++ "="
          if spec.eqVal? ∧ token.startsWith eqPrefix then
            let raw := token.drop eqPrefix.length
            parseConcatValue spec token raw rest st expect
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
                  parseConcatValue spec token raw rest st expect
                else
                  .ok (none, st)
            | none => .ok (none, st)
      | none =>
          match spec.short? with
          | some short =>
              let prefix := shortLexeme short
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
                parseConcatValue spec token raw rest st expect
              else
                .ok (none, st)
          | none => .ok (none, st)

@[inline] def collectOptionValues
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

@[inline] def takePositionalValue?
    {α} [FromArg α] (spec : PosSpec α) (st : State) :
    Except Error (Option α × State) :=
  let expect := expectPositional spec
  match State.consumePre? st with
  | some (token, st') =>
      match FromArg.run token with
      | .ok value => .ok (some value, st')
      | .error msg => .error (invalidValueError token msg expect)
  | none =>
      match State.consumePost? st with
      | some (token, st') =>
          match FromArg.run token with
          | .ok value => .ok (some value, st')
          | .error msg => .error (invalidValueError token msg expect)
      | none => .ok (none, st)

@[inline] def collectPositionalValues
    {α} [FromArg α] (spec : PosSpec α) (st : State) : Except Error (List α × State) :=
  let rec loop (acc : List α) (curr : State) : Except Error (List α × State) :=
    match takePositionalValue? spec curr with
    | .error err => .error err
    | .ok (some value, nextState) => loop (value :: acc) nextState
    | .ok (none, nextState) => .ok (acc.reverse, nextState)
  loop [] st

/-- Parser for positional arguments supporting arities. -/
def positional {α} [FromArg α] (spec : PosSpec α) :
    Parser (match spec.arity with
      | .zero => PUnit
      | .one  => Option α
      | .many => List α
      | .some => List α) := fun st =>
  match spec.arity with
  | .zero => .ok PUnit.unit st
  | .one =>
      match takePositionalValue? spec st with
      | .error err => .err err
      | .ok (value?, st') => .ok value? st'
  | .many =>
      match collectPositionalValues spec st with
      | .error err => .err err
      | .ok (values, st') => .ok values st'
  | .some =>
      match collectPositionalValues spec st with
      | .error err => .err err
      | .ok (values, st') =>
          match values with
          | [] => .err { kind := .missingValue, context := [], expect := [expectPositional spec] }
          | _ => .ok values st'

end ArgParse.Core

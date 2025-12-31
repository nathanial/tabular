/-
  Tabular.Parser.State
  Parser state and monad definition
-/
import Tabular.Core.Error
import Tabular.Core.Config

namespace Tabular.Parser

open Tabular

/-- Parser state tracking position in CSV input -/
structure ParserState where
  input : String
  pos : Nat := 0
  line : Nat := 1
  column : Nat := 1
  config : Config
  deriving Repr

/-- Parser monad combining state and error handling -/
abbrev Parser := ExceptT ParseError (StateM ParserState)

namespace Parser

/-- Get current position as Position struct -/
def getPosition : Parser Position := do
  let s ← get
  return { offset := s.pos, line := s.line, column := s.column }

/-- Check if at end of input -/
def atEnd : Parser Bool := do
  let s ← get
  return s.pos >= s.input.length

/-- Peek at current character without consuming -/
def peek? : Parser (Option Char) := do
  let s ← get
  if s.pos >= s.input.length then
    return none
  else
    return some (s.input.get ⟨s.pos⟩)

/-- Peek at current character, error if at end -/
def peek : Parser Char := do
  match ← peek? with
  | some c => return c
  | none => throw (.unexpectedEnd "input")

/-- Consume and return current character, updating line/column -/
def next : Parser Char := do
  let s ← get
  if s.pos >= s.input.length then
    throw (.unexpectedEnd "input")
  let c := s.input.get ⟨s.pos⟩
  let (newLine, newCol) :=
    if c == '\n' then (s.line + 1, 1)
    else (s.line, s.column + 1)
  set { s with pos := s.pos + 1, line := newLine, column := newCol }
  return c

/-- Get the current configuration -/
def getConfig : Parser Config := do
  let s ← get
  return s.config

/-- Try to consume a specific character -/
def tryChar (c : Char) : Parser Bool := do
  match ← peek? with
  | some x =>
    if x == c then
      let _ ← next
      return true
    else
      return false
  | none => return false

/-- Run parser on input, returning result -/
def run {α : Type} (p : Parser α) (input : String) (config : Config) : ParseResult α :=
  let initState : ParserState := { input, config }
  let (result, _) := (ExceptT.run p).run initState
  result

end Parser

end Tabular.Parser

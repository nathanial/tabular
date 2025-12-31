/-
  Tabular.Parser.Primitives
  Low-level parsing helpers
-/
import Tabular.Parser.State

namespace Tabular.Parser

open Parser

/-- Skip whitespace characters (space, tab only - not newlines) -/
def skipInlineWhitespace : Parser Unit := do
  let mut done := false
  while !done do
    match ← peek? with
    | some c =>
      if c == ' ' || c == '\t' then
        let _ ← next
      else
        done := true
    | none => done := true

/-- Read characters while predicate holds -/
def readWhile (pred : Char → Bool) : Parser String := do
  let mut result := ""
  let mut done := false
  while !done do
    match ← peek? with
    | some c =>
      if pred c then
        let _ ← next
        result := result.push c
      else
        done := true
    | none => done := true
  return result

/-- Check if character is a line ending -/
def isLineEnding (c : Char) : Bool :=
  c == '\n' || c == '\r'

/-- Consume line ending (handles CRLF, LF, CR) -/
def consumeLineEnding : Parser Bool := do
  match ← peek? with
  | some '\r' =>
    let _ ← next
    -- Try to consume following LF
    let _ ← tryChar '\n'
    return true
  | some '\n' =>
    let _ ← next
    return true
  | _ => return false

/-- Skip to end of line or end of input -/
def skipToLineEnd : Parser Unit := do
  let mut done := false
  while !done do
    if ← atEnd then done := true
    else
      match ← peek? with
      | some c =>
        if isLineEnding c then done := true
        else let _ ← next
      | none => done := true

end Tabular.Parser

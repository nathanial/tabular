/-
  Tabular.Parser.Record
  Single record (row) parsing
-/
import Tabular.Parser.Field
import Tabular.Core.Row

namespace Tabular.Parser

open Tabular
open Parser

/-- Parse a single record (one row of fields) -/
def parseRecord : Parser (Array Value) := do
  let config ← getConfig
  let mut fields : Array Value := #[]

  -- Handle empty line
  if ← atEnd then return fields
  match ← peek? with
  | some c => if isLineEnding c then return fields
  | none => return fields

  let mut done := false
  while !done do
    let field ← parseField
    fields := fields.push field

    if ← atEnd then done := true
    else
      -- Check what follows the field
      match ← peek? with
      | some c =>
        if c == config.delimiter then
          let _ ← next  -- consume delimiter
          -- Continue to next field
        else if isLineEnding c then
          done := true
        else
          let pos ← getPosition
          throw (.unexpectedChar pos c "delimiter or newline")
      | none => done := true

  return fields

/-- Parse header row -/
def parseHeaders : Parser (Array String) := do
  let values ← parseRecord
  let _ ← consumeLineEnding  -- consume line ending after headers
  return values.map (·.content)

/-- Parse a data row with column index mapping -/
def parseRow (headers : Array String) : Parser Row := do
  let values ← parseRecord
  if headers.isEmpty then
    return Row.ofArray values
  else
    return Row.ofArrayWithHeaders values headers

end Tabular.Parser

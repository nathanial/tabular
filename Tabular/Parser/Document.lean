/-
  Tabular.Parser.Document
  Full document parsing
-/
import Tabular.Parser.Record
import Tabular.Core.Table

namespace Tabular.Parser

open Tabular
open Parser

/-- Skip empty lines -/
private def skipEmptyLines : Parser Unit := do
  let mut done := false
  while !done do
    if ← atEnd then done := true
    else
      match ← peek? with
      | some c =>
        if isLineEnding c then
          let _ ← consumeLineEnding
        else
          done := true
      | none => done := true

/-- Parse entire CSV/TSV document -/
def parseDocument : Parser Table := do
  let config ← getConfig

  -- Check for empty input
  if ← atEnd then
    return { headers := #[], rows := #[], config }

  -- Parse headers if configured
  let headers ← if config.hasHeader then
    parseHeaders
  else
    pure #[]

  -- Parse data rows
  let mut rows : Array Row := #[]
  let mut done := false
  while !done do
    skipEmptyLines
    if ← atEnd then done := true
    else
      let row ← parseRow headers
      -- Skip empty rows (all fields empty)
      if row.values.any (fun v => !v.isEmpty) then
        -- Validate column count if headers present and not allowing ragged
        if !config.allowRagged && !headers.isEmpty && row.size != headers.size then
          let pos ← getPosition
          throw (.columnMismatch pos headers.size row.size)
        rows := rows.push row

      -- Consume line ending if present
      let _ ← consumeLineEnding

  return { headers, rows, config }

/-- Parse CSV/TSV from string -/
def parse (input : String) (config : Config := Config.csv) : ParseResult Table :=
  Parser.run parseDocument input config

/-- Parse without headers -/
def parseRows (input : String) (config : Config := Config.csv) : ParseResult (Array Row) := do
  let table ← parse input { config with hasHeader := false }
  return table.rows

end Tabular.Parser

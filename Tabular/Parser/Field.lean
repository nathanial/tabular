/-
  Tabular.Parser.Field
  Field/cell parsing with quote handling (RFC 4180 compliant)
-/
import Tabular.Parser.Primitives
import Tabular.Core.Value

namespace Tabular.Parser

open Tabular
open Parser

/-- Check if at field terminator (delimiter, newline, or EOF) -/
private def atFieldEnd : Parser Bool := do
  let config ← getConfig
  match ← peek? with
  | none => return true
  | some c => return c == config.delimiter || isLineEnding c

/-- Parse a quoted field (RFC 4180 compliant)
    - Fields enclosed in double quotes
    - Embedded quotes escaped as ""
    - Embedded newlines allowed -/
def parseQuotedField : Parser Value := do
  let config ← getConfig
  let startPos ← getPosition
  let _ ← next  -- consume opening quote

  let mut content := ""
  let mut done := false
  while !done do
    if ← atEnd then
      throw (.unclosedQuote startPos)
    let c ← next
    if c == config.quote then
      -- Check for escaped quote
      match ← peek? with
      | some c' =>
        if c' == config.quote then
          let _ ← next
          content := content.push config.quote
        else
          -- End of quoted field
          done := true
      | none =>
        -- End of input after quote = end of field
        done := true
    else
      content := content.push c

  return { content }

/-- Parse an unquoted field -/
def parseUnquotedField : Parser Value := do
  let config ← getConfig
  let mut content := ""
  let mut done := false
  while !done do
    if ← atFieldEnd then done := true
    else
      let c ← next
      content := content.push c

  -- Optionally trim whitespace
  let finalContent := if config.trimWhitespace
    then content.trim
    else content
  return { content := finalContent }

/-- Parse a single field (quoted or unquoted) -/
def parseField : Parser Value := do
  let config ← getConfig
  if config.trimWhitespace then
    skipInlineWhitespace

  match ← peek? with
  | none => return Value.empty
  | some c =>
    if c == config.quote then
      parseQuotedField
    else if c == config.delimiter || isLineEnding c then
      return Value.empty
    else
      parseUnquotedField

end Tabular.Parser

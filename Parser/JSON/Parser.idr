module Parser.JSON.Parser

import Data.String
import Data.List

import Parser.Core
import Parser.Combinators
import Parser.JSON.Types

-- Digits and numbers

digit : Parser Char
digit = satisfy isDigit

digit1to9 : Parser Char
digit1to9 = satisfy (\c => c >= '1' && c <= '9')

digits1 : Parser (List Char)
digits1 = some digit

sign : Parser (List Char)
sign = (do _ <- char '-'; pure ['-'])
   <|> pure []

expSign : Parser (List Char)
expSign = (do _ <- char '-'; pure ['-'])
      <|> (do _ <- char '+'; pure ['+'])
      <|> pure []

intPart : Parser (List Char)
intPart =
  (do _ <- char '0'; pure ['0'])
  <|>
  (do d <- digit1to9; ds <- many digit; pure (d :: ds))

fracPart : Parser (List Char)
fracPart =
  (do _ <- char '.'; ds <- digits1; pure ('.' :: ds))
  <|> pure []

expPart : Parser (List Char)
expPart =
  (do e <- (char 'e' <|> char 'E')
      s <- expSign
      ds <- digits1
      pure (e :: s ++ ds))
  <|> pure []

numberLiteral : Parser String
numberLiteral = do
  s <- sign
  i <- intPart
  f <- fracPart
  e <- expPart
  pure (pack (s ++ i ++ f ++ e))

-- JSON string literal with escapes per RFC 8259

isControlChar : Char -> Bool
isControlChar c = ord c < 32

hexDigit : Parser Char
hexDigit = satisfy isHexDigit

hexValue : Char -> Int
hexValue c =
  if c >= '0' && c <= '9' then ord c - ord '0'
  else if c >= 'a' && c <= 'f' then 10 + (ord c - ord 'a')
  else 10 + (ord c - ord 'A')

hex4ToInt : List Char -> Int
hex4ToInt cs =
  foldl (\acc => \c => acc * 16 + hexValue c) 0 cs

isHighSurrogate : Int -> Bool
isHighSurrogate x = x >= 0xD800 && x <= 0xDBFF

isLowSurrogate : Int -> Bool
isLowSurrogate x = x >= 0xDC00 && x <= 0xDFFF

combineSurrogates : Int -> Int -> Int
combineSurrogates hi lo =
  0x10000 + (hi - 0xD800) * 0x400 + (lo - 0xDC00)

escapeChar : Parser Char
escapeChar = do
  _ <- char '\\'
  c <- anyChar
  case c of
    '"'  => pure '"'
    '\\' => pure '\\'
    '/'  => pure '/'
    'b'  => pure '\b'
    'f'  => pure '\f'
    'n'  => pure '\n'
    'r'  => pure '\r'
    't'  => pure '\t'
    'u'  => do
      h1 <- hexDigit
      h2 <- hexDigit
      h3 <- hexDigit
      h4 <- hexDigit
      let code = hex4ToInt [h1, h2, h3, h4]
      case isHighSurrogate code of
        True => do
          _  <- char '\\'
          _  <- char 'u'
          l1 <- hexDigit
          l2 <- hexDigit
          l3 <- hexDigit
          l4 <- hexDigit
          let low = hex4ToInt [l1, l2, l3, l4]
          if isLowSurrogate low
            then pure (chr (combineSurrogates code low))
            else failP
        False =>
          if isLowSurrogate code then failP else pure (chr code)
    _ => failP

stringChar : Parser Char
stringChar =
  escapeChar <|> satisfy (\c => c /= '"' && c /= '\\' && not (isControlChar c))

stringLiteral : Parser String
stringLiteral =
  lexeme $ do
    _     <- char '"'
    chars <- many stringChar
    _     <- char '"'
    pure (pack chars)

keyword : String -> Parser String
keyword s = lexeme $ do
  _ <- stringExact s
  pure s

comma : Parser Char
comma = symbol ','

jsonNull : Parser JSON
jsonNull = do
  _ <- keyword "null"
  pure JNull

jsonBool : Parser JSON
jsonBool =
  (do _ <- keyword "true";  pure (JBool True))
  <|>
  (do _ <- keyword "false"; pure (JBool False))

jsonNumber : Parser JSON
jsonNumber = do
  n <- lexeme numberLiteral
  pure (JNumber n)

jsonString : Parser JSON
jsonString = do
  s <- stringLiteral
  pure (JString s)

mutual
  jsonValue : Parser JSON
  jsonValue =
    jsonNull
      <|> jsonBool
      <|> jsonNumber
      <|> jsonString
      <|> jsonArray
      <|> jsonObject

  jsonArray : Parser JSON
  jsonArray = do
    _   <- symbol '['
    xs  <- sepBy jsonValue comma
    _   <- symbol ']'
    pure (JArray xs)

  jsonObject : Parser JSON
  jsonObject = do
    _      <- symbol '{'
    fields <- sepBy objectField comma
    _      <- symbol '}'
    pure (JObject fields)

  objectField : Parser (String, JSON)
  objectField = do
    key <- stringLiteral
    _   <- symbol ':'
    val <- jsonValue
    pure (key, val)

public export
parseJSON : String -> Maybe JSON
parseJSON = parse (ws *> jsonValue)

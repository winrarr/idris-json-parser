module JsonParser

import Data.String
import Data.List
import System
import System.File

-- A simple JSON type
data JSON
  = JNull
  | JBool Bool
  | JNumber String
  | JString String
  | JArray (List JSON)
  | JObject (List (String, JSON))

stringIntercalate : String -> List String -> String
stringIntercalate sep xs =
  pack (intercalate (unpack sep) (map unpack xs))

covering
showJSON : JSON -> String
showJSON JNull        = "null"
showJSON (JBool b)    = show b
showJSON (JNumber n)  = n
showJSON (JString s)  = "\"" ++ s ++ "\""
showJSON (JArray xs) =
  "[" ++ stringIntercalate ", " (map showJSON xs) ++ "]"
showJSON (JObject fields) =
  let showField : (String, JSON) -> String
      showField (k, v) = "\"" ++ k ++ "\": " ++ showJSON v
  in "{" ++ stringIntercalate ", " (map showField fields) ++ "}"

-- Show instance for debugging / printing
implementation Show JSON where
  show = assert_total showJSON

-- Parser type

record Parser a where
  constructor MkParser
  runParser : String -> Maybe (a, String)

-- Functor / Applicative / Monad so we can use `map`, `<*>`, and `do`

implementation Functor Parser where
  map f (MkParser p) = MkParser $ \s =>
    case p s of
      Just (x, rest) => Just (f x, rest)
      Nothing        => Nothing

implementation Applicative Parser where
  pure x = MkParser $ \s => Just (x, s)

  (<*>) (MkParser pf) (MkParser pa) = MkParser $ \s =>
    case pf s of
      Just (f, s1) =>
        case pa s1 of
          Just (a, s2) => Just (f a, s2)
          Nothing      => Nothing
      Nothing => Nothing

implementation Monad Parser where
  (>>=) (MkParser pa) f = MkParser $ \s =>
    case pa s of
      Just (a, s1) => runParser (f a) s1
      Nothing      => Nothing

-- Choice combinator (like Alternative.<|>, but without the typeclass)

infixl 3 <|>

(<|>) : Parser a -> Parser a -> Parser a
(<|>) (MkParser p1) (MkParser p2) = MkParser $ \s =>
  case p1 s of
    Just res => Just res
    Nothing  => p2 s

-- Run a parser and demand that only whitespace remains

parse : Parser a -> String -> Maybe a
parse p s =
  case runParser p s of
    Just (x, rest) =>
      if all isSpace (unpack rest) then Just x else Nothing
    Nothing => Nothing

-- Basic primitives

anyChar : Parser Char
anyChar = MkParser $ \s =>
  case unpack s of
    []        => Nothing
    (c :: cs) => Just (c, pack cs)

failP : Parser a
failP = MkParser $ \_ => Nothing

satisfy : (Char -> Bool) -> Parser Char
satisfy pred = do
  c <- anyChar
  if pred c then pure c else failP

char : Char -> Parser Char
char c = satisfy (== c)

-- Match an exact string (no escaping etc.)

stringExact : String -> Parser String
stringExact target = MkParser $ \s =>
  let tgtChars  = unpack target
      srcChars  = unpack s
      tgtLen    = length tgtChars
      (pref, rest) = splitAt tgtLen srcChars
  in if pref == tgtChars
        then Just (target, pack rest)
        else Nothing

-- Combinators `many` and `some` (Kleene star / plus)

mutual
  many : Parser a -> Parser (List a)
  many p = some p <|> pure []

  some : Parser a -> Parser (List a)
  some p = do
    x  <- p
    xs <- many p
    pure (x :: xs)

-- Whitespace

ws : Parser ()
ws =
  let p = satisfy isSpace in
    many p *> pure ()

-- Digits and integers (only non-negative for now)

digit : Parser Char
digit = satisfy isDigit

charToDigit : Char -> Integer
charToDigit c = cast (ord c - ord '0')

digitsToInt : List Char -> Integer
digitsToInt = foldl step 0
  where
    step : Integer -> Char -> Integer
    step acc c = acc * 10 + charToDigit c

integer : Parser Integer
integer = do
  ds <- some digit
  ws
  pure (digitsToInt ds)

-- String literal for JSON keys/values (no escapes, just raw text between quotes)

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
stringLiteral = do
  _     <- char '"'
  chars <- many stringChar
  _     <- char '"'
  ws
  pure (pack chars)

-- A keyword like "null", "true", "false" followed by optional whitespace

keyword : String -> Parser String
keyword s = do
  _ <- stringExact s
  ws
  pure s

-- Separator combinator: parse p, then many (sep >> p)

sepBy : Parser a -> Parser sep -> Parser (List a)
sepBy p sep =
  (do
      x  <- p
      xs <- many (sep *> p)
      pure (x :: xs))
  <|> pure []

comma : Parser Char
comma = do
  _ <- char ','
  ws
  pure ','

-- JSON value parsers

jsonNull : Parser JSON
jsonNull = do
  _ <- keyword "null"
  pure JNull

jsonBool : Parser JSON
jsonBool =
  (do _ <- keyword "true";  pure (JBool True))
  <|>
  (do _ <- keyword "false"; pure (JBool False))

-- RFC 8259 number grammar
-- number = [ minus ] int [ frac ] [ exp ]
-- int    = "0" / digit1-9 *digit
-- frac   = "." 1*digit
-- exp    = ("e" / "E") ["-" / "+"] 1*digit

digit1to9 : Parser Char
digit1to9 = satisfy (\c => c >= '1' && c <= '9')

digits1 : Parser (List Char)
digits1 = some digit

sign : Parser (List Char)
sign = (do _ <- char '-'; pure ['-'])
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
      s <- sign
      ds <- digits1
      pure (e :: s ++ ds))
  <|> pure []

numberLiteral : Parser String
numberLiteral = do
  s <- sign
  i <- intPart
  f <- fracPart
  e <- expPart
  ws
  pure (pack (s ++ i ++ f ++ e))

jsonNumber : Parser JSON
jsonNumber = do
  n <- numberLiteral
  pure (JNumber n)

jsonString : Parser JSON
jsonString = do
  s <- stringLiteral
  pure (JString s)

-- Forward reference, so we use `mutual`

mutual
  jsonValue : Parser JSON
  jsonValue = do
    ws
    jsonNull
      <|> jsonBool
      <|> jsonNumber
      <|> jsonString
      <|> jsonArray
      <|> jsonObject

  jsonArray : Parser JSON
  jsonArray = do
    _   <- char '['
    ws
    xs  <- sepBy jsonValue comma
    _   <- char ']'
    ws
    pure (JArray xs)

  jsonObject : Parser JSON
  jsonObject = do
    _      <- char '{'
    ws
    fields <- sepBy objectField comma
    _      <- char '}'
    ws
    pure (JObject fields)

  objectField : Parser (String, JSON)
  objectField = do
    key <- stringLiteral
    _   <- char ':'
    ws
    val <- jsonValue
    pure (key, val)

-- Top-level helper

parseJSON : String -> Maybe JSON
parseJSON = parse jsonValue

-- A tiny CLI entry point

main : IO ()
main = do
  args <- getArgs
  inputRes : Either FileError String <-
    case args of
      []            => readFile "/dev/stdin"
      [_]           => readFile "/dev/stdin"
      (_ :: p :: _) => readFile p
  case inputRes of
    Left err => do
      putStrLn "File error:"
      print err
    Right input => do
      case parseJSON input of
        Just v  => do
          putStrLn "Parsed successfully:"
          print v
        Nothing =>
          putStrLn "Parse error."

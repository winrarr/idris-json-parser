module JsonParser

import Data.String
import Data.List

-- A simple JSON type
data JSON
  = JNull
  | JBool Bool
  | JNumber Integer
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
showJSON (JNumber n)  = show n
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

satisfy : (Char -> Bool) -> Parser Char
satisfy pred = do
  c <- anyChar
  if pred c then pure c else MkParser $ \_ => Nothing

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

stringLiteral : Parser String
stringLiteral = do
  _     <- char '"'
  chars <- many (satisfy (/= '"'))
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

jsonNumber : Parser JSON
jsonNumber = do
  n <- integer
  pure (JNumber n)

jsonString : Parser JSON
jsonString = do
  s <- stringLiteral
  pure (JString s)

-- Forward reference, so we use `mutual`

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
  putStrLn "Enter a JSON value on one line:"
  line <- getLine
  case parseJSON line of
    Just v  => do
      putStrLn "Parsed successfully:"
      print v
    Nothing =>
      putStrLn "Parse error."

module Parser.Combinators

import Data.String
import Data.List

import Parser.Core

mutual
  public export
  many : Parser a -> Parser (List a)
  many p = some p <|> pure []

  public export
  some : Parser a -> Parser (List a)
  some p = do
    x  <- p
    xs <- many p
    pure (x :: xs)

public export
ws : Parser ()
ws =
  let p = satisfy isJsonWhitespace in
    many p *> pure ()

public export
lexeme : Parser a -> Parser a
lexeme p = do
  x <- p
  ws
  pure x

public export
symbol : Char -> Parser Char
symbol c = lexeme (char c)

public export
sepBy : Parser a -> Parser sep -> Parser (List a)
sepBy p sep =
  (do
      x  <- p
      xs <- many (sep *> p)
      pure (x :: xs))
  <|> pure []

module Parser

import Data.String
import Data.List

public export
record Parser a where
  constructor MkParser
  runParser : String -> Maybe (a, String)

public export
implementation Functor Parser where
  map f (MkParser p) = MkParser $ \s =>
    case p s of
      Just (x, rest) => Just (f x, rest)
      Nothing        => Nothing

public export
implementation Applicative Parser where
  pure x = MkParser $ \s => Just (x, s)

  (<*>) (MkParser pf) (MkParser pa) = MkParser $ \s =>
    case pf s of
      Just (f, s1) =>
        case pa s1 of
          Just (a, s2) => Just (f a, s2)
          Nothing      => Nothing
      Nothing => Nothing

public export
implementation Monad Parser where
  (>>=) (MkParser pa) f = MkParser $ \s =>
    case pa s of
      Just (a, s1) => runParser (f a) s1
      Nothing      => Nothing

infixl 3 <|>

public export
(<|>) : Parser a -> Parser a -> Parser a
(<|>) (MkParser p1) (MkParser p2) = MkParser $ \s =>
  case p1 s of
    Just res => Just res
    Nothing  => p2 s

public export
parse : Parser a -> String -> Maybe a
parse p s =
  case runParser p s of
    Just (x, rest) =>
      if all isSpace (unpack rest) then Just x else Nothing
    Nothing => Nothing

public export
anyChar : Parser Char
anyChar = MkParser $ \s =>
  case unpack s of
    []        => Nothing
    (c :: cs) => Just (c, pack cs)

public export
failP : Parser a
failP = MkParser $ \_ => Nothing

public export
satisfy : (Char -> Bool) -> Parser Char
satisfy pred = do
  c <- anyChar
  if pred c then pure c else failP

public export
char : Char -> Parser Char
char c = satisfy (== c)

public export
stringExact : String -> Parser String
stringExact target = MkParser $ \s =>
  let tgtChars  = unpack target
      srcChars  = unpack s
      tgtLen    = length tgtChars
      (pref, rest) = splitAt tgtLen srcChars
  in if pref == tgtChars
        then Just (target, pack rest)
        else Nothing

module Main

import System
import System.File

import Parser.JSON.Parser
import Parser.JSON.Types
import Parser.YAML.Parser


data Format = JSON | YAML

formatName : Format -> String
formatName JSON = "JSON"
formatName YAML = "YAML"

parseAuto : String -> Maybe (Format, JSON)
parseAuto input =
  let parsers : List (Format, String -> Maybe JSON)
      parsers =
        [ (JSON, parseJSON)
        , (YAML, parseYAML)
        ]
      attempt : (Format, String -> Maybe JSON) -> Maybe (Format, JSON)
      attempt (fmt, p) = map (fmt,) (p input)
      orElse : Maybe (Format, JSON) -> Maybe (Format, JSON) -> Maybe (Format, JSON)
      orElse (Just v) _ = Just v
      orElse Nothing next = next
  in foldr orElse Nothing (map attempt parsers)

handleInput : Either FileError String -> IO ()
handleInput inputRes =
  case inputRes of
    Left err => do
      putStrLn "File error:"
      print err
    Right input =>
      case parseAuto input of
        Just (fmt, v) => do
          putStrLn ("Parsed successfully (" ++ formatName fmt ++ "):")
          print v
        Nothing =>
          putStrLn "Parse error."

main : IO ()
main = do
  args <- getArgs
  case args of
    [_]        => do
      res <- readFile "/dev/stdin"
      handleInput res
    [_ , p]    => do
      res <- readFile p
      handleInput res
    _          => putStrLn "Usage: autoparser [FILE]"

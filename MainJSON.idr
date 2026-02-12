module MainJSON

import System
import System.File

import Parser.JSON.Parser
import Parser.JSON.Types

handleInput : Either FileError String -> IO ()
handleInput inputRes =
  case inputRes of
    Left err => do
      putStrLn "File error:"
      print err
    Right input =>
      case parseJSON input of
        Just v => do
          putStrLn "Parsed successfully:"
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
    _          => putStrLn "Usage: jsonparser [FILE]"

module Main

import System
import System.File

import JSONParser
import JSON

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
    Right input =>
      case parseJSON input of
        Just v  => do
          putStrLn "Parsed successfully:"
          print v
        Nothing =>
          putStrLn "Parse error."

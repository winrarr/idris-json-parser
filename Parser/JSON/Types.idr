module Parser.JSON.Types

import Data.String
import Data.List

public export
  data JSON
    = JNull
    | JBool Bool
    | JNumber String
    | JString String
    | JArray (List JSON)
    | JObject (List (String, JSON))

hexDigit : Int -> Char
hexDigit n =
  if n < 10
     then chr (ord '0' + n)
     else chr (ord 'A' + (n - 10))

hex4 : Int -> String
hex4 n =
  let n1 = (n `div` 4096) `mod` 16
      n2 = (n `div` 256) `mod` 16
      n3 = (n `div` 16) `mod` 16
      n4 = n `mod` 16
  in pack [hexDigit n1, hexDigit n2, hexDigit n3, hexDigit n4]

escapeChar : Char -> String
escapeChar '"'  = "\\\""
escapeChar '\\' = "\\\\"
escapeChar '/'  = "\\/"
escapeChar '\b' = "\\b"
escapeChar '\f' = "\\f"
escapeChar '\n' = "\\n"
escapeChar '\r' = "\\r"
escapeChar '\t' = "\\t"
escapeChar c =
  if ord c < 0x20 then "\\u" ++ hex4 (ord c) else pack [c]

escapeString : String -> String
escapeString s = foldl (++) "" (map escapeChar (unpack s))

stringIntercalate : String -> List String -> String
stringIntercalate sep xs =
  pack (intercalate (unpack sep) (map unpack xs))

covering
showJSON : JSON -> String
showJSON JNull        = "null"
showJSON (JBool True) = "true"
showJSON (JBool False)= "false"
showJSON (JNumber n)  = n
showJSON (JString s)  = "\"" ++ escapeString s ++ "\""
showJSON (JArray xs) =
  "[" ++ stringIntercalate ", " (map showJSON xs) ++ "]"
showJSON (JObject fields) =
  let showField : (String, JSON) -> String
      showField (k, v) = "\"" ++ escapeString k ++ "\": " ++ showJSON v
  in "{" ++ stringIntercalate ", " (map showField fields) ++ "}"

public export
implementation Show JSON where
  show = assert_total showJSON

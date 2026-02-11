module JSON

import Data.String
import Data.List

-- A simple JSON type
public export
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

public export
implementation Show JSON where
  show = assert_total showJSON

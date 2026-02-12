module Parser.YAML.Parser

import Data.String
import Data.List

import Parser.Core
import Parser.JSON.Types

%default partial


-- Basic helpers

isYamlSpace : Char -> Bool
isYamlSpace c = c == ' ' || c == '\t'

ytrimLeft : String -> String
ytrimLeft s = pack (dropWhile isYamlSpace (unpack s))

ytrimRight : String -> String
ytrimRight s =
  let rs = reverse (unpack s) in
    pack (reverse (dropWhile isYamlSpace rs))

ytrim : String -> String
ytrim s = ytrimRight (ytrimLeft s)

isAsciiUpper : Char -> Bool
isAsciiUpper c = c >= 'A' && c <= 'Z'

toLowerAscii : Char -> Char
toLowerAscii c = if isAsciiUpper c then chr (ord c + 32) else c

lowerString : String -> String
lowerString s = pack (map toLowerAscii (unpack s))

mapMaybe : (a -> Maybe b) -> List a -> Maybe (List b)
mapMaybe f [] = Just []
mapMaybe f (x :: xs) =
  case f x of
    Nothing => Nothing
    Just y =>
      case mapMaybe f xs of
        Nothing => Nothing
        Just ys => Just (y :: ys)

stringIntercalate : String -> List String -> String
stringIntercalate sep xs =
  pack (intercalate (unpack sep) (map unpack xs))

-- Split lines (handle \r\n)

stripTrailingCR : String -> String
stripTrailingCR s =
  case reverse (unpack s) of
    ('\r' :: cs) => pack (reverse cs)
    _ => s

splitLines : String -> List String
splitLines s =
  let chars = unpack s in
    splitLinesAcc chars [] []
  where
    splitLinesAcc : List Char -> List Char -> List String -> List String
    splitLinesAcc [] current acc =
      let line = reverse current in
        reverse (pack line :: acc)
    splitLinesAcc ('\n' :: cs) current acc =
      let line = reverse current in
        splitLinesAcc cs [] (pack line :: acc)
    splitLinesAcc (c :: cs) current acc =
      splitLinesAcc cs (c :: current) acc

-- Line representation

record Line where
  constructor MkLine
  indent : Nat
  raw : String
  blank : Bool
  commentOnly : Bool

countIndent : String -> Maybe (Nat, String)
countIndent s = go 0 (unpack s) where
  go : Nat -> List Char -> Maybe (Nat, String)
  go n [] = Just (n, "")
  go n (c :: cs) =
    if c == ' ' then go (n + 1) cs
    else if c == '\t' then Nothing
    else Just (n, pack (c :: cs))

isBlankLine : String -> Bool
isBlankLine raw = ytrimLeft raw == ""

isCommentOnlyLine : String -> Bool
isCommentOnlyLine raw =
  case unpack (ytrimLeft raw) of
    ('#' :: _) => True
    _ => False

lexLine : String -> Maybe Line
lexLine s =
  case countIndent (stripTrailingCR s) of
    Nothing => Nothing
    Just (n, rest) =>
      let blank = isBlankLine rest
          commentOnly = isCommentOnlyLine rest
      in Just (MkLine n rest blank commentOnly)

lexLines : String -> Maybe (List Line)
lexLines s = mapMaybe lexLine (splitLines s)

nextNonSkippable : List Line -> Maybe (Line, List Line)
nextNonSkippable [] = Nothing
nextNonSkippable (l :: ls) =
  if l.blank || l.commentOnly then nextNonSkippable ls else Just (l, ls)

allSkippable : List Line -> Bool
allSkippable [] = True
allSkippable (l :: ls) = if l.blank || l.commentOnly then allSkippable ls else False

-- Comment stripping (only for non-block scalar lines)

stripComment : String -> String
stripComment s = pack (go (unpack s) False False True []) where
  go : List Char -> Bool -> Bool -> Bool -> List Char -> List Char
  go [] _ _ _ acc = reverse acc
  go (c :: cs) inS inD prevSpace acc =
    if inS then
      if c == '\'' then
        case cs of
          ('\'' :: rest) => go rest True inD False ('\'' :: '\'' :: acc)
          _ => go cs False inD False (c :: acc)
      else go cs True inD False (c :: acc)
    else if inD then
      if c == '\\' then
        case cs of
          [] => reverse (c :: acc)
          (d :: rest) => go rest False True False (d :: c :: acc)
      else if c == '"' then go cs False False False (c :: acc)
      else go cs False True False (c :: acc)
    else
      if c == '#' && prevSpace then reverse acc
      else go cs False False (isYamlSpace c) (c :: acc)

lineContent : Line -> String
lineContent l = ytrimRight (stripComment l.raw)

-- Mapping detection

splitMapping : String -> Maybe (String, String)
splitMapping s = go (unpack s) False False True [] where
  go : List Char -> Bool -> Bool -> Bool -> List Char -> Maybe (String, String)
  go [] _ _ _ _ = Nothing
  go (c :: cs) inS inD prevSpace acc =
    if inS then
      if c == '\'' then
        case cs of
          ('\'' :: rest) => go rest True inD False ('\'' :: '\'' :: acc)
          _ => go cs False inD False (c :: acc)
      else go cs True inD False (c :: acc)
    else if inD then
      if c == '\\' then
        case cs of
          [] => Nothing
          (d :: rest) => go rest False True False (d :: c :: acc)
      else if c == '"' then go cs False False False (c :: acc)
      else go cs False True False (c :: acc)
    else
      if c == ':' then
        case cs of
          [] =>
            let key = ytrimRight (pack (reverse acc)) in
              if key == "" then Nothing else Just (key, "")
          (n :: rest) =>
            if isYamlSpace n then
              let key = ytrimRight (pack (reverse acc))
                  val = ytrimLeft (pack (n :: rest))
              in if key == "" then Nothing else Just (key, val)
            else go cs False False False (c :: acc)
      else go cs False False (isYamlSpace c) (c :: acc)

isSequenceLine : String -> Bool
isSequenceLine s =
  case unpack s of
    [] => False
    ('-' :: rest) =>
      case rest of
        [] => True
        (' ' :: _) => True
        _ => False
    _ => False

-- Scalar parsing

parseSingleQuotedChars : List Char -> Maybe (String, List Char)
parseSingleQuotedChars cs =
  case cs of
    ('\'' :: rest) => go rest []
    _ => Nothing
  where
    go : List Char -> List Char -> Maybe (String, List Char)
    go [] _ = Nothing
    go ('\'' :: cs2) acc =
      case cs2 of
        ('\'' :: rest2) => go rest2 ('\'' :: acc)
        _ => Just (pack (reverse acc), cs2)
    go (c :: cs2) acc = go cs2 (c :: acc)

parseSingleQuoted : String -> Maybe String
parseSingleQuoted s =
  case parseSingleQuotedChars (unpack s) of
    Just (v, rest) => if rest == [] then Just v else Nothing
    Nothing => Nothing

hexDigitToInt : Char -> Maybe Int
hexDigitToInt c =
  if c >= '0' && c <= '9' then Just (ord c - ord '0')
  else if c >= 'a' && c <= 'f' then Just (10 + ord c - ord 'a')
  else if c >= 'A' && c <= 'F' then Just (10 + ord c - ord 'A')
  else Nothing

parseHex4 : List Char -> Maybe (Int, List Char)
parseHex4 (a :: b :: c :: d :: rest) =
  case (hexDigitToInt a, hexDigitToInt b, hexDigitToInt c, hexDigitToInt d) of
    (Just x1, Just x2, Just x3, Just x4) =>
      Just (x1 * 4096 + x2 * 256 + x3 * 16 + x4, rest)
    _ => Nothing
parseHex4 _ = Nothing

parseDoubleQuotedChars : List Char -> Maybe (String, List Char)
parseDoubleQuotedChars cs =
  case cs of
    ('"' :: rest) => go rest []
    _ => Nothing
  where
    go : List Char -> List Char -> Maybe (String, List Char)
    go [] _ = Nothing
    go ('"' :: cs2) acc = Just (pack (reverse acc), cs2)
    go ('\\' :: cs2) acc =
      case cs2 of
        [] => Nothing
        ('"' :: rest2) => go rest2 ('"' :: acc)
        ('\\' :: rest2) => go rest2 ('\\' :: acc)
        ('/' :: rest2) => go rest2 ('/' :: acc)
        ('b' :: rest2) => go rest2 ('\b' :: acc)
        ('f' :: rest2) => go rest2 ('\f' :: acc)
        ('n' :: rest2) => go rest2 ('\n' :: acc)
        ('r' :: rest2) => go rest2 ('\r' :: acc)
        ('t' :: rest2) => go rest2 ('\t' :: acc)
        ('u' :: rest2) =>
          case parseHex4 rest2 of
            Just (code, rest3) => go rest3 (chr code :: acc)
            Nothing => Nothing
        _ => Nothing
    go (c :: cs2) acc = go cs2 (c :: acc)

parseDoubleQuoted : String -> Maybe String
parseDoubleQuoted s =
  case parseDoubleQuotedChars (unpack s) of
    Just (v, rest) => if rest == [] then Just v else Nothing
    Nothing => Nothing

isDigitAscii : Char -> Bool
isDigitAscii c = c >= '0' && c <= '9'

parseIntPart : List Char -> Maybe (List Char)
parseIntPart [] = Nothing
parseIntPart ('0' :: rest) = Just rest
parseIntPart (c :: rest) =
  if c >= '1' && c <= '9'
     then Just (dropWhile isDigitAscii rest)
     else Nothing

parseFracPart : List Char -> Maybe (List Char)
parseFracPart ('.' :: []) = Nothing
parseFracPart ('.' :: (c :: rest)) =
  if isDigitAscii c then Just (dropWhile isDigitAscii rest) else Nothing
parseFracPart rest = Just rest

parseExpDigits : List Char -> Maybe (List Char)
parseExpDigits [] = Nothing
parseExpDigits (c :: rest) =
  if isDigitAscii c then Just (dropWhile isDigitAscii rest) else Nothing

parseExpTail : List Char -> Maybe (List Char)
parseExpTail ('+' :: rest) = parseExpDigits rest
parseExpTail ('-' :: rest) = parseExpDigits rest
parseExpTail rest = parseExpDigits rest

parseExpPart : List Char -> Maybe (List Char)
parseExpPart ('e' :: rest) = parseExpTail rest
parseExpPart ('E' :: rest) = parseExpTail rest
parseExpPart rest = Just rest

isJsonNumberBody : List Char -> Bool
isJsonNumberBody cs =
  case parseIntPart cs of
    Nothing => False
    Just rest1 =>
      case parseFracPart rest1 of
        Nothing => False
        Just rest2 =>
          case parseExpPart rest2 of
            Nothing => False
            Just rest3 => rest3 == []

isJsonNumberChars : List Char -> Bool
isJsonNumberChars cs =
  case cs of
    ('-' :: rest) => isJsonNumberBody rest
    _ => isJsonNumberBody cs

parseJsonNumber : String -> Maybe String
parseJsonNumber s =
  case unpack s of
    [] => Nothing
    _ => if isJsonNumberChars (unpack s) then Just s else Nothing

parseYamlNumber : String -> Maybe String
parseYamlNumber s =
  let t = ytrim s in
    case unpack t of
      ('+' :: cs) => parseJsonNumber (pack cs)
      _ => parseJsonNumber t

parsePlainScalar : String -> JSON
parsePlainScalar s =
  let t = ytrim s
      lower = lowerString t
  in if t == "" then JNull
     else if lower == "null" || lower == "~" then JNull
     else if lower == "true" then JBool True
     else if lower == "false" then JBool False
     else case parseYamlNumber t of
            Just num => JNumber num
            Nothing => JString t

-- Flow parsing (single-line)

isFlowSpace : Char -> Bool
isFlowSpace c = c == ' ' || c == '\t'

skipFlowWs : List Char -> List Char
skipFlowWs = dropWhile isFlowSpace

mutual
  flowValue : List Char -> Maybe (JSON, List Char)
  flowValue [] = Nothing
  flowValue ('[' :: cs) = flowSeq cs
  flowValue ('{' :: cs) = flowMap cs
  flowValue cs = flowScalar cs

  flowSeq : List Char -> Maybe (JSON, List Char)
  flowSeq cs =
    let cs1 = skipFlowWs cs in
      case cs1 of
        (']' :: rest) => Just (JArray [], rest)
        _ =>
          case flowValue cs1 of
            Nothing => Nothing
            Just (v, rest1) =>
              flowSeqTail [v] rest1
    where
      flowSeqTail : List JSON -> List Char -> Maybe (JSON, List Char)
      flowSeqTail acc rest =
        let r1 = skipFlowWs rest in
          case r1 of
            (',' :: r2) =>
              case flowValue (skipFlowWs r2) of
                Nothing => Nothing
                Just (v, rest2) => flowSeqTail (acc ++ [v]) rest2
            (']' :: r2) => Just (JArray acc, r2)
            _ => Nothing

  flowMap : List Char -> Maybe (JSON, List Char)
  flowMap cs =
    let cs1 = skipFlowWs cs in
      case cs1 of
        ('}' :: rest) => Just (JObject [], rest)
        _ =>
          case flowKey cs1 of
            Nothing => Nothing
            Just (k, rest1) =>
              let rest2 = skipFlowWs rest1 in
                case rest2 of
                  (':' :: rest3) =>
                    case flowValue (skipFlowWs rest3) of
                      Nothing => Nothing
                      Just (v, rest4) => flowMapTail [(k, v)] rest4
                  _ => Nothing
    where
      flowMapTail : List (String, JSON) -> List Char -> Maybe (JSON, List Char)
      flowMapTail acc rest =
        let r1 = skipFlowWs rest in
          case r1 of
            (',' :: r2) =>
              case flowKey (skipFlowWs r2) of
                Nothing => Nothing
                Just (k, r3) =>
                  let r4 = skipFlowWs r3 in
                    case r4 of
                      (':' :: r5) =>
                        case flowValue (skipFlowWs r5) of
                          Nothing => Nothing
                          Just (v, r6) => flowMapTail (acc ++ [(k, v)]) r6
                      _ => Nothing
            ('}' :: r2) => Just (JObject acc, r2)
            _ => Nothing

  flowKey : List Char -> Maybe (String, List Char)
  flowKey cs =
    case cs of
      ('\'' :: _) =>
        case parseSingleQuotedChars cs of
          Just (s, rest) => Just (s, rest)
          Nothing => Nothing
      ('"' :: _) =>
        case parseDoubleQuotedChars cs of
          Just (s, rest) => Just (s, rest)
          Nothing => Nothing
      _ =>
        let (tok, rest) = spanFlowToken cs in
          if tok == "" then Nothing
          else
            case unpack tok of
              ('&' :: _) => Nothing
              ('*' :: _) => Nothing
              ('!' :: _) => Nothing
              ('?' :: _) => Nothing
              _ => Just (ytrim tok, rest)

  flowScalar : List Char -> Maybe (JSON, List Char)
  flowScalar cs = flowScalarRaw cs

  flowScalarRaw : List Char -> Maybe (JSON, List Char)
  flowScalarRaw cs =
    case cs of
      ('\'' :: _) =>
        case parseSingleQuotedChars cs of
          Just (s, rest) => Just (JString s, rest)
          Nothing => Nothing
      ('"' :: _) =>
        case parseDoubleQuotedChars cs of
          Just (s, rest) => Just (JString s, rest)
          Nothing => Nothing
      _ =>
        let (tok, rest) = spanFlowToken cs in
          if tok == "" then Nothing
          else
            case unpack tok of
              ('&' :: _) => Nothing
              ('*' :: _) => Nothing
              ('!' :: _) => Nothing
              ('?' :: _) => Nothing
              _ => Just (parsePlainScalar tok, rest)

  spanFlowToken : List Char -> (String, List Char)
  spanFlowToken cs =
    let (tok, rest) = spanToken cs [] in
      (pack (reverse tok), rest)
    where
      spanToken : List Char -> List Char -> (List Char, List Char)
      spanToken [] acc = (acc, [])
      spanToken (c :: cs) acc =
        if isFlowSpace c || c == ',' || c == ']' || c == '}' || c == ':' then (acc, c :: cs)
        else spanToken cs (c :: acc)

parseFlow : String -> Maybe JSON
parseFlow s =
  case flowValue (skipFlowWs (unpack s)) of
    Just (v, rest) =>
      if ytrim (pack rest) == "" then Just v else Nothing
    Nothing => Nothing

-- Block scalar parsing

firstBlockIndent : Nat -> List Line -> Maybe (Nat, List Line)
firstBlockIndent parentIndent [] = Nothing
firstBlockIndent parentIndent (l :: ls) =
  if l.blank then firstBlockIndent parentIndent ls
  else if l.indent > parentIndent then Just (l.indent, l :: ls) else Nothing

takeBlockLines : Nat -> List Line -> List String -> (List String, List Line)
takeBlockLines blockIndent [] acc = (reverse acc, [])
takeBlockLines blockIndent (l :: ls) acc =
  if l.blank then
    takeBlockLines blockIndent ls ("" :: acc)
  else if l.indent < blockIndent then (reverse acc, l :: ls)
  else
    let dropped = drop blockIndent (unpack l.raw) in
      takeBlockLines blockIndent ls (pack dropped :: acc)

strEndsWithNewline : String -> Bool
strEndsWithNewline s =
  case reverse (unpack s) of
    ('\n' :: _) => True
    _ => False

assembleBlock : Bool -> List String -> String
assembleBlock True lines =
  case lines of
    [] => ""
    _ => stringIntercalate "\n" lines
assembleBlock False lines =
  foldl foldLine "" lines
  where
    foldLine : String -> String -> String
    foldLine acc line =
      if acc == "" then line
      else if line == "" then acc ++ "\n"
      else if strEndsWithNewline acc then acc ++ line
      else acc ++ " " ++ line

parseBlockScalar : Nat -> Bool -> List Line -> Maybe (String, List Line)
parseBlockScalar parentIndent isLiteral ls =
  case firstBlockIndent parentIndent ls of
    Nothing => Just ("", ls)
    Just (blockIndent, _) =>
      let (lines, remaining) = takeBlockLines blockIndent ls [] in
        Just (assembleBlock isLiteral lines, remaining)

-- Core parsing

mutual
  parseValueAt : Nat -> List Line -> Maybe (JSON, List Line)
  parseValueAt indent ls =
    case nextNonSkippable ls of
      Nothing => Nothing
      Just (l, rest) =>
        if l.indent /= indent then Nothing
        else
          let content = lineContent l in
            case blockIndicator content of
              Just isLit =>
                case parseBlockScalar indent isLit rest of
                  Just (txt, rest2) => Just (JString txt, rest2)
                  Nothing => Nothing
              Nothing =>
                if isSequenceLine content then parseSequence indent ls
                else case splitMapping content of
                       Just _ => parseMapping indent ls
                       Nothing =>
                         case parseInlineValue content rest of
                           Just v => Just (v, rest)
                           Nothing => Nothing

  parseInlineValue : String -> List Line -> Maybe JSON
  parseInlineValue s _ =
    let t = ytrim s in
      if t == "" then Just JNull
      else case blockIndicator t of
             Just _ => Nothing
             Nothing =>
               case unpack t of
                 ('&' :: _) => Nothing
                 ('*' :: _) => Nothing
                 ('!' :: _) => Nothing
                 ('?' :: _) => Nothing
                 ('[' :: _) => parseFlow t
                 ('{' :: _) => parseFlow t
                 ('\'' :: _) => case parseSingleQuoted t of
                                  Just v => Just (JString v)
                                  Nothing => Nothing
                 ('"' :: _) => case parseDoubleQuoted t of
                                  Just v => Just (JString v)
                                  Nothing => Nothing
                 _ => Just (parsePlainScalar t)

  blockIndicator : String -> Maybe Bool
  blockIndicator s =
    case unpack (ytrim s) of
      ('|' :: _) => Just True
      ('>' :: _) => Just False
      _ => Nothing

  parseNestedValue : Nat -> List Line -> Maybe (JSON, List Line)
  parseNestedValue parentIndent ls =
    case nextNonSkippable ls of
      Nothing => Just (JNull, ls)
      Just (l, _) =>
        if l.indent > parentIndent then parseValueAt l.indent ls else Just (JNull, ls)

  parseMapping : Nat -> List Line -> Maybe (JSON, List Line)
  parseMapping indent ls =
    case parseMappingEntry indent ls of
      Nothing => Nothing
      Just (entry, rest) =>
        let (entries, rest2) = parseMappingMore indent [entry] rest in
          Just (JObject entries, rest2)

  parseMappingMore : Nat -> List (String, JSON) -> List Line -> (List (String, JSON), List Line)
  parseMappingMore indent acc ls =
    case nextNonSkippable ls of
      Nothing => (acc, ls)
      Just (l, _) =>
        if l.indent /= indent then (acc, ls)
        else
          let content = lineContent l in
            case splitMapping content of
              Nothing => (acc, ls)
              Just _ =>
                case parseMappingEntry indent ls of
                  Nothing => (acc, ls)
                  Just (entry, rest) => parseMappingMore indent (acc ++ [entry]) rest

  parseMappingEntry : Nat -> List Line -> Maybe ((String, JSON), List Line)
  parseMappingEntry indent ls =
    case nextNonSkippable ls of
      Nothing => Nothing
      Just (l, rest) =>
        if l.indent /= indent then Nothing
        else
          let content = lineContent l in
            case splitMapping content of
              Nothing => Nothing
              Just (keyStr, restVal) =>
                case parseKey keyStr of
                  Nothing => Nothing
                  Just key =>
                    if restVal == "" then
                      case parseNestedValue indent rest of
                        Just (v, rest2) => Just ((key, v), rest2)
                        Nothing => Nothing
                    else case blockIndicator restVal of
                           Just isLit =>
                             case parseBlockScalar indent isLit rest of
                               Just (txt, rest2) => Just ((key, JString txt), rest2)
                               Nothing => Nothing
                           Nothing =>
                             case parseInlineValue restVal rest of
                               Just v => Just ((key, v), rest)
                               Nothing => Nothing

  parseKey : String -> Maybe String
  parseKey s =
    let t = ytrim s in
      case unpack t of
        ('\'' :: _) => parseSingleQuoted t
        ('"' :: _) => parseDoubleQuoted t
        _ => if t == "" then Nothing else Just t

  parseSequence : Nat -> List Line -> Maybe (JSON, List Line)
  parseSequence indent ls =
    case parseSeqItem indent ls of
      Nothing => Nothing
      Just (v, rest) =>
        let (items, rest2) = parseSeqMore indent [v] rest in
          Just (JArray items, rest2)

  parseSeqMore : Nat -> List JSON -> List Line -> (List JSON, List Line)
  parseSeqMore indent acc ls =
    case nextNonSkippable ls of
      Nothing => (acc, ls)
      Just (l, _) =>
        if l.indent /= indent then (acc, ls)
        else
          let content = lineContent l in
            if isSequenceLine content then
              case parseSeqItem indent ls of
                Just (v, rest) => parseSeqMore indent (acc ++ [v]) rest
                Nothing => (acc, ls)
            else (acc, ls)

  parseSeqItem : Nat -> List Line -> Maybe (JSON, List Line)
  parseSeqItem indent ls =
    case nextNonSkippable ls of
      Nothing => Nothing
      Just (l, rest) =>
        if l.indent /= indent then Nothing
        else
          let content = lineContent l in
            case unpack content of
              ('-' :: cs) =>
                if cs == [] then
                  parseNestedValue indent rest
                else case cs of
                       (' ' :: _) =>
                         let (spaces, restChars) = spanAfterDash cs 0
                             restStr = ytrimLeft (pack restChars)
                             itemIndent = indent + 1 + spaces
                         in if restStr == "" then
                              parseNestedValue indent rest
                            else case blockIndicator restStr of
                                   Just isLit =>
                                     case parseBlockScalar indent isLit rest of
                                       Just (txt, rest2) => Just (JString txt, rest2)
                                       Nothing => Nothing
                                   Nothing =>
                                     case splitMapping restStr of
                                       Just (k, v) =>
                                         parseInlineMapping itemIndent (k, v) rest
                                       Nothing =>
                                         case parseInlineValue restStr rest of
                                           Just v => Just (v, rest)
                                           Nothing => Nothing
                       _ => Nothing
              _ => Nothing
    where
      spanAfterDash : List Char -> Nat -> (Nat, List Char)
      spanAfterDash [] n = (n, [])
      spanAfterDash (c :: cs) n =
        if c == ' ' then spanAfterDash cs (n + 1)
        else (n, c :: cs)

  parseInlineMapping : Nat -> (String, String) -> List Line -> Maybe (JSON, List Line)
  parseInlineMapping indent (k, v) rest with (parseKey k)
    parseInlineMapping indent (k, v) rest | Nothing = Nothing
    parseInlineMapping indent (k, v) rest | Just key =
        let firstEntry : Maybe ((String, JSON), List Line)
            firstEntry =
              if v == "" then
                case parseNestedValue indent rest of
                  Just (val, rest2) => Just ((key, val), rest2)
                  Nothing => Nothing
              else case blockIndicator v of
                     Just isLit =>
                       case parseBlockScalar indent isLit rest of
                         Just (txt, rest2) => Just ((key, JString txt), rest2)
                         Nothing => Nothing
                     Nothing =>
                       case parseInlineValue v rest of
                         Just val => Just ((key, val), rest)
                         Nothing => Nothing
        in case firstEntry of {
             Nothing => Nothing;
             Just (entry, rest2) =>
               let (entries, rest3) = parseMappingMore indent [entry] rest2 in
                 Just (JObject entries, rest3)
           }

public export
parseYAML : String -> Maybe JSON
parseYAML s =
  case lexLines s of
    Nothing => Nothing
    Just ls =>
      if allSkippable ls then Just JNull
      else
        case parseValueAt 0 ls of
          Just (v, rest) => if allSkippable rest then Just v else Nothing
          Nothing => Nothing

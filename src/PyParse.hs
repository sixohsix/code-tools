
module PyParse where

import Data.String.Utils (join)


data PyToken = OpenBrace
             | CloseBrace
             | Star
             | Str String
             | Comma
             | Whitespace
             | Import
             | From
             | As
             | Newline
             | Hash String
               deriving (Show, Eq)

eatWhitespaceChars "" = ""
eatWhitespaceChars (x:xs)
  | (x == ' ') || (x == '\t') = eatWhitespaceChars xs
  | otherwise = (x:xs)

concatUntilNewline acc [] = (acc, [])
concatUntilNewline acc (t:ts) = case t of
  '\n' -> (acc, ts)
  otherwise -> concatUntilNewline (acc ++ [t]) ts

lexStr accStr "" = (accStr, "")
lexStr accStr (x:xs)
  | any (\c -> c == x) "()*,\n " = (accStr, (x:xs))
  | otherwise = lexStr (accStr ++ [x]) xs

tokenizeStr str
  | str == "import" = Import
  | str == "from" = From
  | str == "as" = As
  | otherwise = Str str

tokenize "" = []
tokenize (x:xs)
  | x == '(' = [OpenBrace] ++ tokenize xs
  | x == ')' = [CloseBrace] ++ tokenize xs
  | x == '*' = [Star] ++ tokenize xs
  | x == ',' = [Comma] ++ tokenize xs
  | x == '\n' = [Newline] ++ tokenize xs
  | x == ' ' = [Whitespace] ++ tokenize (eatWhitespaceChars xs)
  | x == '#' = let (commentStr, rest) = concatUntilNewline "" xs
               in [Hash commentStr] ++ tokenize rest
  | otherwise = let (str, rest) = lexStr "" (x:xs)
                in [tokenizeStr str] ++ tokenize rest

parseError = error "Parse error"

lineTooLong = (<=) 80 . length

type PyModuleName = String
data PyModule = PyModuleAs PyModuleName String
              | PyModule PyModuleName
                deriving (Show, Eq, Ord)
data PyImportStmt = StmtImport [PyModule]
                  | StmtFrom PyModuleName [PyModule]
                  | StmtComment String
                  | StmtSpace
                    deriving (Show, Eq)

class ShowAsPython a where
  showAsPython :: a -> String

instance ShowAsPython PyModule where
  showAsPython pm = case pm of
    (PyModuleAs pyMod pyModName) -> pyMod ++ " as " ++ pyModName
    (PyModule pyMod) -> pyMod

instance ShowAsPython PyImportStmt where
  showAsPython pis =
    let modulesStr = map showAsPython
        modStr = \modules -> join ", " (modulesStr modules)
        modStrBraces = \modules ->
          "(\n    " ++ (join ",\n    " (modulesStr modules)) ++ ",\n    )"
    in case pis of
      (StmtImport modules) ->
        let oneLine = "import " ++ modStr modules
            multiLine = "import " ++ modStrBraces modules
        in if lineTooLong oneLine then multiLine else oneLine
      (StmtFrom mod modules) ->
        let oneLine = "from " ++ mod ++ " import " ++ modStr modules
            multiLine = "from " ++ mod ++ " import " ++ modStrBraces modules
        in if lineTooLong oneLine then multiLine else oneLine
      (StmtComment s) -> "# " ++ s
      (StmtSpace) -> "\n"

consume tTypes [] = []
consume tTypes (t:tokens)
  | any ((==) t) tTypes = consume tTypes tokens
  | otherwise = t:tokens

consumeWN = consume [Whitespace, Newline]

ignore tTypes [] = []
ignore tTypes (t:tokens)
  | any ((==) t) tTypes = ignore tTypes tokens
  | otherwise = t:(ignore tTypes tokens)

ignoreW = ignore [Whitespace]
ignoreWN = ignore [Whitespace, Newline]

parseTop [] = []
parseTop tokens = case (consumeWN tokens) of
  [] -> []
  (t:ts) -> case t of
    Import -> let (mods, rest) = parseImport ts
              in [StmtImport mods] ++ parseTop rest
    From -> let (mod, mods, rest) = parseFrom ts
            in [StmtFrom mod mods] ++ parseTop rest
    Hash str -> [StmtComment str] ++ parseTop ts
    otherwise -> parseError

parseImport [] = parseError
parseImport tokens = case (consume [Whitespace] tokens) of
  [] -> parseError
  (t:ts) -> case t of
    OpenBrace -> let (modules, rest) = parseModulesBraces ts
                 in case rest of
                   (CloseBrace:restAfter) -> (modules, restAfter)
                   otherwise -> parseError
    Str str -> let (modules, rest) = (parseModulesReg tokens)
               in (modules, rest)
    otherwise -> parseError

parseFrom [] = parseError
parseFrom tokens = case ignoreW tokens of
  (Str s0 : Import : ts) ->
    let (mods, rest) = parseImport ts
    in (s0, mods, rest)
  otherwise -> parseError

commaContinue pf tokens = case tokens of
  (t:ts) -> case t of
    Comma -> pf ts
    otherwise -> ([], tokens)
  otherwise -> ([], tokens)

parseModulesReg = parseModules ignoreW
parseModulesBraces = parseModules ignoreWN

parseModules _ [] = parseError
parseModules ignore tokens =
  let tokens' = ignore tokens
      parseMore = commaContinue (parseModules ignore)
  in case tokens' of
    (Str s0 : As : Str s1 : ts) ->
      let (modules, rest) = parseMore ts
      in ([PyModuleAs s0 s1] ++ modules, rest)
    (Str s0 : ts) ->
      let (modules, rest) = parseMore ts
      in ([PyModule s0] ++ modules, rest)
    t@(CloseBrace:_) -> ([], t)
    otherwise -> parseError

showStmtListAsPython :: ShowAsPython a => [a] -> String
showStmtListAsPython = join "\n" . map showAsPython

lexParseImports imports = parseTop (tokenize imports)

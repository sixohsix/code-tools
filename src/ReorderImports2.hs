
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
               deriving (Show, Eq)

eatWhitespaceChars "" = ""
eatWhitespaceChars (x:xs)
  | (x == ' ') || (x == '\t') = eatWhitespaceChars xs
  | otherwise = (x:xs)

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
  | otherwise = let (str, rest) = lexStr "" (x:xs)
                in [tokenizeStr str] ++ tokenize rest

parseError = error "Parse error"

type PyModuleName = String
data PyModule = PyModuleAs PyModuleName String
              | PyModule PyModuleName
                deriving (Show)
data PyImportStmt = StmtImport [PyModule]
                  | StmtFrom PyModuleName [PyModule]
                    deriving (Show)
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
    Import -> let (imp, rest) = parseImport ts
              in [imp] ++ parseTop rest
--    From -> let (imp, rest) = parseFrom ts
--            in [imp] ++ parseTop rest
    otherwise -> parseError

parseImport [] = parseError
parseImport tokens = case (consume [Whitespace] tokens) of
  [] -> parseError
  (t:ts) -> case t of
--    OpenBrace -> let (modules, rest) = parseModulesInBraces ts
--                 in (StmtImport modules, rest)
    Str str -> let (modules, rest) = (parseModules (t:ts))
               in (StmtImport modules, rest)
    otherwise -> parseError

commaContinue pf tokens = case tokens of
  (t:ts) -> case t of
    Comma -> pf ts
    otherwise -> ([], tokens)
  otherwise -> ([], tokens)

parseModules [] = parseError
parseModules tokens =
  let tokens' = ignoreW tokens in case tokens' of
    (Str s0 : As : Str s1 : ts) ->
      let (modules, rest) = commaContinue parseModules ts
      in ([PyModuleAs s0 s1] ++ modules, rest)
    (Str s0 : ts) ->
      let (modules, rest) = commaContinue parseModules ts
      in ([PyModule s0] ++ modules, rest)
    otherwise -> parseError

lexParseImports imports = parseTop (tokenize imports)

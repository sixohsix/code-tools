module Main where

import qualified Control.Exception as CE

import Data.List (sort, sortBy, isPrefixOf, elemIndex)
import Data.Function (on)
import PyParse (
  showStmtListAsPython,
  lexParseImports,
  PyImportStmt (StmtFrom, StmtImport, StmtComment),
  PyModule (PyModule, PyModuleAs),
  ParseError)

pkgClasses :: [String]
pkgClasses = [ "__future__"
             , "*"
             , "abl"
             , "ableton"
             , "django_cms"
             , ""
             ]

pkgClassIdxOf :: String -> Int
pkgClassIdxOf pkg = case elemIndex pkg pkgClasses of
  Just pkgPos -> pkgPos
  Nothing -> case elemIndex "*" pkgClasses of
    Just star -> star
    Nothing -> 0

dropDots = dropWhile (== '.')

getPackage (StmtFrom name _) = dropDots name
getPackage (StmtImport (mod:_)) = dropDots $ getMod mod
getPackage other = undefined

getMod (PyModule m) = m
getMod (PyModuleAs m _) = m

splitImports (StmtImport m) = map (StmtImport . return) m
splitImports o = [o]

reorderFromImports (StmtFrom mod mods) = (StmtFrom mod (sort mods))
reorderFromImports o = o

sortImports = sortBy (
  compare `on` (
     \a -> (pkgClassIdxOf $ getPackage a, getPackage a)))

isComment c = case c of
  StmtComment _ -> True
  otherwise -> False

stripComments = filter (not . isComment)

killDupes :: [PyImportStmt] -> [PyImportStmt]
killDupes (s0:s1:ss) =
  if (s0 == s1)
  then s0:(killDupes ss)
  else s0:(killDupes $ s1:ss)
killDupes ss = ss

reformat = showStmtListAsPython
           . killDupes
           . sortImports
           . stripComments
           . map reorderFromImports
           . concat . map splitImports
           . lexParseImports

catchParseErr :: IO String -> (ParseError -> IO String) -> IO String
catchParseErr = CE.catch

safeInteract f = do
  input <- getContents
  output <- catchParseErr
              (CE.evaluate $ f input)
              (\e -> return (input ++ "\n# !!! Parse error!\n"))
  putStrLn output

main = safeInteract reformat


module Main where

import Data.List (sort, sortBy, isPrefixOf, elemIndex)
import PyParse (
  showStmtListAsPython,
  lexParseImports,
  PyImportStmt (StmtFrom, StmtImport),
  PyModule (PyModule, PyModuleAs))

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

getPackage (StmtFrom name _) = name
getPackage (StmtImport (mod:_)) = getMod mod
getMod (PyModule m) = m
getMod (PyModuleAs m _) = m

splitImports (StmtImport m) = map (StmtImport . return) m
splitImports o = [o]

reorderFromImports (StmtFrom mod mods) = (StmtFrom mod (sort mods))
reorderFromImports o = o

sortImports = sortBy (\a b -> compare
                              (pkgClassIdxOf $ getPackage a)
                              (pkgClassIdxOf $ getPackage b)
                     )

reformat = showStmtListAsPython . sortImports . map reorderFromImports
           . concat . map splitImports . lexParseImports

safeInteract f = do
  input <- getContents
  output <- (return $ f input) `catch` (
    \e -> return ("# Parse error. Didn't work.\n\n" ++ input)
    )
  putStr output

main = safeInteract reformat

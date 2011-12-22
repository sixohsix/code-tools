
module Main where

import Data.List (sort, sortBy, isPrefixOf, elemIndex)
import PyParse (
  showStmtListAsPython,
  lexParseImports,
  PyImportStmt (StmtFrom, StmtImport, StmtComment),
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

takeUntilDot = takeWhile (not . ((==) '.'))

getPackage (StmtFrom name _) = takeUntilDot name
getPackage (StmtImport (mod:_)) = takeUntilDot $ getMod mod
getPackage other = undefined

getMod (PyModule m) = m
getMod (PyModuleAs m _) = m

splitImports (StmtImport m) = map (StmtImport . return) m
splitImports o = [o]

reorderFromImports (StmtFrom mod mods) = (StmtFrom mod (sort mods))
reorderFromImports o = o

sortImports = sortBy (
  \a b -> let pa = getPackage a
              pb = getPackage b
              pkgClsCompare = compare
                              (pkgClassIdxOf pa)
                              (pkgClassIdxOf pb)
          in case pkgClsCompare of
            EQ -> compare pa pb
            otherwise -> pkgClsCompare
  )

isComment c = case c of
  StmtComment _ -> True
  otherwise -> False

stripComments = filter (not . isComment)

killDupes :: [PyImportStmt] -> [PyImportStmt]
killDupes (s0:s1:ss) = if (s0 == s1)
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

safeInteract f = do
  input <- getContents
  output <- (return $ f input) `catch` (
    \e -> return ("# Parse error. Didn't work.\n\n" ++ input)
    )
  putStr output

main = safeInteract reformat

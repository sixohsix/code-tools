module Main where

import Data.Ord
import Data.List (sortBy, foldl')
import Data.Monoid

import Language.Python.Common
import Language.Python.Version2.Parser

statementsOfModule m =
  let Module stmts = m
  in stmts

join sep strLst =
  let j acc str = acc ++ sep ++ str
  in  case strLst of
    []         -> ""
    (str:rest) -> foldl' j str rest


pyAlphabetizeStmts :: ModuleSpan -> String
pyAlphabetizeStmts =
  join "\n" . fmap prettyText . sortBy compareStmts . statementsOfModule

pyAlphabetize :: String -> String
pyAlphabetize inp =
  (either (\_ -> inp ++ "\n!!! Parsing failed.")
   (pyAlphabetizeStmts . fst)
   (parseModule inp "stdin")) ++ "\n"

dedecorated (Decorated _ def _ ) = dedecorated def
dedecorated s                    = s

stmtTypeOrd :: StatementSpan -> Int
stmtTypeOrd (Import _ _)       = 0
stmtTypeOrd (FromImport _ _ _) = 0
stmtTypeOrd (Fun _ _ _ _ _)    = 1
stmtTypeOrd (Class _ _ _ _)    = 1
stmtTypeOrd (Assign _ _ _)     = 1
stmtTypeOrd _                  = 1000000

unknownName = "zzzzzzzzz"

exprNameOrd (Var (Ident name _) _) = name
exprNameOrd (Tuple exprs _) = exprNameOrd (head exprs)
exprNameOrd _               = unknownName

stmtNameOrd :: StatementSpan -> String
stmtNameOrd (Fun (Ident name _) _ _ _ _) = name
stmtNameOrd (Class (Ident name _) _ _ _) = name
stmtNameOrd (Assign v _ _)               = exprNameOrd (head v)
stmtNameOrd _                            = unknownName

compareStmts :: StatementSpan -> StatementSpan -> Ordering
compareStmts l r =
  let doComp f = compare (f $ dedecorated l) (f $ dedecorated r)
  in mconcat [
    doComp stmtTypeOrd,
    doComp stmtNameOrd]

main = interact pyAlphabetize

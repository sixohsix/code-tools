
import Data.List (sortBy)

comparePackageOrder :: String -> String -> Ordering
comparePackageOrder l r =
  compare (package l) (package r) where
    package importLine = case words importLine of
      (_:x:xs)  -> x
      otherwise -> "zzzzzzzzzzzzzz"


reorderImports :: String -> String
reorderImports imports = 
  let
    importLines = lines imports
  in
    unlines (sortBy comparePackageOrder importLines)


main :: IO ()
main = interact reorderImports

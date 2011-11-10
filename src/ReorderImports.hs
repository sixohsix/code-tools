
import Data.List (sortBy)
import Data.List (isPrefixOf, elemIndex)


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


comparePackageOrder :: String -> String -> Ordering
comparePackageOrder l r =
  let mod importLine = case words importLine of
        (_:x:xs)  -> Just x
        otherwise -> Nothing
      mModL = mod l
      mModR = mod r
  in case (mModL, mModR) of
    (Just mL, Just mR) -> 
      let package m = case elemIndex '.' m of
            Just x -> fst (splitAt x m)
            Nothing -> m
          pkgL = package mL
          pkgR = package mR
          pkgClsL = pkgClassIdxOf pkgL
          pkgClsR = pkgClassIdxOf pkgR
      in if pkgClsL /= pkgClsR
         then compare pkgClsL pkgClsR
         else compare mL mR
    otherwise -> compare mModL mModR

stitchImportLines :: [String] -> [String]
stitchImportLines lines = filter ("" /=) (stitchImportLines' lines "" [])

stitchImportLines' :: [String] -> String -> [String] -> [String]
stitchImportLines' [] cur acc = acc ++ [cur]
stitchImportLines' (x:xs) cur acc =
  if any (\lineStart -> isPrefixOf lineStart x) ["import", "from"]
  then stitchImportLines' xs x (acc ++ [cur])
  else stitchImportLines' xs (cur ++ "\n" ++ x) acc

reorderImports :: String -> String
reorderImports imports = 
  let importLines = stitchImportLines (lines imports)
  in
    unlines (sortBy comparePackageOrder importLines)

main :: IO ()
main = interact reorderImports

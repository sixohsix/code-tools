
indent4 :: String -> String
indent4 s =
  let
    inputLines = lines s
  in unlines $ map (\line -> "    " ++ line) inputLines

main :: IO ()
main = interact indent4

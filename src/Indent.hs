
indent4 :: String -> String
indent4 s =
  let
    inputLines = lines s
  in unlines $ map (\line -> if (line /= "")
                             then "    " ++ line
                             else line)
     inputLines

main :: IO ()
main = interact indent4

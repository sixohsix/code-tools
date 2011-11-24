
safeToDedent :: String -> Bool
safeToDedent l = all (' ' ==) (take 4 l)


dedent4 :: String -> String
dedent4 s =
  let
    inputLines = lines s
    outputLines =
      if all safeToDedent inputLines
      then map (drop 4) inputLines
      else inputLines
  in unlines outputLines


main :: IO ()
main = interact dedent4

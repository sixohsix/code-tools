
dedent4 :: String -> String
dedent4 s =
  let
    inputLines = lines s
    safeToDedent = all (\l -> "    " == (take 4 l)) inputLines
    outputLines = if safeToDedent
       then map (drop 4) inputLines
       else inputLines
  in unlines outputLines


main :: IO ()
main = interact dedent4

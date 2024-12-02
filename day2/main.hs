import Data.List.Split (splitOn)

readFileLines :: FilePath -> IO [String]
readFileLines filepath = lines <$> readFile filepath

parseLine :: String -> [Int]
parseLine str = map read (splitOn " " str)

checkValidity :: [Int] -> Bool
checkValidity list =
  let first = list[0] - list[1]
  reduce (&) [ | ]

main :: IO ()
main = do
  content <- readFileLines "./day2/input.txt"
  let parsed = map parseLine content
  print parsed

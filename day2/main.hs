import Data.List.Split (splitOn)

readFileLines :: FilePath -> IO [String]
readFileLines filepath = lines <$> readFile filepath

parseLine :: String -> [Int]
parseLine str = map read (splitOn " " str)

-- first star
checkValidity :: [Int] -> Bool
checkValidity list =
  let sign = list !! 0 - list !! 1
      diffs = zipWith (-) list (tail list)
   in all (\diff -> diff * sign > 0 && abs diff < 4) diffs

-- second star
removeOne :: [Int] -> [[Int]]
removeOne [] = []
removeOne (x : xs) = xs : map (x :) (removeOne xs)

checkValiditish :: [Int] -> Bool
checkValiditish list = any checkValidity (removeOne list)

main :: IO ()
main = do
  content <- readFileLines "./day2/input.txt"
  let res1 = sum (map ((\b -> if b then 1 else 0) . checkValidity . parseLine) content)
  print ("1st star: " ++ show res1)
  let res2 = sum (map ((\b -> if b then 1 else 0) . checkValiditish . parseLine) content)
  print ("2nd star: " ++ show res2)

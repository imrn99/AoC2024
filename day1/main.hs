import Data.List (nub, sort, unzip)
import Data.List.Split (splitOn)
import Data.Map (Map, fromListWith, toList)
import System.IO
import Prelude hiding (lookup)

-- common
readFileLines :: FilePath -> IO [String]
readFileLines filepath = lines <$> readFile filepath

parseLine :: String -> (Int, Int)
parseLine str =
  let [n, m] = splitOn "   " str
   in (read n, read m)

parseIntoLists :: [String] -> ([Int], [Int])
parseIntoLists strs = unzip (map parseLine strs)

-- second star
countUnique :: [Int] -> [(Int, Int)]
countUnique list = toList $ fromListWith (+) [(x, 1) | x <- list]

countOccurences :: Int -> [Int] -> Int
countOccurences x = length . filter (x ==)

main :: IO ()
main = do
  content <- readFileLines "./day1/input.txt"
  let (ll, rl) = parseIntoLists content

  -- first star
  let (lls, rls) = (sort ll, sort rl)
  let res1 = sum (zipWith (\x y -> abs (x - y)) lls rls)
  print ("1st star: " ++ show res1)

  -- second star
  let llu = countUnique ll
  let occs = [countOccurences r rls | r <- map fst llu]
  let res2 = sum (zipWith (\(val, c1) c2 -> val * c1 * c2) llu occs)
  print ("2nd star: " ++ show res2)

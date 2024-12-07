
import Data.List (tails)
import Data.List.Split (splitWhen)
import Data.Map (Map, fromListWith, lookup)
import System.IO
import Prelude hiding (lookup)

type Rules = Map Int [Int]
type Entry = [Int]

-- common
readFileLines :: FilePath -> IO [String]
readFileLines filepath = lines <$> readFile filepath

parseFile :: [String] -> Maybe (Rules, [Entry])
parseFile file = 
  case splitWhen (== "") file of -- hacky way to split on the empty line
    [n, m] ->
      let 
        readRules :: [String] -> Rules
        readRules rules = fromListWith (++) [(x, [y]) | (x, y) <- map parseRule rules]

        parseRule :: String -> (Int, Int)
        parseRule str = 
          let (xStr, '|' : yStr) = break (== '|') str
          in (read xStr, read yStr)

        parseEntry :: String -> Entry
        parseEntry str = 
          let clean = tail (init str)
              nums = map read (words (map (\c -> if c == ',' then ' ' else c) clean))
          in nums
      in Just (readRules n, map parseEntry m)
    _ -> Nothing

middle :: [a] -> a
middle l@(_:_:_:_) = middle $ tail $ init l
middle l           = head l

-- first star
isEntryValid :: Rules -> Entry -> Bool
isEntryValid rules list =
  let processSeq :: [Int] -> Bool
      processSeq [ ] = True
      processSeq [a] = True
      processSeq seq = let check = head seq
                           others = tail seq
                       in and [ processItem check o | o <- others ]

      processItem :: Int -> Int -> Bool
      processItem check other = case lookup other rules of 
        Just v -> check `notElem` v
        Nothing -> True
  in and [processSeq seq | seq <- tails list]

main :: IO ()
main = do
  content <- readFileLines "day5/input.txt"
  case parseFile content of 
    Just (rules, entries) -> do
      let res1 = sum (map middle (filter (isEntryValid rules) entries))
      print ("1st star: " ++ show res1)
    Nothing -> 
      print "E: couldn't parse input"
  

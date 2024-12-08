import Data.List.Split (splitOn, splitWhen)
import Data.List (zip)
import Data.Maybe (mapMaybe)
import Control.Monad (replicateM)

data Op = Add | Mult
type Entry = (Int, [Int])

readFileLines :: FilePath -> IO [String]
readFileLines filepath = lines <$> readFile filepath

parseLine :: String -> Maybe Entry
parseLine str = case splitOn ": " str of
  [resStr, args] -> Just (read resStr, [read xStr | xStr <- splitWhen (== ' ') args])
  _ -> Nothing 

compositions :: [Int] -> [Int]
compositions xs = 
    [ foldl applyOp (head xs) (zip (tail xs) ops)
    | ops <- replicateM (length xs - 1) [(+), (*)]
    ]
  where
    applyOp acc (val, op) = op acc val

solveEquations :: String -> Maybe Int
solveEquations str = case parseLine str of
  Nothing          -> Nothing
  Just (res, args) ->
    let solutions = compositions args
    in if res `elem` solutions then Just res else Nothing

main :: IO ()
main = do
  content <- readFileLines "./day7/input.txt"
  print ("1st star: " ++ show (sum (mapMaybe solveEquations content)))

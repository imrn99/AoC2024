import Data.Char (isDigit)
import Data.List (tails)

readFileLines :: FilePath -> IO [String]
readFileLines filepath = lines <$> readFile filepath

findMulPatterns :: String -> [(Int, Int)]
findMulPatterns str =
  [extractNumbers s | s <- tails str, isValidMulPattern s]
  where
    extractNumbers s =
      let contents = takeWhile (/= ')') (drop 4 s)
          (xStr, ',' : yStr) = break (== ',') contents
       in (read xStr, read yStr)

    isValidMulPattern s =
      take 4 s == "mul("
        && ')' `elem` s
        && validArgs (drop 4 s)

    validArgs s =
      case break (== ')') s of
        (contents, _ : _) ->
          case break (== ',') contents of
            (x, ',' : y) ->
              all isDigit x
                && all isDigit y
                && not (null x)
                && not (null y)
            _ -> False
        _ -> False

main :: IO ()
main = do
  content <- readFileLines "day3/input.txt"
  let pairs = concatMap findMulPatterns content
  print ("1st star: " ++ show (sum (map (uncurry (*)) pairs)))

import Data.List (inits, tails)
import Data.Maybe (catMaybes)

readFileLines :: FilePath -> IO [String]
readFileLines filepath = lines <$> readFile filepath

directions :: [(Int, Int)]
directions = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]

type Grid = [String]

getGridChar :: Grid -> (Int, Int) -> Char
getGridChar grid (x, y)
  | x >= 0
      && x < length grid
      && y >= 0
      && y < length (head grid) =
      grid !! x !! y
  | otherwise = '.'

-- first star

checkXmas :: Grid -> (Int, Int) -> (Int, Int) -> Int
checkXmas grid start@(x, y) dir@(dx, dy) =
  let path = take 4 [(x + i * dx, y + i * dy) | i <- [0 ..]]
      chars = map (getGridChar grid) path
   in if chars == "XMAS" then 1 else 0

countXmas :: Grid -> Int
countXmas grid =
  sum
    [ checkXmas grid (x, y) dir
      | x <- [0 .. length grid - 1],
        y <- [0 .. length (head grid) - 1],
        getGridChar grid (x, y) == 'X',
        dir <- directions
    ]

-- second star

checkMas :: Grid -> (Int, Int) -> Int
checkMas grid (x, y) =
  -- attrocious but it works; I'm sure you can solve this shit with clever XORs tho
  let tl = getGridChar grid (x - 1, y - 1)
      bl = getGridChar grid (x - 1, y + 1)
      tr = getGridChar grid (x + 1, y - 1)
      br = getGridChar grid (x + 1, y + 1)
      c1 = tl == 'M' && bl == 'M' && tr == 'S' && br == 'S'
      c2 = tl == 'M' && bl == 'S' && tr == 'M' && br == 'S'
      c3 = tl == 'S' && bl == 'S' && tr == 'M' && br == 'M'
      c4 = tl == 'S' && bl == 'M' && tr == 'S' && br == 'M'
   in if c1 || c2 || c3 || c4
        then 1
        else 0

countMas :: Grid -> Int
countMas grid =
  sum
    [ checkMas grid (x, y)
      | x <- [1 .. length grid - 2],
        y <- [1 .. length (head grid) - 2],
        getGridChar grid (x, y) == 'A'
    ]

main :: IO ()
main = do
  content <- readFileLines "day4/input.txt"
  let res1 = countXmas content
  putStrLn $ "1st star: " ++ show res1
  let res2 = countMas content
  putStrLn $ "2nd star: " ++ show res2

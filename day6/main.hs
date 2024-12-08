
import Prelude hiding (Right, Left)
import Data.List (find, unfoldr)
import Data.Set (fromList)

readFileLines :: FilePath -> IO [String]
readFileLines filepath = lines <$> readFile filepath

type Grid = [String]
type Position = (Int, Int)

data Direction = Up | Right | Down | Left

turn :: Direction -> Direction
turn d = case d of
  Up -> Right
  Right -> Down
  Down -> Left
  Left -> Up

-- this can return
-- '.' if regular
-- '#' if obstacle
-- 'o' if outside
getGridChar :: Grid -> Position -> Char
getGridChar grid (x, y)
  | x >= 0
      && x < length grid
      && y >= 0
      && y < length (head grid) =
      grid !! x !! y
  | otherwise = 'o'

findStartPos :: Grid -> Maybe Position
findStartPos grid = 
  case find (\(c,x,y) -> c == '^') [ (grid !! x !! y, x, y) | x <- [0..length       grid  - 1],
                                                              y <- [0..length (head grid) - 1]]
  of 
    Just (_, x, y) -> Just (x, y)
    Nothing -> Nothing

traverseGrid :: Grid -> [Position]
traverseGrid grid = case findStartPos grid of
  Nothing    -> [] -- unclean, whatever
  Just start ->
    let nextState :: (Position, Direction) -> (Position, Direction)
        nextState ((x, y), d) = 
          let inspect = case d of 
                Up    -> (x - 1, y    )
                Right -> (x    , y + 1)
                Down  -> (x + 1, y    )
                Left  -> (x    , y - 1) 
          in if getGridChar grid inspect == '#'
             then ((x, y), turn d)
             else (inspect, d)

        reachedOutside :: Position -> Bool
        reachedOutside pos = getGridChar grid pos == 'o'
        
    in unfoldr (\(p, d) -> if reachedOutside p 
                           then Nothing 
                           else Just (p, nextState (p, d))) (start, Up)

main :: IO ()
main = do
  grid <- readFileLines "day6/input.txt"
  let poss = traverseGrid grid
  let res1 = length (fromList poss)
  print ("1st star: " ++ show res1)

import Utils
import Text.Parsec --https://hackage.haskell.org/package/parsec/docs/Text-Parsec.html 
import Control.Monad --https://hackage.haskell.org/package/base/docs/Control-Monad.html 
import qualified Data.Set as Set --https://hackage.haskell.org/package/containers/docs/Data-Set.html 

robotsPath = "Aoc2024/Day14/robots.in"


data Robot = Robot { pos :: (Int, Int), speed :: (Int, Int) } deriving Show


robot :: StringParser Robot
robot = do
   string "p="
   a <- vector
   string " v="
   Robot a <$> vector
   where
      vector = flip (,) <$> integral <* char ',' <*> integral


(rows, cols) = (103, 101)


simulate :: Robot -> Robot
simulate robot = robot { pos = newPos }
   where
      newPos = let (newY, newX) = pos robot /+ speed robot
               in ((newY + rows) `mod` rows, (newX + cols) `mod` cols)


maybeExistsPicture :: Set.Set (Int, Int) -> Bool
maybeExistsPicture set = any (\p -> all ((`Set.member` set) . (p/+)) offsets) set
   where
      offsets = map (1, ) [-1..1] ++ map (2, ) [-2..2]


printState :: [Robot] -> Int -> IO [Robot]
printState robots i = do

   let set = Set.fromList $ map pos robots

   when (maybeExistsPicture set) $ do
      putStrLn $ "\nmaybe the solution is: " ++ show i
      putStrLn "Check that the grid below has a xmas tree"
      let isRobot p = any ((==p) . pos) robots
      let content = map (\r -> map (\c -> if (r, c) `Set.member` set then '@' else '.') [0..cols-1]) [0..rows-1]
      mapM_ putStrLn content
   
   return $ map simulate robots


main :: IO ()
main = do

   robots <- parseFile robotsPath robot

   let robots' = map (\r -> pos $ iterate simulate r !! 100) robots
       yAxis = rows `div` 2
       xAxis = cols `div` 2

   let q1 = howMany (\(y, x) -> y > yAxis && x > xAxis) robots'
       q2 = howMany (\(y, x) -> y > yAxis && x < xAxis) robots'
       q3 = howMany (\(y, x) -> y < yAxis && x > xAxis) robots'
       q4 = howMany (\(y, x) -> y < yAxis && x < xAxis) robots'

   print $ q1 * q2 * q3 * q4

   foldM_ printState robots [0..]
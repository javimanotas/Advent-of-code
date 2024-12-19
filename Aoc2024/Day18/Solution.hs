import Utils
import Data.Maybe --https://hackage.haskell.org/package/base/docs/Data-Maybe.html 
import Text.Parsec --https://hackage.haskell.org/package/parsec/docs/Text-Parsec.html 
import qualified Data.Set as Set --https://hackage.haskell.org/package/containers/docs/Data-Set.html 

bytesPath = "Aoc2024/Day18/bytes.in"


size = 70


minSteps :: [(Int, Int)] -> Int -> Maybe Int
minSteps bytes cnt = dijkstra Set.empty $ singletonPQueue 0 (0, 0)
   where
      grid = Set.fromList [ (row, col)
                          | row <- [0..size]
                          , col <- [0..size]
                          , (row, col) `notElem` take cnt bytes]

      dijkstra visited pqueue
         | null pqueue = Nothing
         | pos == (size, size) = Just cost
         | pos `Set.member` visited = dijkstra visited pqueue'
         | otherwise = dijkstra (Set.insert pos visited) $ foldl (flip $ uncurry enqueueWithPrio) pqueue' moves
         where
            ((cost, pos), pqueue') = dequeueWithPrio pqueue

            moves = [(cost + 1, x) | x <- offsets2D pos, x `Set.member` grid]


binSearch :: [(Int, Int)] -> Int -> Int -> (Int, Int)
binSearch bytes bot top
   | bot >= top = bytes !! bot
   | possible mid = binSearch bytes (mid + 1) top
   | otherwise = binSearch bytes bot (mid - 1)
   where
      mid = (top + bot) `div` 2
      possible x = isJust $ minSteps bytes x


main :: IO ()
main = do

   bytes <- parseFile bytesPath $ (,) <$> integral <*> (char ',' >> integral)
   
   print $ minSteps bytes 1024
   print $ binSearch bytes 0 (length bytes - 1)
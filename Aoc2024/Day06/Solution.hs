import Utils
import qualified Data.Map as Map
import qualified Data.Set as Set

mapPath = "Aoc2024/Day06/map.in"


move :: Matrix Char -> (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
move mtx pos dir@(x, y) = case Map.lookup (pos /+ dir) mtx of
   Just '#' -> move mtx pos (y, -x)
   _ -> (pos /+ dir, dir)


findPath :: Matrix Char -> (Int, Int) -> (Int, Int) -> Set.Set (Int, Int)
findPath mtx p v
   | p `Map.notMember` mtx = Set.empty
   | otherwise = Set.insert p $ uncurry (findPath mtx) $ move mtx p v


hasLoop :: Matrix Char -> (Int, Int) -> (Int, Int) -> Bool
hasLoop mtx = aux Set.empty
   where
      aux set p v
         | p `Map.notMember` mtx = False
         | (p, v) `Set.member` set = True
         | otherwise = uncurry (aux $ Set.insert (p, v) set) $ move mtx p v


main :: IO ()
main = do
   
   mtx <- matrixFromList <$> fileLines mapPath
   
   let [start] = Map.keys $ Map.filter (== '^') mtx
   let path = findPath mtx start (-1, 0)
   print $ Set.size path

   print $ howMany (\p -> hasLoop (Map.insert p '#' mtx) start (-1, 0))
         $ Set.filter (/= start) path
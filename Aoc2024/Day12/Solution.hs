import Utils
import Data.List --https://hackage.haskell.org/package/base/docs/Data-List.html 
import qualified Data.Map as Map --https://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html 
import qualified Data.Set as Set --https://hackage.haskell.org/package/containers/docs/Data-Set.html 

gardenPath = "Aoc2024/Day12/garden.in"


splitInRegions :: ((Int, Int) -> (Int, Int) -> Bool)
               -> Set.Set (Int, Int) -> [Set.Set (Int, Int)]
splitInRegions expandCriteria unvisited
   | Set.null unvisited = []
   | otherwise = newGroup : splitInRegions expandCriteria (unvisited Set.\\ newGroup)
      where
         bfsStart = Set.findMin unvisited
         newGroup = bfs Set.empty [bfsStart]

         bfs :: Set.Set (Int, Int) -> [(Int, Int)] -> Set.Set (Int, Int)
         bfs visited [] = visited
         bfs visited positions = bfs visited' offsets
            where
               visited' = foldl (flip Set.insert) visited positions
               offsets = [x | x <- nub $ foldMap offsets2D positions
                            , expandCriteria x bfsStart
                            , x `Set.notMember` visited]


fencingPrice :: (Set.Set (Int, Int) -> Int) -> Matrix Char -> Int
fencingPrice cost garden = sum $ map ((*) . Set.size <*> cost) groups
   where
      positions = Set.fromList $ Map.keys garden
      expandCriteria x bfsStart = Map.lookup x garden == Just (garden Map.! bfsStart)
      groups = splitInRegions expandCriteria positions


perimeter :: Set.Set (Int, Int) -> Int
perimeter = howMany . flip Set.notMember <*> (foldMap offsets2D . Set.toList)


sides :: Set.Set (Int, Int) -> Int
sides set = sum $ map outerSides $ set : innerBlocks
   where
      ((minRow, minCol), (maxRow, maxCol)) = boxBounds set
      unnused = Set.fromList [(r, c) | r <- [minRow .. maxRow]
                                     , c <- [minCol .. maxCol]
                                     , (r, c) `Set.notMember` set]

      expandCriteria x _ = x `Set.member` unnused
      innerBlocks = filter (not . reachesCorner . Set.findMin) $ splitInRegions expandCriteria unnused

      reachesCorner :: (Int, Int) -> Bool
      reachesCorner = aux Set.empty . (:[])
         where
            aux _ [] = False
            aux visited positions = any (\(a, b) -> a <= minRow || a >= maxRow || b <= minCol || b >= maxCol) positions
                                  || aux visited' offsets
               where
                  visited' = foldl (flip Set.insert) visited positions
                  offsets = [x | x <- nub $ foldMap allOffsets2D positions
                               , x `Set.notMember` set
                               , x `Set.notMember` visited]


outerSides :: Set.Set (Int, Int) -> Int
outerSides set = loop min (0, 1) False
   where
      min = Set.findMin set

      loop pos dir visited
         | pos == min && dir == (0, 1) && visited = 0
         | (pos /+ turnL dir) `Set.member` set = 1 + loop (pos /+ turnL dir) (turnL dir) True
         | (pos /+ dir) `Set.member` set = loop (pos /+ dir) dir True
         | otherwise = 1 + loop pos (turnR dir) True


main :: IO ()
main = do

   garden <- matrixFromList <$> fileLines gardenPath
   mapM_ (print . (`fencingPrice` garden)) [perimeter, sides] 
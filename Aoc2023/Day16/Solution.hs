import Utils
import Data.List --https://hackage.haskell.org/package/base/docs/Data-List.html 
import Data.Ord --https://hackage.haskell.org/package/base/docs/Data-Ord.html 
import qualified Data.Map as Map --https://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html 
import qualified Data.Set as Set --https://hackage.haskell.org/package/containers/docs/Data-Set.html 

mirrorsPath = "Aoc2023/Day16/mirrors.in"


energizedTiles :: Matrix Char -> (Int, Int) -> (Int, Int) -> Int
energizedTiles mtx pos dir = Set.size $ Set.map fst $ aux Set.empty [(pos, dir)]
   where
      aux set rays
         | null valids = set
         | otherwise = aux (foldl (flip Set.insert) set valids) (nub $ valids >>= uncurry step)
         where
            valids = filter (\x@(a, _) -> a `Map.member` mtx && x `Set.notMember` set) rays
            step p d@(vy, vx) =
               let dirs = case mtx Map.! p of
                     '.' -> [d]
                     '/' -> mirror d
                     '\\' -> map ((-1) /*) $ mirror d
                     '|' -> if vy /= 0
                              then [d]
                              else [(1, 0), (-1, 0)]
                     '-' -> if vx /= 0
                              then [d]
                              else [(0, 1), (0, -1)]
               in map (\d -> (p /+ d, d)) dirs
            mirror d = (:[]) $ case d of
               (1, 0) -> (0, -1)
               (-1, 0) -> (0, 1)
               (0, 1) -> (-1, 0)
               (0, -1) -> (1, 0)


main :: IO ()
main = do

   mirrors <- matrixFromList <$> fileLines mirrorsPath
   
   print $ energizedTiles mirrors (0, 0) (0, 1)

   let [maxRow, maxCol] = map (\f -> f $ maximumBy (comparing f) $ Map.keys mirrors) [fst, snd]
   let rowCandidates = foldMap (\n -> [((n, 0), (0, 1)), ((n, maxCol), (0, -1))]) [0..maxRow]
   let colCandidates = foldMap (\n -> [((0, n), (1, 0)), ((maxRow, n), (-1, 0))]) [0..maxCol]
   print $ maximum $ map (uncurry $ energizedTiles mirrors) $ rowCandidates ++ colCandidates
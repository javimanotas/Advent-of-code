import Utils
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map

galaxiesPath = "Aoc2023/Day11/galaxies.in"


dst :: Int -> Set.Set Int -> Set.Set Int -> [(Int, Int)] -> Int
dst n emptyRows emptyCols path = sum $ [if a `Set.member` emptyRows || b `Set.member` emptyCols 
                                                                then n else 1
                                                          | (a, b) <- path]


path :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
path p0@(x0, y0) p1@(x1, y1) = delete p0 $ nub $ map (x0, ) [min y0 y1 .. max y0 y1] ++ map (, y1) [min x0 x1 .. max x0 x1]


pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = map (x, ) xs ++ pairs xs


main :: IO ()
main = do

    universe <- matrixFromList <$> fileLines galaxiesPath

    let galaxies = Map.keys $ Map.filter (== '#') universe
    
    let maxRow = maximum $ map fst $ Map.keys universe
    let maxCol = maximum $ map snd $ Map.keys universe

    let emptyRows = Set.fromAscList $ filter (\r -> all ((== '.') . (universe Map.!) . (r, )) [0..maxCol]) [0..maxRow]
    let emptyCols = Set.fromAscList $ filter (\c -> all ((== '.') . (universe Map.!) . (, c)) [0..maxRow]) [0..maxCol]

    let paths = map (uncurry path) $ pairs galaxies

    print $ sum [dst 2 emptyRows emptyCols p | p <- paths]
    print $ sum [dst 1000000 emptyRows emptyCols p | p <- paths]
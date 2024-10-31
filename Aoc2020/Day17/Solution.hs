import Utils
import Data.List
import qualified Data.Map as Map

gridPath = "Aoc2020/Day17/grid.in"


step :: Ord a => Map.Map a [a] -> Map.Map a Bool -> Map.Map a Bool
step n m = Map.mapWithKey (\k a -> let cnt = howMany id $ map (m Map.!) $ n Map.! k in
                                 if a
                                    then cnt `elem` [2, 3]
                                    else cnt == 3) m


offsets3 (a, b, c) = [ (x, y, z)
                     | x <- [a-1 .. a+1]
                     , y <- [b-1 .. b+1]
                     , z <- [c-1 .. c+1]
                     , (x, y, z) /= (a, b, c) ]


offsets4 (a, b, c, d) = [ (x, y, z, w)
                        | x <- [a-1 .. a+1]
                        , y <- [b-1 .. b+1]
                        , z <- [c-1 .. c+1]
                        , w <- [d-1 .. d+1]
                        , (x, y, z, w) /= (a, b, c, d) ]


main :: IO ()
main = do

    lines <- fileLines gridPath
    let cubes = Map.filter (== '#') $ matrixFromList lines
    
    let m = Map.fromList $ [ ((a, b, c), c == 0 && (a, b) `Map.member` cubes)
                           | a <- [-6..length lines + 6]
                           , b <- [-6..length lines + 6]
                           , c <- [-6..           1 + 6] ]
    let n = Map.mapWithKey (\k _ -> filter (`Map.member` m) $ offsets3 k) m
    print $ howMany id $ Map.elems $ iterate (step n) m !! 6

    let m' = Map.fromList $ [ ((a, b, c, d), c == 0 && d == 0 && (a, b) `Map.member` cubes)
                           | a <- [-6..length lines + 6]
                           , b <- [-6..length lines + 6]
                           , c <- [-6..           1 + 6]
                           , d <- [-6..           1 + 6] ]
    let n' = Map.mapWithKey (\k _ -> filter (`Map.member` m') $ offsets4 k) m'
    print $ howMany id $ Map.elems $ iterate (step n') m' !! 6
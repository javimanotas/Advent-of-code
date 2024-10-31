import Utils
import Data.Char
import qualified Data.Set as Set
import qualified Data.Map as Map

octopusesPath = "Aoc2021/Day11/octopuses.in"

size = 10


neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (a, b) = filter (\(a, b) -> a >= 0 && a < size && b >= 0 && b < size) [ (a + 1, b), (a - 1, b), (a, b + 1), (a, b - 1)
                   , (a + 1, b + 1), (a + 1, b - 1), (a - 1, b + 1), (a - 1, b - 1) ]


simulate :: Matrix Int -> Matrix Int
simulate = Map.map (\x -> if x > 9 then 0 else x) . flash Set.empty . Map.map (+1)
    where
        flash s m = case filter ((&&) . ((> 9) . (m Map.!)) <*> (`Set.notMember` s)) $ Map.keys m of
                [] -> m
                ks -> let neig = ks >>= neighbors
                          s' = foldl (flip Set.insert) s ks
                          m' = foldl (\m i -> Map.insert i ((m Map.! i) + 1) m) m neig
                      in flash s' m'


countFlashes :: Int -> Matrix Int -> Int
countFlashes 0 _ = 0
countFlashes n m = howMany (== 0) (Map.elems $ simulate m) + countFlashes (n - 1) (simulate m)


stepsToSync :: Matrix Int -> Int
stepsToSync mtx
    | howMany (== 0) (Map.elems mtx) == size * size = 0
    | otherwise = 1 + stepsToSync (simulate mtx)


main :: IO ()
main = do

    octopuses <- Map.map digitToInt . matrixFromList <$> fileLines octopusesPath
    
    print $ countFlashes 100 octopuses
    print $ stepsToSync octopuses
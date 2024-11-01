import Utils
import Data.List

containersPath = "Aoc2015/Day17/containers.in"


combinations :: Int -> [Int] -> [[Int]]
combinations n _ | n < 0 = []
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n - x) xs) ++ combinations n xs


main :: IO ()
main = do

    containers <- parseFile containersPath integral
    let comb = combinations 150 containers

    print $ length comb
    print $ howMany ((== minimum (map length comb)) . length) comb
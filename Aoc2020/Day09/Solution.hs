import Utils
import Data.List

numbersPath = "Aoc2020/Day09/numbers.in"


isSumOf2 :: [Int] -> Int -> Bool
isSumOf2 arr n = n `elem` [x + y | x <- arr, y <- arr]


firstWrong :: [Int] -> [Int] -> Int
firstWrong (x:xs) (y:ys)
    | not (isSumOf2 (x:xs) y) = y
    | otherwise = firstWrong (xs ++ [y]) ys


seqThatSum :: [Int] -> Int -> [Int]
seqThatSum (x:xs) n = aux [] (x:xs) n
    where
        aux acc _ m
            | m < 0 = seqThatSum xs n
            | m == 0 = reverse acc
        aux acc (y:ys) m = aux (y:acc) ys (m - y)


main :: IO ()
main = do
    
    numbers <- parseFile numbersPath integral
    
    let x = uncurry firstWrong $ splitAt 25 numbers
    print x

    let seq = seqThatSum numbers x
    print $ maximum seq + minimum seq
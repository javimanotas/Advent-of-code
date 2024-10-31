import Utils
import Data.List

bitsPath = "Aoc2021/Day03/bits.in"


mostCommonBit :: [Int] -> Int
mostCommonBit bits = sort bits !! (length bits `div` 2)

leastCommonBit :: [Int] -> Int
leastCommonBit = (1-) . mostCommonBit


getRating :: ([Int] -> Int) -> [[Int]] -> Int
getRating f = toDec . aux 0
    where
        aux _ [bits] = bits
        aux n xs = let target = f $ map (!!n) xs
                   in aux (n + 1) $ filter ((== target) . (!!n)) xs


toDec :: [Int] -> Int
toDec = aux . reverse
    where
        aux [] = 0
        aux (x:xs) = x + 2 * aux xs


main :: IO ()
main = do
    bitsMatrix <- map (map (read . (:[]))) <$> fileLines bitsPath
    
    let gammaRate = map mostCommonBit $ transpose bitsMatrix
    let epsilonRate = map leastCommonBit $ transpose bitsMatrix
    
    print $ toDec gammaRate * toDec epsilonRate

    print $ getRating mostCommonBit bitsMatrix * getRating leastCommonBit bitsMatrix
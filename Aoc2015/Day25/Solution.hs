import Utils
import Data.Char
import Data.List

positionPath = "Aoc2015/Day25/position.in"


step :: Int -> Int
step = (`mod` 33554393) . (* 252533)


tupleToInt :: (Int, Int) -> Int
tupleToInt (a, b) = iterate' step 20151125 !! n
    where
        diag = a + b - 1
        n = diag * (diag - 1) `div` 2 + b - 1


main :: IO ()
main = do

    [a, b] <- map read . splitWhen (not . isDigit) <$> readFile positionPath
    print $ tupleToInt (a, b)
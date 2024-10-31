import Utils
import Text.Parsec
import Data.List

boxesPath = "Aoc2015/Day02/boxes.in"


surfaceArea :: [Int] -> Int
surfaceArea xs@[l, w, h] = 2*l*w + 2*w*h + 2*h*l + product (take 2 xs)


ribbon :: [Int] -> Int
ribbon l = product l + 2 * sum (take 2 l)


main :: IO ()
main = do

    presents <- map sort <$> parseFile boxesPath (integral `sepBy` char 'x')

    print $ sum $ map surfaceArea presents
    print $ sum $ map ribbon presents
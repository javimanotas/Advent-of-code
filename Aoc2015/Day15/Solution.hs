import Utils
import Text.Parsec
import Data.Char
import Data.List

propertiesPath = "Aoc2015/Day15/properties.in"


score :: [[Int]] -> [Int] -> Int
score a b = product $ map (max 0 . sum . zipWith (*) b) $ transpose a


main :: IO ()
main = do

    properties <- map (map read . splitWhen ((&&) . (not . isDigit) <*> (/= '-'))) <$> fileLines propertiesPath
    
    let combinations = filter ((== 100) . sum) $ mapM (const [0..100]) properties
    let combinations' = filter ((== 500) . sum . zipWith (*) (map last properties)) combinations

    mapM_ (print . maximum . map (score (map init properties)))
        [combinations, combinations']
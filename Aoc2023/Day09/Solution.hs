import Utils
import Text.Parsec

historiesPath = "Aoc2023/Day09/histories.in"


prediction :: ([[Int]] -> Int) -> [Int] -> Int
prediction f = f . reduce
    where
        reduce l
            | all (== 0) l = [l]
            | otherwise = l : reduce (zipWith (-) (tail l) l)


main :: IO ()
main = do

    histories <- parseFile historiesPath $ integral `sepBy` spaces
    
    print $ sum $ map (prediction (sum . map last)) histories
    print $ sum $ map (prediction (foldr1 (-) . map head)) histories
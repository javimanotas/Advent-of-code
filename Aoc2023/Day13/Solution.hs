import Utils
import Data.List
import Data.Maybe

patternsPath = "Aoc2023/Day13/patterns.in"


mirror :: Int -> [String] -> Int
mirror errors pattern = fromMaybe ((*100) $ fromJust $ aux $ transpose pattern) (aux pattern)
    where
        aux pattern = (+1) <$> find (\i -> (== errors) $ length $ foldMap (reflectsAt i) pattern) [0 .. length (head pattern) - 2]

        reflectsAt i str = filter id $ zipWith (/=) right $ reverse left
            where
                right = map (str !!) [i + 1..length str - 1]
                left = map (str !!) [0..i]


main :: IO ()
main = do

    patterns <- splitWhen null <$> fileLines patternsPath
    
    print $ sum $ map (mirror 0) patterns
    print $ sum $ map (mirror 1) patterns
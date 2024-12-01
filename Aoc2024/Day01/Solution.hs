import Utils
import Text.Parsec
import Data.List

numbersPath = "Aoc2024/Day01/numbers.in"


main :: IO ()
main = do

    [groupA, groupB] <- map sort . transpose <$> parseFile numbersPath (integral `sepBy` spaces)

    print $ sum $ zipWith (\a -> abs . (a -)) groupA groupB
    print $ sum $ map (\n -> n * howMany (== n) groupB) groupA
import Utils
import Data.Char

racesPath = "Aoc2023/Day06/races.in"


numWays :: Int -> Int -> Int
numWays t dst = howMany wins [0..t]
    where
        wins hold = hold * (t - hold) > dst

main :: IO ()
main = do

    [times, distances] <- map (map read . splitWhen (not . isDigit)) <$> fileLines racesPath
    print $ product $ zipWith numWays times distances

    [time, distance] <- map (read . filter isDigit) <$> fileLines racesPath
    print $ numWays time distance
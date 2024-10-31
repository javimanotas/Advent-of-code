import Utils

positionsPath = "Aoc2021/Day07/positions.in"


minFuel :: (Int -> Int) -> [Int] -> Int
minFuel f positions = minimum $ map (\n -> sum $ map (f . abs . subtract n) positions) [0 .. maximum positions]


main :: IO ()
main = do

    positions <- map read . splitWhen (== ',') <$> readFile positionsPath

    print $ minFuel id positions
    print $ minFuel (\n -> (n * (n + 1)) `div` 2) positions
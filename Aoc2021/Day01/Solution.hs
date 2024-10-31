import Utils

numbersPath = "Aoc2021/Day01/numbers.in"


cntIncreasing :: [Int] -> Int
cntIncreasing = howMany (uncurry (<)) . (zip <*> tail)


main :: IO ()
main = do
    
    numbers <- map read <$> fileLines numbersPath
    
    print $ cntIncreasing numbers
    print $ cntIncreasing $ zipWith (+) (zipWith (+) numbers (tail numbers)) $ tail $ tail numbers
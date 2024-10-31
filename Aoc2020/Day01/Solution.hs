import Utils
import Data.List
import Control.Monad

numbersPath = "Aoc2020/Day01/numbers.in"


solve :: [Int] -> Int -> Maybe [Int]
solve numbers n = find ((== 2020) . sum) $ replicateM n numbers


main :: IO ()
main = do

    numbers <- map read <$> fileLines numbersPath
    mapM_ (print . fmap product . solve numbers) [2, 3]
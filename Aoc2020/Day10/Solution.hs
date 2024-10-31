import Utils
import Data.List

joltagesPath = "Aoc2020/Day10/joltages.in"


differences :: [Int] -> Int
differences joltages = howMany (== 3) gaps * howMany (== 1) gaps
    where
        gaps = zipWith (-) (tail joltages) joltages


arrangements :: [Int] -> Int
arrangements = evalMemo . aux
    where
        aux [_] = pure 1
        aux (x:xs) = memoize (x:xs) $ sum <$> mapM (\l -> aux $ l ++ notValids) (init $ tails valids)
            where
                (valids, notValids) = span (\n -> n - x <= 3) xs


main :: IO ()
main = do
    
    nums <- parseFile joltagesPath integral
    let joltages = 0 : sort nums ++ [maximum nums + 3]
    
    print $ differences joltages
    print $ arrangements joltages
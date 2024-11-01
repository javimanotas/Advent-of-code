import Utils
import Data.List
import Data.Ord
import Data.Function

packagesPath = "Aoc2015/Day24/packages.in"


groupsThatSum :: Int -> [Int] -> [[Int]]
groupsThatSum target xs = evalMemo $ aux target xs
    where
        aux 0 _ = pure [[]]
        aux _ [] = pure []
        aux n (x:xs) = memoize (n, x:xs) $ do
            a <- aux (n - x) xs
            b <- aux n xs
            pure $ map (x:) a ++ b


canBeSplitIn :: Int -> Int -> [Int] -> Bool
canBeSplitIn target n xs = aux (replicate n 0) xs
    where
        aux l _
            | any (> target) l = False

        aux [a, b] [] = a == b
        aux [a, b] (x:xs) = aux [x + a, b] xs || aux [a, b + x] xs

        aux [a, b, c] [] = a == b && b == c
        aux [a, b, c] (x:xs) = aux [x + a, b, c] xs || aux [a, b + x, c] xs || aux [a, b, c + x] xs


solve :: [Int] -> Int -> Int
solve packages n = minimum $ map product $ head $ dropWhile null $ map (filter (\g -> canBeSplitIn target (n - 1) (packages \\ g))) groups
    where
        target = sum packages `div` n
        groups = groupBy ((==) `on` length) $ sortBy (comparing length) $ groupsThatSum target packages


main :: IO ()
main = do

    packages <- parseFile packagesPath integral
    mapM_ (print . solve packages) [3, 4]
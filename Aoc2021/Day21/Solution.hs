import Utils
import Data.Char
import Data.List
import Control.Monad

startingTurnsPath = "Aoc2021/Day21/startingTurns.in"


solve :: Integer -> Integer -> Integer
solve posA posB = uncurry (*) $ aux posA 0 posB 0 0
    where
        aux posA scoreA posB scoreB n
            | score' >= 1000 = (scoreB, 3)
            | otherwise = (+3) <$> aux posB scoreB pos' score' (n + 3)
            where
                rolls = map ((+1) . (`mod` 100)) [n..n + 2]

                pos' = (posA + sum rolls) `mod` 10
                score' = scoreA + pos' + 1


winsAndLooses :: Integer -> Integer -> [Integer]
winsAndLooses posA posB = evalMemo $ aux posA 0 posB 0
    where
        aux _ _ _ scoreB
            | scoreB >= 21 = pure [0, 1]
        aux posA scoreA posB scoreB = memoize (posA, scoreA, posB, scoreB) $ do
                let perms = replicateM 3 [1, 2, 3]
                results <- mapM (\p -> let pos = (posA + sum p) `mod` 10
                                       in aux posB scoreB pos $ scoreA + pos + 1) perms
                pure $ reverse $ foldl (zipWith (+)) [0, 0] results


main :: IO ()
main = do

    [posA, posB] <- map (subtract 1 . read . (:[]) . last) <$> fileLines startingTurnsPath
    
    print $ solve posA posB
    print $ maximum $ winsAndLooses posA posB
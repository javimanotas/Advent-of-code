import Utils
import Data.List
import Data.Ord
import Data.Function
import qualified Data.Set as Set

rocksPath = "Aoc2023/Day14/rocks.in"


tiltUp :: [String] -> [String]
tiltUp = map (foldMap (sortBy (comparing Down)) . groupBy ((==) `on` (== '#')))


load :: [String] -> Int
load = sum . mapi (\i xs -> (i + 1) * howMany (== 'O') xs) . reverse . transpose


stepN :: Int -> [String] -> [String]
stepN n rocks = iterate step rocks !! (start + (n - start) `mod` cycle)
    where
        (cycle, start) = detectCycle (Set.singleton rocks) (step rocks)

        detectCycle set r
            | r `Set.member` set = (stepsTo r r, 1)
            | otherwise = (+1) <$> detectCycle (Set.insert r set) (step r)

        stepsTo x r
            | r' == x = 1
            | otherwise = 1 + stepsTo x r'
            where
                r' = step r

        step = rotate . tiltUp . rotate . tiltUp . rotate . tiltUp . rotate . tiltUp
        rotate = transpose . map reverse


main :: IO ()
main = do

    rocks <- transpose <$> fileLines rocksPath

    print $ load $ tiltUp rocks
    print $ load $ stepN 1000000000 rocks
import Utils
import Text.Parsec
import Data.List
import qualified Data.Map as Map

distancesPath = "Aoc2015/Day09/distances.in"


dst :: Map.Map [String] Int -> [String] -> Int
dst m l = sum $ map (m Map.!) $ zipWith (\a b -> sort [a, b]) l $ tail l


main :: IO ()
main = do

    distances <- Map.fromList <$> parseFile distancesPath (do
            a <- many1 letter
            string " to "
            b <- many1 letter
            string " = "
            (sort [a, b], ) <$> integral
        )

    let perms = permutations $ nub $ concat $ Map.keys distances
    
    print $ minimum $ map (dst distances) perms
    print $ maximum $ map (dst distances) perms
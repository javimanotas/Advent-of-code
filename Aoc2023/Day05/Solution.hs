import Utils
import Text.Parsec
import Data.List
import Control.Monad (foldM)

mapsPath = "Aoc2023/Day05/maps.in"
seedsPath = "Aoc2023/Day05/seeds.in"


applyMap :: Int -> [[Int]] -> Int
applyMap i maps = case find (\[_, a, b] -> i - a >= 0 && i - a < b) maps of
    Nothing -> i
    Just [x, a, _] -> i - a + x


applyMap' :: (Int, Int) -> [[Int]] -> [(Int, Int)]
applyMap' (a, b) [] = [(a, b)]
applyMap' (a, b) (l@[x, y, z]:xs)
    | a > y + z - 1 = applyMap' (a, b) xs
    | a >= y = if a + b - 1 > y + z - 1
                    then applyMap' (a, y + z - 1 - a + 1) (l:xs) ++ applyMap' (y + z - 1 + 1, a + b - 1 - (y + z - 1 + 1) + 1) xs
                    else [(a - y + x, b)]
    | otherwise = if a + b - 1 < y
                    then applyMap' (a, b) xs
                    else applyMap' (a, y - a + 1 - 1) xs ++ applyMap' (y, a + b - 1 - y + 1) (l:xs)


main :: IO ()
main = do

    [seeds] <- parseFile seedsPath $ integral `sepBy` spaces
    maps <- fileLines mapsPath >>= mapM ((`parseLines` (integral `sepBy` spaces)) . tail) . splitWhen null
    
    print $ minimum $ map (flip (foldl applyMap) maps) seeds
    
    let pairs = map (\[a, b] -> (a, b)) $ chunksOf 2 seeds
    print $ minimum $ map fst $ foldMap (flip (foldM applyMap') maps) pairs
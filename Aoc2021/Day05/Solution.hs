import Utils
import Data.Char
import qualified Data.Map as M

segmentsPath = "Aoc2021/Day05/segments.in"

type LineSegment = [Int] -- 4 values


pointsCovered :: LineSegment -> [(Int, Int)]
pointsCovered [x1, y1, x2, y2]
    | x1 == x2  = map (x1, ) $ range y1 y2
    | y1 == y2  = map (, y1) $ range x1 x2
    | abs (x1 - x2) == abs (y1 - y2) = zip (range x1 x2) (range y1 y2)
        where
            range a b = if a <= b then [a..b] else [a,a-1..b]



cntIntersections :: [(Int, Int)] -> Int
cntIntersections = M.size . M.filter (>= 2) . foldl (\g p -> M.insertWith (+) p 1 g) M.empty


main :: IO ()
main = do
    
    segments <- map (map read . splitWhen (not . isDigit)) <$> fileLines segmentsPath
    let horizontals = filter (\[x1, y1, x2, y2] -> x1 == x2 || y1 == y2) segments

    mapM_ (print . cntIntersections . concatMap pointsCovered) [horizontals, segments]
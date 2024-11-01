import Utils
import Text.Parsec

reindeersPath = "Aoc2015/Day14/reindeers.in"


data Reindeer = Reindeer { speeed :: Int, moveTime :: Int, restTime :: Int }


dstAt :: Int -> Reindeer -> Int
dstAt t r
    | t <= moveTime r = t * speeed r
    | t <= moveTime r + restTime r = moveTime r * speeed r
    | otherwise = t `div` time * moveTime r * speeed r + dstAt (t `mod` time) r
        where
            time = moveTime r + restTime r


points :: Int -> [Reindeer] -> [Int]
points 0 r = map (const 0) r
points n r = zipWith (+) (map (\d -> if d == maximum dsts then 1 else 0) dsts) $ points (n - 1) r
    where
        dsts = map (dstAt n) r


main :: IO ()
main = do

    reindeers <- parseFile reindeersPath (do
            many1 letter
            string " can fly "
            s <- integral
            string " km/s for "
            mT <- integral
            string " seconds, but then must rest for "
            Reindeer s mT <$> integral
        )
    
    print $ maximum $ map (dstAt 2503) reindeers
    print $ maximum $ points 2503 reindeers
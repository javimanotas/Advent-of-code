import Utils
import Data.List
import Data.Ord

directionsPath = "Aoc2020/Day05/directions.in"


sitId :: String -> Int
sitId directions = 8 * aux 0 127 row + aux 0 7 col
    where
        (row, col) = splitAt 7 directions
        
        aux a b [] = b
        aux a b (x:xs) = let mid = (a + b) `div` 2
                         in case x of
                            'F' -> aux a mid xs
                            'B' -> aux mid b xs
                            'L' -> aux a mid xs
                            'R' -> aux mid b xs


main :: IO ()
main = do

    directions <- fileLines directionsPath

    let seats = sortBy (comparing Down) $ map sitId directions 
    print $ head seats
    print $ (`div` 2) . uncurry (+) <$> find (\(a, b) -> a - 1 /= b) (zip seats (tail seats))
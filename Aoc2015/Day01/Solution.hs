import Utils
import Data.List

directionsPath = "Aoc2015/Day01/directions.in"


main :: IO ()
main = do

    directions <- readFile directionsPath
    let height dirs = howMany (== '(') dirs - howMany (== ')') dirs
    
    print $ height directions
    print $ findIndex ((== -1) . height) $ inits directions
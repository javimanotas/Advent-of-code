{-# LANGUAGE LambdaCase #-}
import Utils
import Data.List

directionsPath = "Aoc2015/Day03/directions.in"


move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) = \case
    '^' -> (x, y + 1)
    'v' -> (x, y - 1)
    '<' -> (x - 1, y)
    '>' -> (x + 1, y)


main :: IO ()
main = do

    directions <- readFile directionsPath
    
    print $ length $ nub $ scanl move (0, 0) directions

    let santa = scanl move (0, 0) $ filteri (\i _ -> even i) directions
    let robo = scanl move (0, 0) $ filteri (\i _ -> odd i) directions
    print $ length $ nub $ santa ++ robo
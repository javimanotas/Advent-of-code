{-# LANGUAGE LambdaCase #-}
import Utils
import Data.List
import Data.Either

numbersPath = "Aoc2021/Day04/numbers.in"
gridsPath = "Aoc2021/Day04/grids.in"


type Bingo = [[Either Int Int]] -- Left for unmarked values and right for the marked ones


mark :: Int -> Bingo -> Bingo
mark i = map (map (\case Left a
                            | a == i -> Right a
                         other -> other))


wins :: Bingo -> Bool
wins = any isRight . lines
    where
        lines = (++) . map sequence <*> map sequence . transpose


score :: [Bingo] -> [Int] -> Int
score grids (x:xs) = let newGrids = map (mark x) grids
                     in case find wins newGrids of
                            Nothing -> score newGrids xs
                            Just grid -> x * sum (map (sum . lefts) grid)


lastToWin :: [Bingo] -> [Int] -> Int
lastToWin grids (x:xs) = let newGrids = map (mark x) grids
                         in case filter (not . wins) newGrids of
                                [grid] -> score [grid] xs
                                other -> lastToWin other xs


main :: IO ()
main = do
    
    numbers <- map read . splitWhen (== ',') <$> readFile numbersPath
    grids <- map (map (map (Left . read) . splitWhen (== ' '))) . splitWhen null <$> fileLines gridsPath

    print $ score grids numbers
    print $ lastToWin grids numbers
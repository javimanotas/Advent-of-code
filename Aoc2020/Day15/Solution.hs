import Utils
import Text.Parsec
import Data.Maybe
import qualified Data.Map as Map

numbersPath = "Aoc2020/Day15/numbers.in"


nth :: Map.Map Int Int -> Int -> Int -> Int -> Int
nth map turn end last 
    | turn == end = last
    | otherwise = case Map.lookup last map of
                    Nothing -> nth (Map.insert last turn map) (turn + 1) end 0
                    Just x -> nth (Map.insert last turn map) (turn + 1) end (turn - x)


main :: IO ()
main = do

    [numbers] <- parseFile numbersPath $ integral `sepBy` char ','
    let map = Map.fromList $ zip (init numbers) [1..] 

    print $ nth map (length numbers) 2020 $ last numbers
    print $ nth map (length numbers) 30000000 $ last numbers
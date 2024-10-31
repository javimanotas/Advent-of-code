import Utils
import Data.Maybe
import Data.List
import qualified Data.Map as Map

seatsPath = "Aoc2020/Day11/seats.in"


offsets (a, b) = [ (a + 1, b), (a, b + 1), (a - 1, b), (a, b - 1)
                 , (a + 1, b + 1), (a + 1, b - 1), (a - 1, b + 1), (a - 1, b - 1)]


offsets' (a, b) = map (`map` [1..50]) [ \n -> (a + n, b), \n -> (a, b + n), \n -> (a - n, b), \n -> (a, b - n)
                                            , \n -> (a + n, b + n), \n -> (a + n, b - n), \n -> (a - n, b + n), \n -> (a - n, b - n) ]


simulate :: Int -> Matrix [(Int, Int)] -> Matrix Char -> Int
simulate occupied neighbors = howMany (== '#') . Map.elems . step
    where
        step m = if m == m' then m else step m'
            where
                m' = Map.mapWithKey (\k b -> let neig = map (m Map.! ) $ neighbors Map.! k
                                                 occu = howMany (== '#') neig
                                                 free = howMany (== 'L') neig
                                             in if b == 'L'
                                                then if occu == 0 then '#' else 'L'
                                                else if occu >= occupied then 'L' else '#') m


main :: IO ()
main = do

    seats <- Map.filter (/= '.') . matrixFromList <$> fileLines seatsPath
    
    let neig = Map.fromList
             $ map (\key -> (key, filter (`Map.member` seats) $ offsets key))
             $ Map.keys seats
    print $ simulate 4 neig seats

    let neig' = Map.fromList
              $ map (\key -> (key, mapMaybe (find (`Map.member` seats)) $ offsets' key))
              $ Map.keys seats
    print $ simulate 5 neig' seats
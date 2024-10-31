import Utils
import Data.List
import Data.Ord
import Data.Maybe
import Data.Char

busesPath = "Aoc2020/Day13/buses.in"


solve :: [Int] -> [Int] -> Int
solve modulos remainders = (`mod` n) $ sum $ zipWith3 (\a b c -> a * b * c) remainders ni mi
    where
        n = product modulos
        ni = map (div n) modulos
        mi = zipWith (\a b -> fromJust $ find (\x -> (a * x) `mod` b == 1) [1..b]) ni modulos 

main :: IO ()
main = do
    [a, b] <- fileLines busesPath

    let start = read a
    let (buses, times) = unzip [(read b, i)| (b, i) <- zip (splitWhen (== ',') b) [0..], b /= "x"]
    
    print $ uncurry (*)
          $ minimumBy (comparing snd)
          $ map (\b -> (b, b - start `mod` b)) buses

    print $ solve buses $ map negate times
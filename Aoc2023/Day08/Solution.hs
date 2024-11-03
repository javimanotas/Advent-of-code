import Utils
import Text.Parsec
import qualified Data.Map as Map

directionsPath = "Aoc2023/Day08/directions.in"
mapPath = "Aoc2023/Day08/map.in"


stepsTo :: (String -> Bool) 
        -> Map.Map String (String, String)
        -> [(String, String) -> String]
        -> String
        -> Int
stepsTo f m (x:xs) a
    | f a = 0
    | otherwise = 1 + stepsTo f m xs (x $ m Map.! a)


main :: IO ()
main = do

    [directions] <- parseFile directionsPath (many ((char 'L' >> pure fst) <|> (char 'R' >> pure snd)))
    entries <- Map.fromList <$> parseFile mapPath (do
        k <- many1 letter
        string " = ("
        a <- many1 letter
        string ", "
        b <- many1 letter
        pure (k, (a, b)))
    
    print $ stepsTo (== "ZZZ") entries (cycle directions) "AAA"
    print $ foldl1 lcm $ [ stepsTo ((== 'Z') . last) entries (cycle directions) k
                         | k <- Map.keys entries
                         , last k == 'A']
import Utils
import Text.Parsec
import Data.List
import qualified Data.Map as Map

cardsPath = "Aoc2023/Day04/cards.in"


copies :: [(Int, Int)] -> Map.Map Int Int
copies l = foldl (\m (i, n) -> foldl (\m' k -> Map.insertWith (+) k (m Map.! i) m') m [i+1..i+n]) (Map.map (const 1) $ Map.fromList l) l


main :: IO ()
main = do

    matching <- parseFile cardsPath (do
            string "Card" >> spaces
            n <- integral
            char ':' >> spaces
            let nums = integral `sepEndBy1` spaces
            [a, b] <- nums `sepBy` (char '|' >> spaces)
            pure (n, length (a `intersect` b))
        )
    
    print $ sum $ map (\(_, n) -> (2 ^ n) `div` 2) matching
    print $ sum $ Map.elems $ copies matching
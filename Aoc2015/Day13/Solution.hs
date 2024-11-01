import Utils
import Text.Parsec
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Tuple (swap)

happinessPath = "Aoc2015/Day13/happiness.in"


total :: Map.Map (String, String) Int -> [String] -> Int
total m l = aux pairs + aux (map swap pairs)
    where
        pairs = zip l $ tail l
        aux = sum . map (fromMaybe 0 . (`Map.lookup` m))


main :: IO ()
main = do

    happiness <- Map.fromList <$> parseFile happinessPath (do
            a <- many1 letter
            string " would "
            let positive = string "gain " >> integral
            let negative = negate <$> (string "lose " >> integral)
            n <- positive <|> negative
            string " happiness units by sitting next to "
            b <- many1 letter
            pure ((a, b), n)
        )
    let people = nub $ map fst $ Map.keys happiness

    mapM_ (print . maximum . map (total happiness . \p -> p ++ [head p]) . permutations)
        [people, "me":people]
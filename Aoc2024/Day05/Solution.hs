import Utils
import Data.List
import Text.Parsec
import Data.Maybe
import qualified Data.Map as Map

orderingPath = "Aoc2024/Day05/ordering.in"
seqsPath = "Aoc2024/Day05/seqs.in"


pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = [(x, y) | y <- xs] ++ pairs xs


sorted :: Graph Int -> [Int] -> Bool
sorted m l = all (\(a, b) -> a `notElem` concat (maybeToList (Map.lookup b m))) $ pairs l


swapTillCorrect :: Graph Int -> [Int] -> [Int]
swapTillCorrect m l = case find (\(a, b) -> ((a `elem`) <$> Map.lookup b m) == Just True) $ pairs l of
   Nothing -> l
   Just (a, b) -> swapTillCorrect m $ map (\x -> fromMaybe x $ lookup x [(a, b), (b, a)]) l


main :: IO ()
main = do

   ordering <- graphFromEdges <$> parseFile orderingPath ((,) <$> integral <* char '|' <*> integral)
   seqs <- parseFile seqsPath $ integral `sepBy1` char ','
   
   mapM_ (print . sum . map (\s -> s !! (length s `div` 2))) [
         filter (sorted ordering) seqs,
         zipWith (\a b -> if a == b then [0] else b) seqs $ map (swapTillCorrect ordering) seqs
      ]
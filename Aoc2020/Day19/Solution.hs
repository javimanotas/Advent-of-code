import Utils
import Text.Parsec
import Data.Either
import Data.Maybe
import Control.Monad
import qualified Data.Map as Map

rulesPath = "Aoc2020/Day19/rules.in"
stringsPath = "Aoc2020/Day19/strings.in"


data Rule = Seq [[Int]] | Match Char


rule :: StringParser (Int, Rule)
rule = do
    key <- integral
    string ": "
    (key, ) <$> (match <|> seq)
    where
        match = between (char '"') (char '"') (Match <$> anyChar)
        seq = Seq . map (map read . splitWhen (== ' ')) . splitWhen (== '|') <$> many anyChar


match :: Map.Map Int Rule -> Int -> String -> Bool
match rules i str = "" `elem` match str (rules Map.! i)
    where
        match [] (Match c) = []
        match (x:xs) (Match c)
            | x == c = [xs]
            | otherwise = []
        match xs (Seq q) = q >>= foldM (\a b -> match a $ rules Map.! b) xs



main :: IO ()
main = do

    rules <- Map.fromList <$> parseFile rulesPath rule
    strings <- fileLines stringsPath
    
    print $ howMany (match rules 0) strings

    let rules' = Map.insert 11 (Seq [[42, 31], [42, 11, 31]]) $ Map.insert 8 (Seq [[42], [42, 8]]) rules    
    print $ howMany (match rules' 0) strings
import Utils
import Data.List
import Text.Parsec
import Data.Maybe
import qualified Data.Map as Map

segmentsPath = "Aoc2021/Day08/segments.in"


numbers = Map.fromList [ ("abcefg", 0)
                       , ("cf", 1)
                       , ("acdeg", 2)
                       , ("acdfg", 3)
                       , ("bcdf", 4)
                       , ("abdfg", 5)
                       , ("abdefg", 6)
                       , ("acf", 7)
                       , ("abcdefg", 8)
                       , ("abcdfg", 9)]


applyPermutation :: String -> String -> Maybe Int
applyPermutation a b = Map.lookup (sort $ map (b !!) indeces) numbers
    where
        indeces = map (\c -> fromEnum c - fromEnum 'a') $ sort a


getNum :: [String] -> [String] -> Int
getNum a = read . foldMap show . fromJust . mapM (`applyPermutation` perm)
    where
        (Just perm) = find (\p -> (/= Nothing) $ mapM (`applyPermutation` p) a) $ permutations "abcdefg"


main :: IO ()
main = do
    
    outputs <- map (splitWhen (== ' ') . tail . dropWhile (/= '|')) <$> fileLines segmentsPath
    print $ howMany ((`elem` [2, 4, 3, 7]) . length) $ concat outputs

    entries <- parseFile segmentsPath (do
            let seq = splitWhen (== ' ') <$> many1 (letter <|> char ' ')
            a <- seq
            char '|'
            b <- seq
            pure (a, b)
        )
    print $ sum $ map (uncurry getNum) entries
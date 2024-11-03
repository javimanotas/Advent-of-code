import Utils
import Text.Parsec
import Data.Function
import Data.Ord
import Data.List

handsPath = "Aoc2023/Day07/hands.in"


data HandType = HighCard
              | OnePair
              | TwoPair
              | ThreeOfAKind
              | FullHouse
              | FourOfAKind
              | FiveOFAKind
              deriving (Show, Eq, Ord)


getType :: String -> HandType
getType l = case sortBy (comparing Down) $ map length $ group $ sort l of
    (5:_) -> FiveOFAKind
    (4:_) -> FourOfAKind
    (3:2:_) -> FullHouse
    (3:_) -> ThreeOfAKind
    (2:2:_) -> TwoPair
    (2:_) -> OnePair
    _ -> HighCard


winnings :: [((HandType, [Maybe Int]), Int)] -> Int
winnings = sum . mapi (\i (_, n) -> (i + 1) * n) . sortBy (comparing fst)


jokers :: String -> [String]
jokers [] = [[]]
jokers ('J':xs) = foldMap (\s -> map (s:) $ jokers xs) "23456789TQKA"
jokers (x:xs) = map (x:) $ jokers xs


main :: IO ()
main = do

    hands <- parseFile handsPath $ do
        hand <- many1 (letter <|> digit)
        spaces
        (hand, ) <$> integral

    print $ winnings $ map (\(a, b) -> ((getType a , map (`elemIndex` "23456789TJQKA") a), b)) hands
    print $ winnings $ map (\(a, b) -> ((maximum $ map getType $ jokers a , map (`elemIndex` "J23456789TQKA") a), b)) hands
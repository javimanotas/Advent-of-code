import Utils
import Text.Parsec
import Data.List
import Data.Maybe
import Data.Ord

rangesPath = "Aoc2020/Day16/ranges.in"
ticketPath = "Aoc2020/Day16/ticket.in"
ticketsPath = "Aoc2020/Day16/tickets.in"


ticketParser :: StringParser [Int]
ticketParser = integral `sepBy` char ','


conditionParser :: StringParser (String, Int -> Bool)
conditionParser = do
    label <- many $ noneOf ":"
    string ": "
    let range = (,) <$> integral <*> (char '-' >> integral)
    (a, b) <- range
    string " or "
    (c, d) <- range
    pure (label, \n -> (n >= a && n <= b) || (n >= c && n <= d))


findPositions :: [[String]] -> [String]
findPositions = map snd . sortBy (comparing fst) . purge . zip [0..]
    where
        purge [] = []
        purge l = case find ((== 1). length . snd) l of
                    Just item@(i, [str]) -> (i, str) : purge (map (fmap (filter (/=str))) $ delete item l)


main :: IO ()
main = do
    
    ranges <- parseFile rangesPath conditionParser
    [ticket] <- parseFile ticketPath ticketParser
    tickets <- parseFile ticketsPath ticketParser

    let conditions = map snd ranges
    let invalid num = (not . any ($ num)) conditions
    
    print $ sum $ concatMap (filter invalid) tickets

    let validTickets = filter (not . any invalid) tickets
    
    let candidates = map (\row -> fst <$> filter (\(_, cond) -> all cond row) ranges)
                   $ transpose validTickets
    let positions = findPositions candidates
    print $ product [ t | (t, str) <- zip ticket positions, "departure" `isPrefixOf` str]
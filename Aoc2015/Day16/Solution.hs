import Utils
import Text.Parsec
import qualified Data.Map as Map

auntsPath = "Aoc2015/Day16/aunts.in"
conditionsPath = "Aoc2015/Day16/conditions.in"


keyValue :: StringParser (String, Integer)
keyValue = do
    a <- many1 letter
    string ": "
    (a, ) <$> integral


main :: IO ()
main = do

    aunts <- parseFile auntsPath (do
            string "Sue "
            idx <- integral
            string ": "
            (idx, ) . Map.fromList <$> keyValue `sepBy` string ", "
        )
    conditions <- Map.fromList <$> parseFile conditionsPath keyValue
    print $ [i | (i, x) <- aunts, all (\k -> (conditions Map.! k) == (x Map.! k)) $ Map.keys x]

    let conditions' = conditions
    print $ [i | (i, x) <- aunts, all (\k -> let n = conditions' Map.! k
                                                 m = x Map.! k
                                             in case k of
                                                "cats" -> m > n
                                                "trees" -> m > n
                                                "pomeranians" -> m < n
                                                "goldfish" -> m < n
                                                _ -> m == n) $ Map.keys x]
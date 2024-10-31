import Utils
import Text.Parsec

rulesPath = "Aoc2020/Day02/rules.in"


data Rule = Rule { low :: Int
                 , high :: Int
                 , target :: Char
                 , str :: String }


isValid :: Rule -> Bool
isValid rule = cnt >= low rule && cnt <= high rule
    where
        cnt = howMany (== target rule) (str rule)


isValid' :: Rule -> Bool
isValid' rule = lowContains /= highContains
    where
        lowContains = (str rule !! (low rule - 1)) == target rule
        highContains = (str rule !! (high rule - 1)) == target rule


main :: IO ()
main = do

    rules <- parseFile rulesPath (do
            l <- integral <* char '-'
            h <- integral <* spaces
            t <- anyChar <* string ": "
            Rule l h t <$> many1 letter
        )
    
    print $ howMany isValid rules
    print $ howMany isValid' rules
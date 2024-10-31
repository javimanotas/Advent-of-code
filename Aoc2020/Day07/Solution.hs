import Utils
import Text.Parsec
import qualified Data.Set as Set
import qualified Data.Map as Map

rulesPath = "Aoc2020/Day07/rules.in"


bagColor :: StringParser String
bagColor = do
    color1 <- many1 letter
    space
    color2 <- many1 letter
    pure (color1 ++ " " ++ color2)


containedBag :: StringParser (Int, String)
containedBag = do
    num <- read <$> many1 digit
    space
    color <- bagColor
    string " bag" <* optional (char 's')
    return (num, color)


parentColors :: [(String, [(Int, String)])] -> String -> Set.Set String
parentColors l str = Set.fromList valids `Set.union` foldMap (parentColors l) valids
    where
        valids = [name | (name, xs) <- l, any ((== str) . snd) xs]


contained :: Map.Map String [(Int, String)] -> String -> Int
contained m k = sum $ zipWith (\n str -> n + n * contained m str) nums names
    where
        (nums, names) = unzip $ m Map.! k


main :: IO ()
main = do

    rules <- parseFile rulesPath (do
            color <- bagColor
            string " bags contain "
            containedBags <- (string "no other bags" >> return []) <|> containedBag `sepBy` string ", "
            return (color, containedBags)
        )
    
    print $ Set.size $ parentColors rules "shiny gold"
    print $ contained (Map.fromList rules) "shiny gold"
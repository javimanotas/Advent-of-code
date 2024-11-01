import Utils
import Text.Parsec
import Data.Char

jsonPath = "Aoc2015/Day12/json.in"


data Nested = Num Int | Word String | Object [Nested] | Array [Nested] deriving Eq


nested :: StringParser Nested
nested = object <|> array <|> num <|> word
    where
        object = Object <$> between (char '{') (char '}') (pair `sepBy` char ',')
        pair = char '"' >> many1 letter >> string "\":" >> nested
        array = Array <$> between (char '[') (char ']') (nested `sepBy` char ',')
        num = Num <$> integral
        word = Word <$> between (char '"') (char '"') (many1 letter)


total :: Nested -> Int
total (Num n) = n
total (Word w) = 0
total (Array arr) = sum $ map total arr
total (Object obj)
    | Word "red" `elem` obj = 0
    | otherwise = sum $ map total obj


main :: IO ()
main = do

    nums <- map read . splitWhen ((&&) . (not . isDigit) <*> (/= '-')) <$> readFile jsonPath
    print $ sum nums

    [json] <- parseFile jsonPath nested
    print $ total json
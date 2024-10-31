import Utils
import Text.Parsec
import Control.Monad

stringsPath = "Aoc2015/Day08/strings.in"


scape :: StringParser ()
scape = void letter <|> try slash <|> try quotes <|> try hex
    where
        slash = void $ string "\\\\"
        quotes = void $ string "\\\""
        hex = void $ string "\\x" >> count 2 (oneOf "0123456789abcdef")


main :: IO ()
main = do

    strings <- fileLines stringsPath

    numCharacters <- parseFile stringsPath (length <$> between (char '"') (char '"') (many scape))
    print $ length (concat strings) - sum numCharacters

    print $ sum (map (length . show) strings) - length (concat strings)
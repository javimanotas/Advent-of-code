import Utils
import Text.Parsec
import qualified Data.Map as Map

gamesPath = "Aoc2023/Day02/games.in"


valid :: String -> Int -> Bool
valid "red" n = n <= 12
valid "green" n = n <= 13
valid "blue" n = n <= 14


main :: IO ()
main = do

    games <- parseFile gamesPath (do
            string "Game "
            idx <- integral
            string ": "
            let cube = flip (,) <$> integral <*> (spaces >> many1 letter)
            (idx, ) <$> cube `sepBy1` (string ", " <|> string "; ")
        )

    print $ sum [id | (id, cubes) <- games, all (uncurry valid) cubes]
    print $ sum $ map (product . Map.elems . foldl (flip $ uncurry $ Map.insertWith max) Map.empty . snd) games
import Utils
import Text.Parsec

sequencePath = "Aoc2023/Day15/sequence.in"


data Lens = Equal String Int | Dash String


getLabel :: Lens -> String
getLabel (Equal lbl _) = lbl
getLabel (Dash lbl) = lbl 


hash :: String -> Int
hash = foldl (\a b -> ((a + fromEnum b) * 17) `mod` 256) 0


insert :: [[(String, Int)]] -> Lens -> [[(String, Int)]]
insert l lens = mapi (\i xs -> if hash (getLabel lens) /= i then xs else aux lens xs) l
    where
        aux (Dash lbl) xs = filter ((/= lbl) . fst) xs
        aux (Equal lbl i) xs
            | any ((==lbl). fst) xs = map (\(str, j) -> if str == lbl then (str, i) else (str, j)) xs
            | otherwise = xs ++ [(lbl, i)]


power :: [[(String, Int)]] -> Int
power = sum . mapi (\i l -> (i + 1) * sum (mapi (\j (_, n) -> (j + 1) * n) l))


main :: IO ()
main = do

    [seq] <- parseFile sequencePath $ many (noneOf ",") `sepBy` char ','
    print $ sum $ map hash seq

    [lenses] <- parseFile sequencePath $ do
        let lens = do
                lbl <- many1 letter
                (char '-' >> pure (Dash lbl)) <|> (char '=' >> Equal lbl <$> integral)
        lens `sepBy` char ','

    print $ power $ foldl insert (replicate 256 []) lenses
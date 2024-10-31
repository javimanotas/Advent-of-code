import Utils
import Text.Parsec
import Data.Char
import qualified Data.Map as Map

bitsPath = "Aoc2020/Day14/bits.in"


data Entry =  Mask String | Mem Int String deriving Show


toBin :: Int -> String
toBin = reverse . aux 36
    where
        aux 0 _ = []
        aux n m = show (m `mod` 2) ++ aux (n - 1) (m `div` 2)


fromBin :: String -> Int
fromBin = aux . map digitToInt . reverse
    where
        aux [] = 0
        aux (x:xs) = x + 2 * aux xs


solve :: (Map.Map Int String, String) -> Entry -> (Map.Map Int String, String)
solve (m, e) (Mask e') = (m, e')
solve (m, e) (Mem i str) = (Map.insert i (zipWith mask e str) m, e)
    where
        mask 'X' x = x
        mask x _ = x


solve' :: (Map.Map Int String, String) -> Entry -> (Map.Map Int String, String)
solve' (m, e) (Mask e') = (m, e')
solve' (m, e) (Mem i str) = (foldl (\m x -> Map.insert (fromBin x) str m) m l, e)
    where
        l = aux e $ toBin i
        
        aux [] [] = [[]]
        aux ('0':xs) (y:ys) = map (y:) $ aux xs ys
        aux ('1':xs) (_:ys) = map ('1':) $ aux xs ys
        aux ('X':xs) (_:ys) = map ('0':) (aux xs ys) ++ map ('1':) (aux xs ys)

main :: IO ()
main = do

    (Mask x:xs) <- parseFile bitsPath (do
            let mask = string "mask = " >> Mask <$> many anyChar
            let mem = do
                    string "mem["
                    i <- integral
                    string "] = "
                    str <- toBin <$> integral
                    pure $ Mem i str

            try mask <|> mem
        )

    print $ sum $ map fromBin $ Map.elems $ fst $ foldl solve (Map.empty, x) xs
    print $ sum $ map fromBin $ Map.elems $ fst $ foldl solve' (Map.empty, x) xs
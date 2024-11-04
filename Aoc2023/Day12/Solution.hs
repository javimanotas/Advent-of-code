import Utils
import Data.List
import Text.Parsec

springsPath = "Aoc2023/Day12/springs.in"


arrangements :: String -> [Int] -> Int
arrangements str = evalMemo . aux '.' str
    where
        aux _ [] l
            | null l || l == [0] = pure 1
            | otherwise = pure 0
        aux _ ('#':xs) l
            | null l || head l == 0 = pure 0
        aux c ('#':xs) (g:gs) = memoize (c, '#':xs, g:gs) $ aux '#' xs (g - 1 : gs)
        aux '#' ('.':xs) (g:gs)
            | g /= 0 = pure 0
            | otherwise = memoize ('#', '.':xs, g:gs) $ aux '.' xs gs
        aux c ('.':xs) gs = memoize (c, '.':xs, gs) $ aux '.' xs gs
        aux c ('?':xs) gs = memoize (c, '?':xs, gs) $ (+) <$> aux c ('#':xs) gs <*> aux c ('.':xs) gs


main :: IO ()
main = do

    springs <- parseFile springsPath $ do
        spring <- many $ noneOf " "
        spaces
        (spring, ) <$> integral `sepBy` char ','
    
    print $ sum $ map (uncurry arrangements) springs

    print $ sum $ map (\(a, b) -> arrangements (intercalate "?" $ replicate 5 a) (concat $ replicate 5 b)) springs
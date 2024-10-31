import Utils
import Text.Parsec

instructionsPath = "Aoc2020/Day12/instructions.in"


transform :: [Int] -> (Char, Int) -> [Int]
transform [a, b, c, d] (x, n) = case x of
    'N' -> [a, b + n, c, d]
    'S' -> [a, b - n, c, d]
    'E' -> [a + n, b, c, d]
    'W' -> [a - n, b, c, d]
    'R' -> transform [a, b, c, d] ('L', 360 - n)
    'L' -> case n of
            90 -> [a, b, -d, c]
            180 -> [a, b, -c, -d]
            270 -> [a, b, d, -c]
    'F' -> [a + c * n, b + d * n, c, d]


transform' :: [Int] -> (Char, Int) -> [Int]
transform' [a, b, c, d] (x, n) = case x of
    'N' -> [a, b, c, d + n]
    'S' -> [a, b, c, d - n]
    'E' -> [a, b, c + n, d]
    'W' -> [a, b, c - n, d]
    'R' -> transform' [a, b, c, d] ('L', 360 - n)
    'L' -> case n of
            90 -> [a, b, -(d - b) + a, (c - a) + b]
            180 -> [a, b, a - (c - a), b - (d - b)]
            270 -> [a, b, (d - b) + a, -(c - a) + b]
    'F' -> let diffx = c - a
               diffy = d - b
            in [a + n * diffx, b + n * diffy, c + n * diffx, d + n * diffy]


main :: IO ()
main = do

    instructions <- parseFile instructionsPath $ (,) <$> anyChar <*> integral
    
    print $ sum $ map abs $ take 2 $ foldl transform [0, 0, 1, 0] instructions
    print $ sum $ map abs $ take 2 $ foldl transform' [0, 0, 10, 1] instructions

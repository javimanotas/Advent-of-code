import Utils
import Text.Parsec

equationsPath = "Aoc2024/Day07/equations.in"


valid :: [Int -> Int -> Int] -> [Int] -> Bool
valid funs (target:hd:tl) = aux hd tl
   where
      aux acc [] = acc == target
      aux acc (x:xs) = any (\f -> aux (f acc x) xs) funs


main :: IO ()
main = do
   
   entries <- parseFile equationsPath $ integral `sepBy` (optional (char ':') >> spaces)
   
   mapM_ (print . sum . map head . flip filter entries . valid) [
         [(+), (*)],
         [(+), (*), \a b -> read (show a ++ show b)]
      ]
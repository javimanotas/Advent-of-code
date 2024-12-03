import Utils
import Text.Parsec
import Data.Maybe

filePath = "Aoc2024/Day03/file.in"


mul :: StringParser Int
mul = string "mul(" >> (*) <$> integral <* char ',' <*> integral <* char ')'


solve :: Int -> [Either Int Int] -> Int
solve _ [] = 0
solve _ ((Left a):xs) = solve a xs
solve k ((Right n):xs) = k * n + solve k xs


main :: IO ()
main = do
   
   nums <- concatMap catMaybes <$> parseFile filePath (many1 $ choice [
         Just <$> try mul,
         anyChar >> pure Nothing
      ])
   print $ sum nums

   line <- concatMap catMaybes <$> parseFile filePath (many1 $ choice [
         try (string "don't") >> pure (Just (Left 0)),
         try (string "do") >> pure (Just (Left 1)),
         Just . Right <$> try mul,
         anyChar >> pure Nothing
      ])
   
   print $ solve 1 line
import Utils
import Data.List --https://hackage.haskell.org/package/base/docs/Data-List.html 
import Text.Parsec --https://hackage.haskell.org/package/parsec/docs/Text-Parsec.html 

machinesPath = "Aoc2024/Day13/machines.in"


data Machine = Machine { buttonA :: (Int, Int)
                       , buttonB :: (Int, Int)
                       , prize   :: (Int, Int)
                       } deriving Show


machine :: StringParser Machine
machine = do
   [a, b, p] <- line `sepBy` char '.'
   return $ Machine a b p
   where
      discard = many $ noneOf ['0'..'9']
      line = do
         discard
         a <- fromIntegral <$> integral
         discard
         (a, ) . fromIntegral <$> integral


solve :: Machine -> Int
solve m
   | map (sum . zipWith (*) [a, b] . init) [l1, l2] /= map last [l1, l2] = 0
   | a < 0 || b < 0 = 0
   | otherwise = 3 * a + b
   where

      (l1, l2) = unzip [buttonA m, buttonB m, prize m]
      
      l1' = map (*head l2) l1
      l2' = map (* (-head l1)) l2
      
      [_, x, y] = zipWith (+) l1' l2'

      b = y `div` x
      a = ((l1' !! 2) - ((l1' !! 1) * b)) `div` head l1'


main :: IO ()
main = do

   entries <- map (intercalate ".") . splitWhen null <$> fileLines machinesPath
   machines <- parseLines entries machine
   
   print $ sum $ map solve machines

   let incPrize m = m { prize = prize m /+ (10000000000000, 10000000000000)}
   print $ sum $ map (solve . incPrize) machines
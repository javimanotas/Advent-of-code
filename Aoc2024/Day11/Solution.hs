import Utils
import Text.Parsec --https://hackage.haskell.org/package/parsec/docs/Text-Parsec.html 

stonesPath = "Aoc2024/Day11/stones.in"


blinkNTimes :: Int -> [Int] -> Int
blinkNTimes n = evalMemo . fmap sum . mapM (aux n)
   where
      aux 0 _ = return 1
      aux n 0 = memoize (n, 0) $ aux (n - 1) 1
      aux n x
         | even $ length $ show x =
            let str = show x
                len = length str
                (a, b) = splitAt (len `div` 2) str
            in memoize (n, x) $ (+) <$> aux (n - 1) (read a) <*> aux (n - 1) (read b)
         | otherwise = memoize (n, x) $ aux (n - 1) (x * 2024)


main :: IO ()
main = do

   [stones] <- parseFile stonesPath $ integral `sepBy` spaces
   mapM_ (print . (`blinkNTimes` stones)) [25, 75]
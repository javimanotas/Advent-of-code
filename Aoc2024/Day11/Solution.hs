import Utils
import Data.List --https://hackage.haskell.org/package/base/docs/Data-List.html 
import Data.Ord --https://hackage.haskell.org/package/base/docs/Data-Ord.html 
import Data.Maybe --https://hackage.haskell.org/package/base/docs/Data-Maybe.html 
import Data.Either --https://hackage.haskell.org/package/base/docs/Data-Either.html 
import Data.Char --https://hackage.haskell.org/package/base/docs/Data-Char.html 
import Data.Function --https://hackage.haskell.org/package/base/docs/Data-Function.html 
import Data.Tuple --https://hackage.haskell.org/package/base/docs/Data-Tuple.html 
import Data.Foldable --https://hackage.haskell.org/package/base/docs/Data-Foldable.html 
import Control.Monad --https://hackage.haskell.org/package/base/docs/Control-Monad.html 
import Text.Parsec --https://hackage.haskell.org/package/parsec/docs/Text-Parsec.html 
import qualified Data.Map as Map --https://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html 
import qualified Data.Set as Set --https://hackage.haskell.org/package/containers/docs/Data-Set.html 

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
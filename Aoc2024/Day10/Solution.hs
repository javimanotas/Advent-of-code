import Utils
import Data.List --https://hackage.haskell.org/package/base/docs/Data-List.html 
import Data.Char --https://hackage.haskell.org/package/base/docs/Data-Char.html 
import Text.Parsec --https://hackage.haskell.org/package/parsec/docs/Text-Parsec.html 
import qualified Data.Map as Map --https://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html 


heightsPath = "Aoc2024/Day10/heights.in"


score :: ([(Int, Int)] -> [(Int, Int)]) -> Matrix Int -> (Int, Int) -> Int
score fun mtx pos = length $ fun $ ends mtx 0 pos
   where
      ends _ 9 pos = [pos]
      ends mtx n pos = foldMap (ends mtx (n + 1)) dirs
         where
            dirs = filter ((== Just (n + 1)) . (`Map.lookup` mtx)) $ offsets2D pos


main :: IO ()
main = do

   mtx <- matrixFromList <$> parseFile heightsPath (many1(digitToInt <$> digit))
   let origins = Map.keys $ Map.filter (==0) mtx
   mapM_ (\fun -> print $ sum $ map (score fun mtx) origins) [nub, id]
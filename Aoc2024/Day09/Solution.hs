import Utils
import Data.List --https://hackage.haskell.org/package/base/docs/Data-List.html  
import Data.Char --https://hackage.haskell.org/package/base/docs/Data-Char.html 
import Text.Parsec --https://hackage.haskell.org/package/parsec/docs/Text-Parsec.html 
import qualified Data.Map as Map  --https://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html 

numbersPath = "Aoc2024/Day09/numbers.in"


fillGaps :: Map.Map Int (Maybe Int) -> [Int]
fillGaps = unfoldr $ \m -> do
   (k, m') <- Map.minView m
   case k of
      Just x -> Just (x, m')
      Nothing ->
         let ((_, Just a), b) = until ((/= Nothing) . snd . fst) (Map.deleteFindMax . snd) ((undefined, Nothing), m')
         in Just (a, b)


checksums :: (Int, Map.Map Int (Either Int (Int, Int))) -> [Int]
checksums = unfoldr $ \(start, m) -> do
   ((k, a), m') <- Map.minViewWithKey m
   let result n r = sum $ map (*r) [start..start+n-1]
   case a of
      Right (n, r) -> Just (result n r, (start + n, m'))
      Left n -> case find (validR n . snd) $ Map.toDescList m' of
         Nothing -> Just (0, (start + n, m'))
         Just (k', r@(Right (nn, i))) ->
            let map = if nn == n
                 then Map.insert k' (Left n) m'
                 else Map.insert k (Left $ n - nn) $ Map.insert k' (Left nn) m'
            in Just (result nn i, (start + nn, map))
      where
         validR n (Right (k, _)) = k <= n
         validR _ _ = False


main :: IO ()
main = do

   [numbers] <- parseFile numbersPath $ many1 (digitToInt <$> digit)

   let encoded = Map.fromList $ mapi (,) $ concat $ mapi (\i n -> if even i
         then replicate n (Just (i `div` 2))
         else replicate n Nothing) numbers
   print $ sum $ mapi (*) $ fillGaps encoded

   let encoded' = Map.fromList $ mapi (,) $ mapi (\i n -> if even i
         then Right (n, i `div` 2)
         else Left n) numbers
   print $ sum $ checksums (0, encoded')
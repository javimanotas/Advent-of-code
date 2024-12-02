import Utils
import Data.List
import Text.Parsec

seqsPath = "Aoc2024/Day02/seqs.in"


valid :: [Int] -> Bool
valid l = all (`elem` [1..3]) diff || all (`elem` [-3.. -1]) diff
   where
      diff = zipWith (-) l $ tail l


valid' :: [Int] -> Bool
valid' l = any valid (filter ((>= (length l - 1)) . length) $ subsequences l)


main :: IO ()
main = do
   
   numbers <- parseFile seqsPath $ integral `sepBy` spaces
   mapM_ (print . (`howMany` numbers)) [valid, valid']
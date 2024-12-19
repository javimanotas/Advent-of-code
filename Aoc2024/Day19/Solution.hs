import Utils
import Data.List --https://hackage.haskell.org/package/base/docs/Data-List.html 
import Text.Parsec --https://hackage.haskell.org/package/parsec/docs/Text-Parsec.html 

patternsPath = "Aoc2024/Day19/patterns.in"
towelsPath = "Aoc2024/Day19/towels.in"


numWays :: [String] -> String -> Int
numWays patterns = evalMemo . aux
   where
      aux "" = return 1
      aux str = memoize str $ sum <$> mapM aux suffixes
         where
            suffixes = map (str\\) $ filter (`isPrefixOf` str) patterns


main :: IO ()
main = do

   [patterns] <- parseFile patternsPath $ many letter `sepBy` string ", "
   towels <- fileLines towelsPath

   print $ howMany ((/= 0) . numWays patterns) towels
   print $ sum $ map (numWays patterns) towels
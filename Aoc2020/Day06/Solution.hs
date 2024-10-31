import Utils
import qualified Data.Set as Set

answersPath = "Aoc2020/Day06/answers.in"


main :: IO ()
main = do

    answers <- splitWhen Set.null . map Set.fromList <$> fileLines answersPath
    
    print $ sum $ map (Set.size . Set.unions) answers
    print $ sum $ map (Set.size . foldl1 Set.intersection) answers
import Utils
import Text.Parsec
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (join)

replacementsPath = "Aoc2015/Day19/replacements.in"
moleculePath = "Aoc2015/Day19/molecule.in"


replace :: Map.Map String [String] -> String -> [String]
replace m = aux ""
    where
        aux _ [] = []
        aux acc l@(x:xs) = concat [ map (\v -> reverse acc ++ v ++ (l \\ k)) (m Map.! k)
                                  | k <- Map.keys m
                                  , k `isPrefixOf` l] ++ aux (x:acc) xs


minSteps :: Map.Map String [String] -> String -> Maybe Int
minSteps m = evalMemo . aux
    where
        aux "e" = pure $ Just 0
        aux str = do
            l <- mapM aux $ replace m str
            pure $ fmap succ $ join $ find (/= Nothing) l


main :: IO ()
main = do

    molecule <- readFile moleculePath
    
    replacements <- foldl (flip $ uncurry (Map.insertWith (++))) Map.empty <$> parseFile replacementsPath (do
            a <- many1 letter
            string " => "
            (a, ) . (:[]) <$> many1 letter
        )
    print $ length $ nub $ replace replacements molecule

    revReplacements <- Map.fromList <$> parseFile replacementsPath (do
            a <- many1 letter
            string " => "
            (, [a]) <$> many1 letter
        )
    print $ minSteps revReplacements molecule
    -- Part 2 is literally impossible even trying to find one solution and not the
    -- minimum one takes forever. I have no clue how to improve this.
import Utils
import Data.Char
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

edgesPath = "Aoc2021/Day12/edges.in"


createGraph :: [(String, String)] -> Graph String
createGraph = foldl insert Map.empty
    where
        insert m (a, b) = Map.insertWith (++) a [b] $ Map.insertWith (++) b [a] m


numPaths :: Graph String -> String -> Int
numPaths graph = aux Set.empty
    where
        aux _ "end" = 1
        aux s str = let neighbors = filter (`Set.notMember` s) $ graph Map.! str
                    in sum $ map (aux $ if all isAsciiUpper str then s else Set.insert str s) neighbors


numPaths' :: Graph String -> String -> Int
numPaths' graph = aux Set.empty True
    where
        aux _ _ "end" = 1
        aux s opt str = sum $ do
            node <- graph Map.! str
            guard $ node /= "start"
            guard $ opt || node `Set.notMember` s
            let (s', opt')
                    | all isAsciiUpper node = (s, opt)
                    | node `Set.member` s = (s, False)
                    | otherwise = (Set.insert node s, opt)
            pure $ aux s' opt' node


main :: IO ()
main = do

    graph <- createGraph . map ((\[a, b] -> (a, b)) . splitWhen (== '-')) <$> fileLines edgesPath
    
    print $ numPaths graph "start"
    print $ numPaths' graph "start"
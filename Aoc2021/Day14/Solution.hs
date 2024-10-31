import Utils
import Data.List
import Data.Ord
import Data.Function
import qualified Data.Map as Map

polymerPath = "Aoc2021/Day14/polymer.in"
rulesPath = "Aoc2021/Day14/rules.in"


frequencies :: Map.Map String Char -> String-> Int -> [Int]
frequencies rules polymer n = sort $ map ((`div` 2). snd) endState
    where
        mkGroups xs = map (\l -> (fst $ head l, sum $ map snd l))
                    $ groupBy ((==) `on` fst)
                    $ sortBy (comparing fst) xs
        
        pairs = mkGroups $ zipWith (\a b -> ([a, b], 1)) polymer $ tail polymer
        
        step pairs = mkGroups $ do
            (p@[a, b], i) <- pairs
            let c = rules Map.! p
            [([a, c], i), ([c, b], i)]

        endState = mkGroups $ (head polymer, 1) : (last polymer, 1) : do
            ([a, b], i) <- iterate step pairs !! n
            [(a, i), (b, i)]


main :: IO ()
main = do

    rules <- Map.fromList . map ((\[a, b] -> (a, head b)) . splitWhen (`elem` " ->")) <$> fileLines rulesPath
    polymer <- readFile polymerPath

    mapM_ (print . ((-) . last <*> head) . frequencies rules polymer) [10, 40]
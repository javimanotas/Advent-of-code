import Utils

treesPath = "Aoc2020/Day03/trees.in"


foundedTrees :: [String] -> Int -> Int -> Int
foundedTrees tress x y = howMany ((== '#') . head) cols
    where
        cols = mapi (drop . (*x)) rows
        rows = filteri (\i _ -> i `mod` y == 0) tress


main :: IO ()
main = do

    trees <- map cycle <$> fileLines treesPath

    print $ foundedTrees trees 3 1
    print $ product $ map (uncurry $ foundedTrees trees) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
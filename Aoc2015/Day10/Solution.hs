import Data.List


step :: String -> Int -> Int
step str i = length $ iterate aux str !! i
    where
        aux x = concatMap (\l -> [head $ show $ length l, head l]) $ group x


main :: IO ()
main = do

    let input = "1113122113"
    mapM_ (print . step input) [40, 50]
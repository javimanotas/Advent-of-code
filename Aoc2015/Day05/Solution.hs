import Utils
import Data.List

stringsPath = "Aoc2015/Day05/strings.in"


isNice :: String -> Bool
isNice str = howMany (`elem` "aeiou") str >= 3
           && any (uncurry (==)) (zip str $ tail str)
           && all (\s -> not (s `isInfixOf` str)) ["ab", "cd", "pq", "xy"]


isNice' :: String -> Bool
isNice' str = aux (zip str $ tail str)
            && any (\(a, _, b) -> a == b) (zip3 str (tail str) $ tail $ tail str)
    where
        aux (x:y:xs) = x `elem` xs || aux (y:xs)
        aux _ = False


main :: IO ()
main = do

    strings <- fileLines stringsPath
    
    print $ howMany isNice strings
    print $ howMany isNice' strings
    
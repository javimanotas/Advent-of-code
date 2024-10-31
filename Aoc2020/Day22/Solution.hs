import Utils
import qualified Data.Set as Set

player1Path = "Aoc2020/Day22/player1.in"
player2Path = "Aoc2020/Day22/player2.in"


data Result = Player1 Int | Player2 Int


score :: [Int] -> Int
score = sum . zipWith (*) [1..] . reverse


getScore :: Result -> Int
getScore (Player1 x) = x
getScore (Player2 x) = x


playGame :: [Int] -> [Int] -> Int
playGame a = score . aux a
    where
        aux [] l = l
        aux l [] = l
        aux (x:xs) (y:ys)
            | x > y = aux (xs ++ [x, y]) ys
            | y > x = aux xs (ys ++ [y, x])


recursiveCombat :: [Int] -> [Int] -> Int
recursiveCombat a = getScore . aux Set.empty a
    where
        aux _ xs [] = Player1 $ score xs
        aux _ [] ys = Player2 $ score ys
        aux s (x:xs) (y:ys)
            | (xs, ys) `Set.member` s = Player1 $ score xs
            | x > length xs || y > length ys = if x > y
                then aux (Set.insert (xs, ys) s) (xs ++ [x, y]) ys
                else aux (Set.insert (xs, ys) s) xs (ys ++ [y, x])
            | otherwise = case aux Set.empty (take x xs) (take y ys) of
                Player1 p -> aux (Set.insert (xs, ys) s) (xs ++ [x, y]) ys
                Player2 p -> aux (Set.insert (xs, ys) s) xs (ys ++ [y, x])


main :: IO ()
main = do

    player1 <- parseFile player1Path integral
    player2 <- parseFile player2Path integral

    print $ playGame player1 player2
    print $ recursiveCombat player1 player2
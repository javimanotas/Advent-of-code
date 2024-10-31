import Utils

commandsPath = "Aoc2021/Day02/commands.in"


move :: (Int, Int) -> (String, Int) -> (Int, Int)
move (a, b) ("forward", x) = (a + x, b)
move (a, b) ("down"   , x) = (a, b + x)
move (a, b) ("up"     , x) = (a, b - x)


move' :: [Int] -> (String, Int) -> [Int]
move' [a, b, c] ("forward", x) = [a + x, b + c * x, c]
move' [a, b, c] ("down"   , x) = [a, b, c + x]
move' [a, b, c] ("up"     , x) = [a, b, c - x]


main :: IO ()
main = do
    
    commands <- map ((\[a, b] -> (a, read b)) . splitWhen (== ' ')) <$> fileLines commandsPath
    
    print $ uncurry (*) $ foldl move (0, 0) commands
    print $ product $ init $ foldl move' [0, 0, 0] commands
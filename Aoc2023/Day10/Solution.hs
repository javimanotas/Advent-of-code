{-#LANGUAGE LambdaCase #-}
import Utils
import Data.List
import qualified Data.Map as Map
import Distribution.Simple (KnownExtension(LambdaCase))

mazePath = "Aoc2023/Day10/maze.in"


validExit :: Char -> (Int, Int) -> [(Int, Int)]
validExit c (a, b) = case c of
    '-' -> [(a, b - 1), (a, b + 1)]
    '|' -> [(a + 1, b), (a - 1, b)]
    'L' -> [(a - 1, b), (a, b + 1)]
    'J' -> [(a - 1, b), (a, b - 1)]
    '7' -> [(a + 1, b), (a, b - 1)]
    'F' -> [(a + 1, b), (a, b + 1)]
    'S' -> [(a, b + 1), (a, b - 1), (a + 1, b), (a - 1, b), (a + 1, b + 1), (a + 1, b - 1), (a - 1, b + 1), (a - 1, b - 1)]


enters = Map.fromList [
                ('-', [(0, 1), (0, -1)]),
                ('|', [(-1, 0), (1, 0)]),
                ('L', [(1, 0), (0, -1)]),
                ('J', [(1, 0), (0, 1)]),
                ('7', [(-1, 0), (0, 1)]),
                ('F', [(-1, 0), (0, -1)]),
                ('.', [])
            ]


validEnter :: Char -> (Int, Int) -> Bool
validEnter c p = c == 'S' || p `elem` (enters Map.! c)
        

findPath :: Matrix Char -> (Int, Int) -> [(Int, Int)]
findPath mtx start = head $ aux (-1, -1) start
    where
        aux last p@(a, b)
            | p == start && last /= (-1, -1) = [[]]
            | otherwise = let exits = validExit (mtx Map.! p) p
                              valid = filter (\p2@(x, y) -> p2 `Map.member` mtx && validEnter (mtx Map.! p2) (x - a, y - b)) exits
                          in foldMap (map (p:) . aux p) $ delete last valid


enclosed :: Matrix Char -> Int
enclosed mtx = sum $ map (scan ' ' False 0) [0..maxRow]
    where
        maxRow = maximum $ map fst $ Map.keys mtx

        other = \case 'L' -> '7'
                      'F' -> 'J'
                      _ -> '?'

        scan c b col row
            | (row, col) `Map.notMember` mtx = 0
            | otherwise = case mtx Map.! (row, col) of
                '-' -> scan c b (col + 1) row
                '|' -> scan '|' (not b) (col + 1) row
                '.' -> scan '.' b (col + 1) row + if b then 1 else 0
                x -> scan x (if other c == x then not b else b) (col + 1) row


main :: IO ()
main = do

    maze <- matrixFromList <$> fileLines mazePath
    let start = head $ Map.keys $ Map.filter (== 'S') maze

    let positions = findPath maze start
    print $ length positions `div` 2

    let ((a0, a1), (b0, b1), (c0, c1)) = (head positions, head $ tail positions, last positions)
    let (Just sChar) = find (\k -> all (`elem` (enters Map.! k)) [(a0 - b0, a1 - b1), (a0 - c0, a1 - c1)]) $ Map.keys enters
    let maze' = Map.mapWithKey (\k c -> if c == 'S' then sChar
                                else if k `elem` positions then c else '.') maze
    print $ enclosed maze'
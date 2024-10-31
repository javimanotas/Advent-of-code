{-# LANGUAGE LambdaCase #-}
import Utils
import Data.Either
import Data.List
import Control.Monad

chunksPath = "Aoc2021/Day10/chunks.in"


parsing :: String -> Either Char String
parsing (x:xs) = aux [x] xs
    where
        aux [] (y:ys)
            | isClosing y = Left y
            | otherwise = aux [y] ys
        aux (x:xs) (y:ys)
            | isClosing y = if other x == y then aux xs ys else Left y
            | otherwise = aux (y:x:xs) ys
        aux xs [] = Right $ map other xs
        
        isClosing = (`elem` ")]}>")
        
        other = \case
            '(' -> ')'
            '[' -> ']'
            '{' -> '}'
            '<' -> '>'


main :: IO ()
main = do

    lines <- fileLines chunksPath
    let parsed = map parsing lines
    
    print $ sum $ map (\case
        ')' -> 3
        ']' -> 57
        '}' -> 1197
        '>' -> 25137) $ lefts parsed

    let scores = sort $ map (foldl (\acc c -> 5 * acc + case c of
            ')' -> 1
            ']' -> 2
            '}' -> 3
            '>' -> 4
            ) 0) $ rights parsed
    
    print $ scores !! (length scores `div` 2)
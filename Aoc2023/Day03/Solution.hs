import Utils
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map

enginePath = "Aoc2023/Day03/engine.in"


adjacentsTo :: Matrix Char -> (Int, Int) -> [Int]
adjacentsTo mtx (a, b) = map (read . map (mtx Map.!)) numsPos
    where
        offsets = [(a - 1, b), (a + 1, b), (a, b + 1), (a, b - 1), (a + 1, b + 1), (a + 1, b - 1), (a - 1, b + 1), (a - 1, b - 1)]
        positions = filter (isDigit . fromMaybe ' ' . (`Map.lookup` mtx)) offsets
        numsPos = nub $ mapMaybe (\p -> do
                (a0, b0) <- go (-1) p
                (_, b1) <- go 1 (a0, b0)
                pure $ map (a0, ) [b0 .. b1]
            ) positions

        go n (a, b) = if isDigit $ fromMaybe ' ' $ Map.lookup (a, b + n) mtx
                then go n (a, b + n)
                else pure (a, b)


main :: IO ()
main = do

    engine <- matrixFromList <$> fileLines enginePath

    print $ sum $ foldMap (adjacentsTo engine) $ Map.keys $ Map.filter ((&&) . (not . isDigit) <*> (/= '.')) engine
    print $ sum $ map (\p -> let a = adjacentsTo engine p
                             in if length a == 2 then product a else 0) $ Map.keys $ Map.filter (== '*') engine
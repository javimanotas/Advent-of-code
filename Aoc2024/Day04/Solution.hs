import Utils
import qualified Data.Map as Map

matrixPath = "Aoc2024/Day04/matrix.in"


main :: IO ()
main = do

   mtx <- matrixFromList <$> fileLines matrixPath

   print $ sum [1 | let offsets = allOffsets2D (0, 0)
                  , key <- Map.keys $ Map.filter (== 'X') mtx
                  , let neighbors = map (\dir -> map (\n -> key /+ n /* dir) [1..3]) offsets
                  , content <- map (mapM (`Map.lookup` mtx)) neighbors
                  , content == Just "MAS"]

   print $ sum [1 | key <- Map.keys $ Map.filter (== 'A') mtx
                  , let content = mapM (`Map.lookup` mtx) $ corners2D key
                  , content `elem` map Just ["SMMS", "MSSM", "SSMM", "MMSS"]]
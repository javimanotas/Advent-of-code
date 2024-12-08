import Utils
import Data.List --https://hackage.haskell.org/package/base/docs/Data-List.html 
import qualified Data.Map as Map --https://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html 

antennasPath = "Aoc2024/Day08/antennas.in"


main :: IO ()
main = do

   mtx <- matrixFromList <$> fileLines antennasPath
   let antennas = inverseMap $ Map.filter (/= '.') mtx
   let antennasPos = concat $ Map.elems antennas

   mapM_ (\offsets -> print $ length $ nub $ concat $ Map.elems $ Map.map
      (\l -> [p | x <- l
                , y <- l
                , x /= y
                , p <- map ((x /+) . (/* (y /- x))) offsets
                , p `Map.member` mtx]) antennas
      ) [[-1, 2], [-100..100]]
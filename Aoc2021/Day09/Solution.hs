import Utils
import Data.Maybe
import Data.Char
import Data.List
import Data.Ord
import qualified Data.Map as Map
import qualified Data.Set as Set

heightmapPath = "Aoc2021/Day09/heightmap.in"


riskLevel :: Matrix Int -> Int
riskLevel matrix = sum $ map ((+1) . (matrix Map.!)) lowPoints
    where
        lowPoints = filter (\k -> all (> (matrix Map.! k)) $ neighbors k) $ Map.keys matrix

        neighbors (a, b) = mapMaybe (`Map.lookup` matrix) [(a, b + 1), (a + 1, b), (a, b - 1), (a - 1, b)]


basinsSizes :: Set.Set (Int, Int) -> [Int]
basinsSizes set
    | Set.null set = []
    | otherwise = Set.size basin : basinsSizes (Set.difference set basin)
    where
        start = Set.elemAt 0 set

        basin = fill Set.empty start

        fill :: Set.Set (Int, Int) -> (Int, Int) -> Set.Set (Int, Int)
        fill s (a, b) = let offsets = [(a, b), (a + 1, b), (a, b + 1), (a - 1, b), (a, b - 1)]
                            news = filter (\p -> not (Set.member p s) && Set.member p set) offsets
                            s' = foldl (flip Set.insert) s news
                        in foldl fill s' news
        

main :: IO ()
main = do
    
    matrix <- Map.map digitToInt . matrixFromList <$> fileLines heightmapPath
    print $ riskLevel matrix

    let set = Set.fromList $ Map.keys $ Map.filter (/= 9) matrix
    print $ product $ take 3 $ sortBy (comparing Down) $ basinsSizes set
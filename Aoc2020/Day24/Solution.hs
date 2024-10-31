import Utils
import Text.Parsec
import Data.List
import qualified Data.Set as Set

directionsPath = "Aoc2020/Day24/directions.in"


data Direction = E | SE | SW | W | NW | NE deriving (Enum, Bounded)


offsets (a, b) = map (move (a, b)) $ enumFrom minBound


move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) d = case d of
    E -> (x + 1, y)
    W -> (x - 1, y)
    NE -> (x, y + 1)
    SW -> (x, y - 1)
    NW -> (x - 1, y + 1)
    SE -> (x + 1, y - 1)


flipSet :: Set.Set (Int, Int) -> (Int, Int) -> Set.Set (Int, Int)
flipSet set coords
    | coords `Set.member` set = Set.delete coords set
    | otherwise = Set.insert coords set


step :: Set.Set (Int, Int) -> Set.Set (Int, Int)
step set = Set.fromList $ filter (\c -> if c `Set.member` set
                                            then neig c `elem` [1, 2]
                                            else neig c == 2) candidates
    where
        neig = howMany (`Set.member` set) . offsets
        candidates = foldMap offsets $ Set.toList set


main :: IO ()
main = do

    directions <- parseFile directionsPath $ many $ choice
                                           $ map try
                                           $ zipWith (\a b -> string a >> pure b)
                                           ["e", "se", "sw", "w", "nw", "ne" ]
                                           [E, SE, SW, W, NW, NE]
    
    let coordinates = map (foldl move (0, 0)) directions
    let tiles = foldl flipSet Set.empty coordinates
    
    print $ Set.size tiles
    print $ Set.size $ iterate step tiles !! 100
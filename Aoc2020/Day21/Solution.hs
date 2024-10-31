import Utils
import Text.Parsec
import Data.List
import Data.Ord
import qualified Data.Set as Set

foodsPath = "Aoc2020/Day21/foods.in"


data Entry = Entry { ingredients :: Set.Set String
                   , allergens :: Set.Set String }


solve :: [Entry] -> ([Set.Set String], [(String, String)])
solve entries
    | Set.null aller = (map ingredients entries, [])
    | otherwise = (mapped++) <$> solve entries'
    where
        aller = foldMap allergens entries
        mapped = [ (a, Set.elemAt 0 candidates)
                 | a <- Set.toList aller
                 , let candidates = intersections [ingredients e | e <- entries, a `Set.member` allergens e]
                 , Set.size candidates == 1 ]
        
        entries' = map (\e -> e {
            ingredients = foldl (\s (_, b) -> Set.delete b s) (ingredients e) mapped,
            allergens   = foldl (\s (a, _) -> Set.delete a s) (allergens e) mapped
        }) entries


main :: IO ()
main = do

    entries <- parseFile foodsPath (do
            ingredients <- many1 letter `sepEndBy1` spaces
            string "(contains "
            allergens <- many1 letter `sepEndBy1` string ", "
            pure $ Entry (Set.fromList ingredients) (Set.fromList allergens)
        )
    let (without, allergens) = solve entries

    print $ sum $ map Set.size without
    print $ intercalate "," $ map snd $ sortBy (comparing fst) allergens
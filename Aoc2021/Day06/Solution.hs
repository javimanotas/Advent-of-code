import Utils
import Data.List
import Data.Maybe
import qualified Data.Map as Map

fishesPath = "Aoc2021/Day06/fishes.in"


step :: Map.Map Int Int -> Map.Map Int Int
step map =
    let new = Map.mapKeys (subtract 1) map
        reproductions = fromMaybe 0 $ Map.lookup (-1) new
    in Map.insertWith (+) 6 reproductions $ Map.insertWith (+) 8 reproductions $ Map.delete (-1) new


main :: IO ()
main = do

    fishes <- Map.fromList . map ((,) . head <*> length) . group . sort . map read . splitWhen (== ',') <$> readFile fishesPath
    mapM_ (\n -> print . sum . Map.elems $ iterate step fishes !! n) [80, 256]
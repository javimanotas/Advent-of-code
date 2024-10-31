import Utils
import Text.Parsec
import Data.List
import Data.Ord
import Control.Monad
import qualified Data.Set as Set
import Data.Foldable (maximumBy)

pointsPath = "Aoc2021/Day13/points.in"
foldsPath = "Aoc2021/Day13/folds.in"


data Fold = FoldX Int | FoldY Int


fold :: Set.Set (Int, Int) -> Fold -> Set.Set (Int, Int)
fold s f = Set.map (\(x, y) -> case f of
    FoldX i -> if x > i then (i - (x - i), y) else (x, y)
    FoldY i -> if y > i then (x, i - (y - i)) else (x, y) ) s


main :: IO ()
main = do

    points <- Set.fromList . map ((\[a, b] -> (a, b)) . map read . splitWhen (== ',')) <$> fileLines pointsPath
    folds <- parseFile foldsPath (do
        let foldx = string "fold along x=" >> read <$> many digit
        let foldy = string "fold along y=" >> read <$> many digit
        FoldX <$> try foldx <|> FoldY <$> foldy)
    
    
    print $ Set.size $ fold points $ head folds

    let end = foldl fold points folds
    let maxX = fst $ maximumBy (comparing fst) $ Set.toList end
    let maxY = snd $ maximumBy (comparing snd) $ Set.toList end
    
    forM_ [0..maxY] (\y -> do
            forM_ [0..maxX] (\x -> if Set.member (x, y) end
                then putStr "#"
                else putStr " ")
            putStrLn ""
        )
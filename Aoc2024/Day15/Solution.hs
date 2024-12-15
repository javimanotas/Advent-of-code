import Utils
import Data.List --https://hackage.haskell.org/package/base/docs/Data-List.html 
import qualified Data.Map as Map --https://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html 
import qualified Data.Set as Set --https://hackage.haskell.org/package/containers/docs/Data-Set.html 

gridPath = "Aoc2024/Day15/grid.in"
directionsPath = "Aoc2024/Day15/directions.in"


toVector :: Char -> (Int, Int)
toVector c = case c of
   '^' -> (-1, 0)
   'v' -> (1, 0)
   '<' -> (0, -1)
   '>' -> (0, 1)


type GridState = (Set.Set (Int, Int), (Int, Int))


simulate :: Set.Set (Int, Int) -> GridState -> (Int, Int) -> GridState
simulate walls (boxes, pos) dir = case tillSpaces of
   Nothing -> (boxes, pos)
   Just p -> let boxesToMove = takeWhile (/= p) tillWall
             in (Set.map (\b -> if b `elem` boxesToMove then b /+ dir else b) boxes, pos /+ dir)
   where
      tillWall = takeWhile (`Set.notMember` walls) $ map (\n -> pos /+ n /* dir) [1..]
      tillSpaces = find (`Set.notMember` boxes) tillWall


expand :: String -> String
expand [] = []
expand ('#':xs) = '#' : '#' : expand xs
expand ('O':xs) = '[' : ']' : expand xs
expand ('.':xs) = '.' : '.' : expand xs
expand ('@':xs) = '@' : '.' : expand xs


type GridState' = (Matrix Char, (Int, Int))


simulate' :: Set.Set (Int, Int) -> GridState' -> (Int, Int) -> GridState'
simulate' walls (boxes, pos) dir
   | fst dir /= 0 = case boxesToMove pos of
      Nothing -> (boxes, pos)
      Just l -> (move l boxes, pos /+ dir)
   | otherwise = 
      let tillWall = takeWhile (`Set.notMember` walls) $ map (\n -> pos /+ n /* dir) [1..]
      in case find (`Map.notMember` boxes) tillWall of
               Nothing -> (boxes, pos)
               Just p ->
                  let boxesToMove = takeWhile (/= p) tillWall
                  in (move boxesToMove boxes, pos /+ dir)
      where
         move l = Map.mapKeys (\k -> if k `elem` l then k /+ dir else k)

         boxesToMove p
            | p' `Set.member` walls = Nothing
            | p' `Map.notMember` boxes = Just [p]
            | otherwise = do
               a <- boxesToMove p'
               b <- boxesToMove otherBox
               return $ p : a ++ b
            where
               p' = p /+ dir
               otherBox = case boxes Map.! p' of
                  '[' -> p' /+ (0, 1)
                  ']' -> p' /+ (0, -1)


main :: IO ()
main = do

   directions <- foldMap (map toVector) <$> fileLines directionsPath
   
   lines <- fileLines gridPath
   
   let grid = matrixFromList lines
       walls = Set.fromList $ Map.keys $ Map.filter (=='#') grid
       boxes = Set.fromList $ Map.keys $ Map.filter (=='O') grid
       [startPos] = Map.keys $ Map.filter (=='@') grid

   let (result, _) = foldl (simulate walls) (boxes, startPos) directions
   print $ sum $ Set.map (\(a, b) -> 100 * a + b) result

   let lines' = map expand lines
       grid' = matrixFromList lines'
       walls' = Set.fromList $ Map.keys $ Map.filter (=='#') grid'
       boxes' = Map.filter (`elem` "[]") grid'
       [startPos'] = Map.keys $ Map.filter (=='@') grid'

   let (result', _) = foldl (simulate' walls') (boxes', startPos') directions
   print $ sum $ map (\(a, b) -> 100 * a + b) $ Map.keys $ Map.filter (=='[') result'
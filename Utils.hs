module Utils where

import Text.Parsec hiding (State)
import Data.Function
import Data.List
import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map

{-------------------- Memoization --------------------}

type Memo k v = State (Map.Map k v) v

evalMemo :: Memo k v -> v
evalMemo = (`evalState` Map.empty)

memoize :: (Ord k) => k -> Memo k v -> Memo k v
memoize key compute = do
    result <- gets (Map.lookup key)
    case result of
        Just value -> return value
        Nothing -> do
            value <- compute
            memo <- get
            put (Map.insert key value memo)
            return value

{-------------------- Parsing --------------------}

type StringParser = Parsec String ()

fileLines :: FilePath -> IO [String]
fileLines path = lines <$> readFile path

parseLines :: [String] -> StringParser a -> IO [a]
parseLines fLines parser = case mapM (parse parser "") fLines of
    Left err -> print err >> error "parsing error"
    Right ok -> return ok

parseFile :: FilePath -> StringParser a -> IO [a] 
parseFile path parser = do
    fLines <- fileLines path
    parseLines fLines parser

integral :: (Read a, Integral a) => StringParser a
integral = negative <|> positive
    where
        negative = char '-' >> (negate . read <$> many1 digit)
        positive = optional (char '+') >> (read <$> many1 digit)

{-------------------- Foldables --------------------}

howMany :: (Foldable f, Num b) => (a -> Bool) -> f a -> b
howMany f = foldl (\acc x -> acc + if f x then 1 else 0) 0

{-------------------- Lists --------------------}

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen f = filter (not . any f) . groupBy ((==) `on` f)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = take n l : chunksOf n (drop n l)

mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f = zipWith f [0..]

filteri :: (Int -> a -> Bool) -> [a] -> [a]
filteri f = map snd . filter (uncurry f) . zip [0..]

{-------------------- Sets --------------------}

intersections :: (Foldable f, Ord a) => f (Set.Set a) -> Set.Set a
intersections sets
    | null sets = Set.empty
    | otherwise = foldl1 Set.intersection sets

{-------------------- Graphs --------------------}

type Graph k = Map.Map k [k]

{-------------------- Matrices --------------------}

type Matrix = Map.Map (Int, Int)

matrixFromList :: [[a]] -> Matrix a
matrixFromList = Map.fromList . concat . zipWith (\n -> zip (map (n, ) [0..])) [0..]

{-------------------- Vectors2D --------------------}

(/+) :: Num a => (a, a) -> (a, a) -> (a, a)
(a, b) /+ (c, d) = (a + c, b + d)

(/-) :: Num a => (a, a) -> (a, a) -> (a, a)
(a, b) /- (c, d) = (a - c, b - d)

(/*) :: Num a => a -> (a, a) -> (a, a)
k /* (a, b) = (k * a, k * b)

offsets2D :: Num a => (a, a) -> [(a, a)]
offsets2D (a, b) = [(a, b + 1), (a + 1, b), (a, b - 1), (a - 1, b)]

offsetsC2D :: Num a => (a, a) -> [(a, a)] -- With corners
offsetsC2D (a, b) = offsetsC2D (a, b) ++ [(a + 1, b + 1), (a + 1, b - 1), (a - 1, b + 1), (a - 1, b - 1)]
import Utils
import Text.Parsec
import Data.Maybe
import qualified Data.Set as Set

instructionsPath = "Aoc2020/Day08/instructions.in"


data Instruction = Acc Int | Jmp Int | Nop Int deriving Show


exec :: [Instruction] -> Int -> Int -> Set.Set Int -> Int
exec instructions acc ptr set
    | ptr `Set.member` set = acc
exec instructions acc ptr set = case instructions !! ptr of
    Acc n -> exec instructions (acc + n) (ptr + 1) s
    Jmp n -> exec instructions acc (ptr + n) s
    Nop n -> exec instructions acc (ptr + 1) s
    where
        s = Set.insert ptr set

exec' :: [Instruction] -> Int -> Int -> Set.Set Int -> Bool -> Maybe Int
exec' instructions acc ptr set changed
    | ptr == length instructions = Just acc
    | ptr `Set.member` set = Nothing
exec' instructions acc ptr set changed = case instructions !! ptr of
    Acc n -> exec' instructions (acc + n) (ptr + 1) s changed
    Jmp n -> let a = exec' instructions acc (ptr + n) s changed
             in if isNothing a && not changed
                then exec' instructions acc (ptr + 1) s True else a
    Nop n -> let a = exec' instructions acc (ptr + 1) s changed
             in if isNothing a && not changed
                then exec' instructions acc (ptr + n) s True else a
    where
        s = Set.insert ptr set


main :: IO ()
main = do

    instructions <- parseFile instructionsPath (do
            choice $ zipWith (\str f -> string str >> (f <$> integral))
                ["acc ", "jmp ", "nop "] [Acc, Jmp, Nop]
        )

    print $ exec instructions 0 0 Set.empty
    print $ exec' instructions 0 0 Set.empty False
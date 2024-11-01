import Utils
import Text.Parsec

instructionsPath = "Aoc2015/Day23/instructions.in"


data Instrucion = Hlf Char | Tpl Char | Inc Char | Jmp Int | Jie Char Int | Jio Char Int deriving Show


instruction :: StringParser Instrucion
instruction = choice $ map try [hlf, tpl, inc, jmp, jie, jio]
    where
        hlf = string "hlf " >> (Hlf <$> letter)
        tpl = string "tpl " >> (Tpl <$> letter)
        inc = string "inc " >> (Inc <$> letter)
        jmp = string "jmp " >> (Jmp <$> integral)
        jie = do
            string "jie "
            x <- letter
            string ", "
            Jie x <$> integral
        jio = do
            string "jio "
            x <- letter
            string ", "
            Jio x <$> integral


exec :: [Instrucion] -> (Int, (Int, Int)) -> Int -- Note b is only used on 1 place on the input file
exec instructions (pc, (a, b))
    | pc >= length instructions = b
    | otherwise = case instructions !! pc of
        Hlf c -> exec instructions (pc + 1, (a `div` 2, b))
        Tpl c -> exec instructions (pc + 1, (a * 3, b))
        Inc c -> exec instructions (pc + 1, if c == 'a' then (a + 1, b) else (a, b + 1))
        Jmp i -> exec instructions (pc + i, (a, b))
        Jie c i -> exec instructions (if even a then pc + i else pc + 1, (a, b))
        Jio c i -> exec instructions (if a == 1 then pc + i else pc + 1, (a, b))


main :: IO ()
main = do

    instructions <- parseFile instructionsPath instruction
    print instructions
    print $ exec instructions (0, (0, 0))
    print $ exec instructions (0, (1, 0))
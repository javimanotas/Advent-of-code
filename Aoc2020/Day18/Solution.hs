import Utils
import Text.Parsec

expressionsPath = "Aoc2020/Day18/expressions.in"


data Term = Num Int | Addition Term Term | Multiplication Term Term


addition :: StringParser (Term -> Term -> Term)
addition = string " + " >> pure Addition


multiplication :: StringParser (Term -> Term -> Term)
multiplication = string " * " >> pure Multiplication


factor :: StringParser Term -> StringParser Term
factor term = number <|> between (char '(') (char ')') term
    where
        number = Num . read <$> many1 digit


term :: StringParser Term
term = chainl1 (factor term) (try addition <|> try multiplication)


term' :: StringParser Term
term' = chainl1 additionTerm (try multiplication)
    where
        additionTerm = chainl1 (factor term') (try addition)


eval :: Term -> Int
eval (Num n) = n
eval (Addition a b) = eval a + eval b
eval (Multiplication a b) = eval a * eval b


main :: IO ()
main = do

    expressions <- parseFile expressionsPath term
    print $ sum $ map eval expressions

    expressions' <- parseFile expressionsPath term'
    print $ sum $ map eval expressions'
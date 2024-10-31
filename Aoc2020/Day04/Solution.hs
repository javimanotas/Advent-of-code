import Utils
import Text.Parsec
import Data.List
import Data.Char
import qualified Data.Map as Map

passportsPath = "Aoc2020/Day04/passports.in"


fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]


completed :: Map.Map String String -> Bool
completed p = all (`Map.member` p) fields


valid :: String -> String -> Bool
valid key value = case key of
    "byr" -> let x = read value in x >= 1920 && x <= 2002
    "iyr" -> let x = read value in x >= 2010 && x <= 2020
    "eyr" -> let x = read value in x >= 2020 && x <= 2030
    "hgt" -> case filter (not . isDigit) value of
                    "cm" -> let num = read $ filter isDigit value in num >= 150 && num <= 193
                    "in" -> let num = read $ filter isDigit value in num >= 59 && num <= 76
                    _ -> False
    "hcl" -> length value == 7 && head value == '#' && all (`elem` (['0'..'9']++['a'..'f'])) (tail value)
    "ecl" -> value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    "pid" -> length value == 9 && all isDigit value
    "cid" -> True


main :: IO ()
main = do

    groups <- map unwords . splitWhen null <$> fileLines passportsPath
    passports <- map Map.fromList <$> parseLines groups (do
            let field = do
                    name <- many1 letter
                    char ':'
                    (name, ) <$> many1 (noneOf " ")
            field `sepBy` spaces
        )

    print $ howMany completed passports
    print $ howMany (all $ uncurry valid) [Map.toList p | p <- passports, completed p]
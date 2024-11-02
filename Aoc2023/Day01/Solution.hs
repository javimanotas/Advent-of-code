import Utils
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Map as Map

stringsPath = "Aoc2023/Day01/strings.in"


getDigit :: String -> Maybe Char
getDigit str = (m Map.!) <$> find (`isPrefixOf` str) (Map.keys m)
    where
        digits = map (:[]) ['0'..'9']
        m = Map.fromList $ zip digits ['0'..'9'] ++ zip ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] ['0'..'9']


main :: IO ()
main = do

    lines <- fileLines stringsPath

    mapM_ (print . sum . map (\l -> read [head l, last l]))
        [map (filter isDigit) lines, map (mapMaybe getDigit . tails) lines]
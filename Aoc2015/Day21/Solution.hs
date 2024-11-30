import Utils
import Text.Parsec
import Data.List
import Control.Monad

bossPath = "Aoc2015/Day21/boss.in"
weaponsPath = "Aoc2015/Day21/weapons.in"
armorPath = "Aoc2015/Day21/armor.in"
ringsPath = "Aoc2015/Day21/rings.in"


data Item = Item { cost :: Int
                 , dmg :: Int
                 , armor :: Int }


data Player = Player { money :: Int
                     , hitPoints :: Int
                     , attack :: Int
                     , defense :: Int }


combinations :: [a] -> [a] -> [a] -> [[a]]
combinations weapons armors rings = do
    w <- wCombs
    a <- aCombs
    r <- rCombs
    return $ w ++ a ++ r
    where
        wCombs = map (:[]) weapons
        aCombs = [] : map (:[]) armors
        rCombs = filter ((<= 2) . length) $ subsequences rings


item :: StringParser Item
item = do
    [a, b, c] <- replicateM 3 $ integral <* optional spaces
    pure $ Item a b c


wins :: Player -> Player -> Bool
a `wins` b
    | hitPoints a <= 0 = False
    | otherwise = not $ wins (b { hitPoints = hitPoints b - damage}) a
    where
        damage = max 1 $ attack a - defense b


buildPlayer :: [Item] -> Player
buildPlayer items = Player m 100 a d
    where
        m = sum $ map cost items
        a = sum $ map dmg items
        d = sum $ map armor items


main :: IO ()
main = do

    [bossHitPoints, bossDmg, bossArmor] <- parseFile bossPath integral
    let boss = Player 0 bossHitPoints bossDmg bossArmor

    [weapons, armor, rings] <- mapM (`parseFile` item) [weaponsPath, armorPath, ringsPath]

    let players = map buildPlayer $ combinations weapons armor rings

    let results = [(money p, p `wins` boss) | p <- players]
    let (wins, looses) = partition snd results

    print $ minimum $ map fst wins
    print $ maximum $ map fst looses
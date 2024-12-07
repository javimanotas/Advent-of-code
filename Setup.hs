module Main ( main ) where

import Utils
import Data.List
import Data.Maybe
import System.IO
import System.Environment
import System.Directory
import System.Process
import Control.Monad


main :: IO ()
main = do

    hSetBuffering stdout NoBuffering

    putStrLn ""
    putStrLn "  █████╗ ██████╗ ██╗   ██╗███████╗███╗   ██╗████████╗       ██████╗ ███████╗"
    putStrLn " ██╔══██╗██╔══██╗██║   ██║██╔════╝████╗  ██║╚══██╔══╝      ██╔═══██╗██╔════╝"
    putStrLn " ███████║██║  ██║██║   ██║█████╗  ██╔██╗ ██║   ██║         ██║   ██║█████╗  "
    putStrLn " ██╔══██║██║  ██║██║   ██║██╔══╝  ██║╚██╗██║   ██║         ██║   ██║██╔══╝  "
    putStrLn " ██║  ██║██████╔╝╚██████╔╝███████╗██║ ╚████║   ██║         ╚██████╔╝██║     "
    putStrLn " ╚═╝  ╚═╝╚═════╝  ╚═════╝ ╚══════╝╚═╝  ╚═══╝   ╚═╝          ╚═════╝ ╚═╝     "
    putStrLn ""
    putStrLn " ██████╗ ██████╗ ██████╗ ███████╗"
    putStrLn " ██╔════╝██╔═══██╗██╔══██╗██╔════╝"
    putStrLn " ██║     ██║   ██║██║  ██║█████╗  "
    putStrLn " ██║     ██║   ██║██║  ██║██╔══╝  "
    putStrLn " ╚██████╗╚██████╔╝██████╔╝███████╗"
    putStrLn "  ╚═════╝ ╚═════╝ ╚═════╝ ╚══════╝"
    putStrLn ""

    putStr "Select year: "
    year <- getLine

    let tree = [ "     *                      *                    ",
                 "                 *                               ",
                 "                                       *         ",
                 "         *             ,                         ",
                 "                     _/^\\_                       ",
                 "                    <     >                      ",
                 "   *                 /.-.\\         *             ",
                 "            *        `/&\\`                   *   ",
                 "                    ,@.*;@,                      ",
                 "                   /_o.I %_\\    *                ",
                 "      *           (`'--:o(_@;                    ",
                 "                 /`;--.,__ `')             *     ",
                 "                ;@`o % O,*`'`&\\                  ",
                 "          *    (`'--)_@ ;o %'()\\      *          ",
                 "               /`;--._`''--._O'@;                ",
                 "              /&*,()~o`;-.,_ `\"\"`)               ",
                 "   *          /`,@ ;+& () o*`;-';\\               ",
                 "             (`\"\"--.,_0 +% @' &()\\               ",
                 "             /-.,_    ``''--....-'`)  *          ",
                 "        *    /@%;o`:;'--,.__   __.'\\             ",
                 "            ;*,&(); @ % &^;~`\"`o;@();        *   ",
                 "            /(); o^~; & ().o@*&`;&%O\\            ",
                 "     *      `\"=\"==\"\"==,,,.,=\"==\"===\"`            ",
                 "         __.----.(\\-''#####---...___...-----._   ",
                 "       '`         \\)_`\"\"\"\"\"`                     "]
    
    createDirectoryIfMissing True $ "Aoc" ++ year
    subdirectory <- listDirectory $ "Aoc" ++ year
    mapM_ putStrLn $ zipWith (\l i -> l ++ let nums = map (drop 3) subdirectory
                                        in fromMaybe "" $ find ((== i) . read) nums) tree [25, 24..1]

    putStr "\nEnter day: "
    day <- getLine
    
    let directory = "Aoc" ++ year ++ "/" ++ "Day" ++ day ++ "/"
    let file = "Solution.hs"

    dayExists <- doesDirectoryExist directory
    
    files <- if dayExists then map (directory++) <$> listDirectory directory else do
        
        putStrLn $ "Setting up " ++ init directory ++ " ..."
        createDirectoryIfMissing True directory
        
        putStr "Enter input files names: "
        files <- splitWhen (== ' ') <$> getLine
        let inputFiles = map (\f -> directory ++ f ++ ".in") files
        mapM_ (`writeFile` "") inputFiles
        
        let filesDefinitions = map (\f -> f ++ "Path = \"" ++ directory ++ f ++ ".in" ++ "\"") files
        writeFile (directory ++ file) $
            "import Utils\n" ++
            "import Data.List --https://hackage.haskell.org/package/base/docs/Data-List.html \n" ++
            "import Data.Ord --https://hackage.haskell.org/package/base/docs/Data-Ord.html \n" ++
            "import Data.Maybe --https://hackage.haskell.org/package/base/docs/Data-Maybe.html \n" ++
            "import Data.Either --https://hackage.haskell.org/package/base/docs/Data-Either.html \n" ++
            "import Data.Char --https://hackage.haskell.org/package/base/docs/Data-Char.html \n" ++
            "import Data.Function --https://hackage.haskell.org/package/base/docs/Data-Function.html \n" ++
            "import Control.Monad --https://hackage.haskell.org/package/base/docs/Control-Monad.html \n" ++
            "import Text.Parsec --https://hackage.haskell.org/package/parsec/docs/Text-Parsec.html \n" ++
            "import qualified Data.Map as Map --https://hackage.haskell.org/package/containers/docs/Data-Map.html \n" ++
            "import qualified Data.Set as Set --https://hackage.haskell.org/package/containers/docs/Data-Set.html \n" ++
            "\n" ++
            init (unlines filesDefinitions) ++
            "\n\n\n" ++
            "main :: IO ()\n" ++
            "main = do\n" ++
            "\n" ++
            "   undefined"

        pure $ (directory ++ file) : inputFiles 

    mapM_ (callCommand . ("code " ++)) $ "." : reverse files
    callCommand $ "ghci " ++ directory ++ file
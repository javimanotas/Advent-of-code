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
            "import Data.List\n" ++
            "import Data.Ord\n" ++
            "import Data.Function\n" ++
            "import Text.Parsec\n" ++
            "import Data.Char\n" ++
            "import Data.Maybe\n" ++
            "import Data.Either\n" ++
            "import Control.Monad\n" ++
            "import qualified Data.Map as Map\n" ++
            "import qualified Data.Set as Set\n" ++
            "\n"++
            init (unlines filesDefinitions) ++
            "\n\n\n" ++
            "main :: IO ()\n" ++
            "main = do\n" ++
            "   \n" ++
            "   undefined"

        pure $ (directory ++ file) : inputFiles 

    mapM_ (callCommand . ("code " ++)) $ "." : reverse files
    callCommand $ "ghci " ++ directory ++ file
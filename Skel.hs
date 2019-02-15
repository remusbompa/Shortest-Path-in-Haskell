module Skel (
        TestType (..),
        solve
        ) where

import System.IO
import Data.List.Split
import Data.Array
import System.Environment

import Tema1


data TestType = Simple | Costs deriving Eq
instance Show TestType where
    show Simple = "simple"
    show Costs = "costs"


main = do
    -- open file
    args <- getArgs
    if length args < 2 then do
        printHelp
    else do
        let inFile = args !! 0
            outFile = args !! 1
            opt = args !! 2

        if opt == "1"
            then solve inFile outFile Simple
            else if opt == "2"
                    then solve inFile outFile Costs
                    else printHelp


printHelp :: IO ()
printHelp = putStrLn "runghc ./Skel.hs input_file output_file problem_number"



evalSimple = solveSimple . parseSimple
evalCosts = solveCosts . parseCosts


solve :: FilePath -> FilePath -> TestType -> IO ()
solve inFile outFile tt = do
    contents <- readFile inFile
    if tt == Simple
        then printSimple outFile $ evalSimple contents
        else printCosts outFile $ evalCosts contents


printSimple :: FilePath -> Maybe ([Int], Int) -> IO ()
printSimple out result = writeFile out (str result)
    where
        showPath = foldr (\n r -> show n ++ "\n" ++ r) ""
        str Nothing = "Nothing"
        str (Just (path, len)) = show len ++ "\n" ++ showPath path


printCosts :: FilePath -> Maybe ([(Int, Int)], Int) -> IO ()
printCosts out result = writeFile out (str result)
    where
        showPath = foldr (\(n, m) r -> show n ++ " " ++ show m ++ "\n" ++ r) ""
        str Nothing = "Nothing"
        str (Just (path, len)) = show len ++ "\n" ++ showPath path


tuplify3 :: [a] -> (a, a, a)
tuplify3 [x, y, z] = (x, y, z)

-- in both cases, get the number of nodes and a list of tokens
parseBasic :: String -> (Int, [Int])
parseBasic contents = (head l, tail l)
    where l = map (read :: String -> Int) (words contents)

parseSimple :: String -> (Int, [(Int, Int, Int)])
parseSimple contents = (n, edges)
    where
        (n, input) = parseBasic contents
        edges = map tuplify3 $ chunksOf 3 (tail input)  -- skip edge count

parseCosts :: String -> (Int, Int, [Int], [(Int, Int, Int)])
parseCosts contents = (n, m, costs, edges)
    where
        (n, input) = parseBasic contents
        m = input !! 1
        costs = take n (drop 2 input)
        edges = map tuplify3 $ chunksOf 3 (drop (2 + n) input)

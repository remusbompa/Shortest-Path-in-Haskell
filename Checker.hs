#!/usr/bin/env runhaskell

module Checker where

import System.Environment (getArgs)
import System.Directory (getDirectoryContents, createDirectoryIfMissing)
import System.FilePath ((</>), takeBaseName, joinPath)

import Control.Monad (liftM, forM)
import Data.List (isPrefixOf, sortBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

import Skel


getFilesFrom :: FilePath -> IO [FilePath]
getFilesFrom topDir = liftM (map concatTop) . liftM filterHardLinks $ contents
  where
    concatTop = (</>) topDir
    filterHardLinks = filter (`notElem` [".", ".."])
    contents = getDirectoryContents topDir

intercalate :: String -> [String] -> String
intercalate s = foldr1 (\x acc -> x ++ s ++ acc)

replace :: String -> String -> String -> String
replace old new = intercalate new . splitOn old

inputToOut :: FilePath -> FilePath
inputToOut = replace "in" "out"

inputToRef :: FilePath -> FilePath
inputToRef = replace "in" "ref"

readInt :: String -> Int
readInt s = read s :: Int

getTestPoints :: FilePath -> Int
getTestPoints t = 1

sortByIndex :: [String] -> [String]
sortByIndex = sortBy . comparing $ getIndex
  where
    getIndex = readInt . head . splitOn "-" . takeBaseName

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)

parseSimpleOut :: String -> Maybe ([Int], Int)
parseSimpleOut s = if isPrefixOf "Nothing" s then Nothing else Just (path, cost)
    where
        l = lines s
        cost = readInt $ head l
        path = map readInt $ drop 1 l

parseCostsOut :: String -> Maybe ([(Int, Int)], Int)
parseCostsOut s = if isPrefixOf "Nothing" s then Nothing else Just (path, cost)
    where
        l = lines s
        cost = readInt $ head l
        path = map (tuplify2 . map readInt . words) $ drop 1 l


runTests :: (Eq a, Show a)
         => TestType
         -> (String -> a)  -- parse output function
         -> IO ()
runTests tt parseOut = do
  putStrLn $ "Running " ++ show tt ++ " tests:"

  let inputsDir = joinPath [testsDir, show tt, "in"]
  let outputsDir = joinPath [testsDir, show tt, "out"]

  inputs <- sortByIndex `fmap` getFilesFrom inputsDir
  createDirectoryIfMissing False outputsDir

  points_total <- forM inputs $ \inFile -> do
    let outFile = inputToOut inFile
        refFile = inputToRef inFile

    solve inFile outFile tt
    output <- parseOut `fmap` readFile outFile
    ref <- parseOut `fmap` readFile refFile

    let baseName = takeBaseName inFile
    let outputLine = baseName ++ " " ++ replicate (24 - length baseName) '.'

    if output == ref
       then putStrLn (outputLine ++ " OK") >> return (getTestPoints inFile)
       else putStrLn (outputLine ++ " FAIL") >> return 0

  putStrLn $ "\n\t\tTOTAL: "
             ++ show (sum points_total)
             ++ "/"
             ++ show (sum . map getTestPoints $ inputs)


main = do
  args <- getArgs
  let opt = head args
  if length args == 1 && (opt == "simple" || opt == "costs" || opt == "both")
    then
        if opt == "both"
            then
                runTests Costs parseCostsOut >>
                putStrLn "" >>
                runTests Simple parseSimpleOut
            else
                if opt == "costs"
                    then runTests Costs parseCostsOut
                    else runTests Simple parseSimpleOut
    else
        putStrLn "Usage:\n\trunhaskell Checker.hs [simple|costs|both]"

testsDir = "./tests"

module Day6.P2 (solve) where

import Data.List (foldl')
import Data.Char (isDigit, digitToInt)
import Day6.P1 (findNumIntsInRootRange, parseDoubles)

solve :: IO ()
solve = do
    inp <- readFile "Day6/inp.txt"
    let allLines = lines inp

    let [timesStr, distancesStr] = allLines

    let times     = parseDoubles ((filter (/= ' ') . dropWhile (== ' ') . drop 5) timesStr ++ "$")
    let distances = parseDoubles ((filter (/= ' ') . dropWhile (== ' ') . drop 9) distancesStr ++ "$")

    let numIntRootRange = zipWith findNumIntsInRootRange times distances

    print (head numIntRootRange)
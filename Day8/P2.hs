module Day8.P2 (solve) where

import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import qualified Data.Map as Map

import Day8.P1 (parseNode, findNumSteps)


solve :: IO ()
solve = do
    inp <- readFile "Day8/inp.txt"
    let [dirLine, nodeLinesStr] = splitOn "\n\n" inp
    let nodeLines = lines nodeLinesStr

    let nodeStrs = map parseNode nodeLines
    let startNodes = filter (isSuffixOf "A") $ map fst nodeStrs
    
    let nodeMap = Map.fromList nodeStrs

    let steps = map (\startNode -> findNumSteps startNode nodeMap (cycle dirLine) (isSuffixOf "Z") 0) startNodes

    print $ foldl lcm 1 steps
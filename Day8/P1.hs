module Day8.P1 (solve, parseNode, findNumSteps) where

import Data.Char (isAlpha)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map


parseNode :: String -> (String, (String, String))
parseNode str = do
    let [node, paths] = splitOn " = " str
    let (left, right) = ((\[x, y] -> (x, takeWhile isAlpha y)) . splitOn ", " . drop 1) paths
    (node, (left, right))

mapNode :: String -> Map String (String, String) -> Char -> String
mapNode node nodeMap 'L' = fst $ nodeMap Map.! node
mapNode node nodeMap 'R' = snd $ nodeMap Map.! node


findNumSteps :: String -> Map String (String, String) -> String -> (String -> Bool) -> Int -> Int
findNumSteps start nodeMap dirs pred step = case start of
    node | pred node -> step
    _ -> do
        let dir : dirs' = dirs
        let next = mapNode start nodeMap dir
        findNumSteps next nodeMap dirs' pred (step + 1)

solve :: IO ()
solve = do
    inp <- readFile "Day8/inp.txt"
    let [dirLine, nodeLinesStr] = splitOn "\n\n" inp
    let nodeLines = lines nodeLinesStr

    let nodeStrs = map parseNode nodeLines
    let nodeMap = Map.fromList nodeStrs

    let steps = findNumSteps "AAA" nodeMap (cycle dirLine) (== "ZZZ") 0

    print steps
module Day5.P1 (solve, applyMaps) where

import Data.List (foldl')
import Data.List.Split (splitOn)
import Day5.Parsing (parseSeeds, parseMap, Range (..), inRange)


applyMap :: Int -> [Range] -> Int
applyMap seed maps =
    let seedInRange = foldr (\i range -> if inRange seed i then i else range) Range{src=0,len=0,dst=0} maps in
    case seedInRange of
        Range{src=0,len=0}   -> seed
        Range{src=s1,dst=d1} -> seed - s1 + d1

applyMaps :: [[Range]] -> Int -> Int
applyMaps = flip (foldl' applyMap)

solve :: IO ()
solve = do
    inp <- readFile "Day5/inp.txt"
    let (seedsLine : mapLines) = splitOn "\n\n" inp

    let seeds = parseSeeds seedsLine
    let maps = map parseMap mapLines
    let locs = map (applyMaps maps) seeds

    print (minimum locs)

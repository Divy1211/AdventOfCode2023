module Day10.P2 (solve) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Day10.P1 (findStartCoords, rmIdx)


findLoopIdxs :: Map Int (Map Int Char) -> [(Int, Int)] -> Set (Int, Int) -> Set (Int, Int)
findLoopIdxs maze toVisit pipes =
    let (i, j, ls) = case toVisit of
            []            -> (-1, -1, [])
            (i', j') : xs -> (i', j', xs)
        in
    if i == -1 then pipes else
    let ((i', j'), (i'', j'')) = case Map.lookup i maze >>= Map.lookup j of
            Nothing                      -> ((-1, -1), (-1, -1))
            Just x | x `elem` ['.']      -> ((-1, -1), (-1, -1))
            Just x | x `elem` ['-']      -> ((i, j-1), (i, j+1))
            Just x | x `elem` ['L']      -> ((i-1, j), (i, j+1))
            Just x | x `elem` ['J']      -> ((i-1, j), (i, j-1))
            Just x | x `elem` ['7']      -> ((i, j-1), (i+1, j))
            Just x | x `elem` ['F']      -> ((i, j+1), (i+1, j))
            Just x | x `elem` ['|', 'S'] -> ((i-1, j), (i+1, j))
        in
    case ((i', j'), (i'', j'')) of
        ((-1, -1), (-1, -1)) -> findLoopIdxs (rmIdx maze (i, j)) ls pipes
        _                    -> do
            let ls' = ls ++ [(i', j'), (i'', j'')]
            findLoopIdxs (rmIdx maze (i, j)) ls' $ foldr Set.insert pipes [(i', j'), (i'', j'')]

countInnerTiles :: [String] -> Set (Int, Int) -> Int
countInnerTiles tiles loopTiles = sum $ map (\(_, _, c, _) -> c) $ zipWith (\i str -> foldl (\(j, isInside, total, (fromUp, fromDown)) char ->
        if Set.member (i, j) loopTiles then
            case char of
                'L'                     -> (j+1, isInside, total, (True, False))
                'F'                     -> (j+1, isInside, total, (False, True))
                'J' | fromUp            -> (j+1, isInside, total, (False, False))
                'J' | fromDown          -> (j+1, not isInside, total, (False, False))
                '7' | fromUp            -> (j+1, not isInside, total, (False, False))
                '7' | fromDown          -> (j+1, isInside, total, (False, False))
                x | x `elem` ['|', 'S'] -> (j+1, not isInside, total, (fromUp, fromDown))
                _                       -> (j+1, isInside, total, (fromUp, fromDown))
        else if isInside
            then (j+1, isInside, total + 1, (fromUp, fromDown))
            else (j+1, isInside, total, (fromUp, fromDown))
    ) (0, False, 0, (False, False)) str) [0..] tiles

solve :: IO ()
solve = do
    inp <- readFile "Day10/inp.txt"
    let allLines = lines inp

    let (si, sj) = findStartCoords allLines 0
    let maze = Map.fromList $ zip [0..] $ map (Map.fromList . zip [0..]) allLines
    let markLoopTiles = findLoopIdxs maze [(si, sj)] Set.empty
    let numInside = countInnerTiles allLines markLoopTiles

    print numInside

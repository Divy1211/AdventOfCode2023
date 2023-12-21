module Day10.P1 (solve, findStartCoords, rmIdx) where


import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map


findStartCoords :: [String] -> Int -> (Int, Int)
findStartCoords (str : strs) step =
    case 'S' `elemIndex` str of
        Nothing -> findStartCoords strs (step+1)
        Just x  -> (step, x)

rmIdx :: Map Int (Map Int Char) -> (Int, Int) -> Map Int (Map Int Char)
rmIdx maze (i, j) = Map.update (Just . Map.delete j) i maze

findMaxPipeDistance :: Map Int (Map Int Char) -> [(Int, Int)] -> Int -> Int
findMaxPipeDistance maze toVisit step =
    let (i, j, ls) = case toVisit of
            []            -> (-1, -1, [])
            (i', j') : xs -> (i', j', xs)
        in
    if i == -1 then step else
    let ((i', j'), (i'', j'')) = case Map.lookup i maze >>= Map.lookup j of
            Nothing                      -> ((-1, -1), (-1, -1))
            Just '.'                     -> ((-1, -1), (-1, -1))
            Just '-'                     -> ((i, j-1), (i, j+1))
            Just 'L'                     -> ((i-1, j), (i, j+1))
            Just 'J'                     -> ((i-1, j), (i, j-1))
            Just '7'                     -> ((i, j-1), (i+1, j))
            Just 'F'                     -> ((i, j+1), (i+1, j))
            Just x | x `elem` ['|', 'S'] -> ((i-1, j), (i+1, j))
        in
    case ((i', j'), (i'', j'')) of
        ((-1, -1), (-1, -1)) -> findMaxPipeDistance (rmIdx maze (i, j)) ls step
        _                    -> do 
            let ls' = ls ++ [(i', j'), (i'', j'')]
            findMaxPipeDistance (rmIdx maze (i, j)) ls' (step + 1)

solve :: IO ()
solve = do
    inp <- readFile "Day10/inp.txt"
    let allLines = lines inp

    let (si, sj) = findStartCoords allLines 0
    let maze = Map.fromList $ zip [0..] $ map (Map.fromList . zip [0..]) allLines
    let dist = findMaxPipeDistance maze [(si, sj)] 0

    print (dist `div` 2)
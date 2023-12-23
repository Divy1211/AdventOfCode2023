module Day11.P1 (solve, findColumnsToExpand, findGalaxies) where

import Data.List (foldl', tails)
import Data.Set (Set)
import qualified Data.Set as Set
import Day9.P1 (allEq)


expandRows :: [String] -> [String]
expandRows []                       = []
expandRows (row : rows) | allEq row = [row, row] ++ expandRows rows
expandRows (row : rows)             = row : expandRows rows

findColumnsToExpand :: [String] -> Set Int
findColumnsToExpand img = do
    let cols = Set.fromAscList [0..length (head img) - 1]
    foldl' (\cols' row ->
        fst $ foldl' (\(cols'', j) c -> do
                let cols''' = case c of
                        '#' -> Set.delete j cols''
                        _   -> cols''
                (cols''', j+1)
            ) (cols', 0) row
        ) cols img

expandCol :: Set Int -> Int -> String -> String
expandCol colsToExpand j ""                                     = ""
expandCol colsToExpand j (c : cols) | Set.member j colsToExpand = [c, c] ++ expandCol colsToExpand (j+1) cols
expandCol colsToExpand j (c : cols)                             = c : expandCol colsToExpand (j+1) cols

expandCols :: Set Int -> [String] -> [String]
expandCols colsToExpand = map (expandCol colsToExpand 0)

performExpansion :: [String] -> [String]
performExpansion img = do
    let colsToExpand = findColumnsToExpand img
    expandCols colsToExpand (expandRows img)

findGalaxiesInRow :: Int -> String -> Set (Int, Int)
findGalaxiesInRow i = fst . foldl' (\(points, j) point -> do
        let points' = case point of
                '#' -> Set.insert (i, j) points
                _   -> points
        (points', j+1)
    ) (Set.empty, 0)

findGalaxies :: [String] -> Set (Int, Int)
findGalaxies = foldl' Set.union Set.empty . zipWith findGalaxiesInRow [0..]

taxiDist :: ((Int, Int), (Int, Int)) -> Int
taxiDist ((x1, y1), (x2, y2)) = abs (x2-x1) + abs (y2-y1)

solve :: IO ()
solve = do
    inp <- readFile "Day11/inp.txt"
    let img = lines inp

    let expanded = performExpansion img
    let galaxyCoords = Set.toList $ findGalaxies expanded
    let pairs = [(coord1, coord2) | (coord1 : coords) <- tails galaxyCoords, coord2 <- coords]
    
    print $ sum $ map taxiDist pairs
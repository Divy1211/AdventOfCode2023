module Day11.P2 (solve) where

import Data.List (tails, foldl')
import Day9.P1 (allEq)
import Day11.P1 ( findColumnsToExpand, findGalaxies)
import Data.Set (Set)
import qualified Data.Set as Set


findRowsToExpand :: [String] -> Set Int
findRowsToExpand = fst . foldl' (\(rows, i) row -> do
        let rows' = if allEq row then Set.insert i rows else rows
        (rows', i+1)
    ) (Set.empty, 0)

countInRange :: (Int, Int) -> Set Int -> Int
countInRange (s, e) = Set.size . Set.filter (\i -> s < i && i < e)

extra :: Int
extra = 1000000-1

taxiDist' :: Set Int -> Set Int -> ((Int, Int), (Int, Int)) -> Int
taxiDist' rows cols ((x1, y1), (x2, y2)) = abs (x2-x1)
                                         + abs (y2-y1)
                                         + extra * countInRange (min x1 x2, max x1 x2) rows
                                         + extra * countInRange (min y1 y2, max y1 y2) cols

solve :: IO ()
solve = do
    inp <- readFile "Day11/inp.txt"
    let img = lines inp

    let galaxyCoords = Set.toList $ findGalaxies img
    let (rows, cols) = (findRowsToExpand img, findColumnsToExpand img)
    let pairs = [(coord1, coord2) | (coord1 : coords) <- tails galaxyCoords, coord2 <- coords]

    print $ sum $ map (taxiDist' rows cols) pairs
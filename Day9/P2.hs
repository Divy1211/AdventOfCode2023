module Day9.P2 (solve) where

import Day9.P1 (parseInts, findDiffSeq)


solve :: IO ()
solve = do
    inp <- readFile "Day9/inp.txt"
    let allLines = lines inp

    let seqs = map (parseInts . (++ ".")) allLines
    let diffs = map (map head . findDiffSeq) seqs

    print $ sum $ map (foldr (-) 0) diffs
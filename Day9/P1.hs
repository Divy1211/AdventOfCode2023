module Day9.P1 (solve, parseInts, findDiffSeq, allEq) where

import Data.Char (isDigit, digitToInt)


parseInts :: String -> [Int]
parseInts = (\(_, ls, _) -> reverse ls) . foldl(\(num, ls, neg) i ->
    case i of
        '-'           -> (0, ls, True)
        x | isDigit x -> (num * 10 + digitToInt x, ls, neg)
        _             -> (0, (if neg then -num else num) : ls, False)
    ) (0, [], False)

calcDiffs :: [Int] -> [Int]
calcDiffs seq = zipWith (-) (tail seq) seq

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq [_] = True
allEq (x1 : x2 : xs) = x1 == x2 && allEq xs

findDiffSeq :: [Int] -> [[Int]]
findDiffSeq [t] = [[t]]
findDiffSeq ts | allEq ts = [ts]
findDiffSeq ts = ts : findDiffSeq (calcDiffs ts)

choose :: Int -> Int -> Int
choose n k
    | k == 0    = 1
    | k > (n `div` 2) = n `choose` (n-k)
    | otherwise = n * ((n-1) `choose` (k-1)) `div` k

nthTerm :: Int -> [Int] -> Int
nthTerm len diffs = sum $ zipWith (*) diffs [len `choose` k | k <- [0..]]

solve :: IO ()
solve = do
    inp <- readFile "Day9/inp.txt"
    let allLines = lines inp

    let seqs = map (parseInts . (++ ".")) allLines
    let diffs = map (map head . findDiffSeq) seqs

    print $ sum $ zipWith nthTerm (map length seqs) diffs

module Day6.P1 (solve, findNumIntsInRootRange, parseDoubles) where

import Data.List (foldl')
import Data.Char (isDigit, digitToInt)


parseDoubles :: String -> [Double]
parseDoubles = snd . foldl(\(num, ls) i -> case i of
        x | isDigit x -> (num * 10 + digitToInt x, ls)
        _ | num /= 0  -> (0, (fromIntegral num :: Double) : ls)
        _             -> (0, ls)
    ) (0, [])

isInt :: Double -> Bool
isInt x = x == fromInteger (round x)

findNumIntsInRootRange :: Double -> Double -> Int
findNumIntsInRootRange t s =
    let d = sqrt (t * t - 4 * s) in
    let (l', u') = ((t-d) / 2, (t+d) / 2) in
    let l | isInt l' = ceiling l'+1
          | otherwise = ceiling l' in
    let u | isInt u' = floor u'-1
          | otherwise = floor u' in
    u-l+1

solve :: IO ()
solve = do
    inp <- readFile "Day6/inp.txt"
    let allLines = lines inp

    let [timesStr, distancesStr] = allLines

    let times     = parseDoubles ((dropWhile (== ' ') . drop 5) timesStr ++ " ")
    let distances = parseDoubles ((dropWhile (== ' ') . drop 9) distancesStr ++ " ")

    let numIntRootRange = zipWith findNumIntsInRootRange times distances

    print (foldl' (*) 1 numIntRootRange)

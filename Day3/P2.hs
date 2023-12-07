module Day3.P2 (solve) where

import Data.List (foldl')
import Data.Char (isDigit, digitToInt)
import qualified Data.Map as Map

isSym :: Char -> Bool
isSym = (== '*')

findGearPosNums :: (Int, (String, String, String)) -> [((Int, Int), Int)] -- pos, num
findGearPosNums (line, (l1, l2, l3)) =
    (\(_, _, _, _, posList) -> posList) (foldl' (\(col, (symLine, symCol), num, wasSym, posList) (c1, c, c2) ->
        let ((symLine', symCol'), symAround)
                | isSym c1  = ((line - 1, col), True)
                | isSym c   = ((line, col), True)
                | isSym c2  = ((line + 1, col), True)
                | wasSym    = ((symLine, symCol), False)
                | otherwise = ((-1, -1), False)
             in
        let (num', wasSym', posList') = case c of
                x   | isDigit x                         -> (num * 10 + digitToInt x, symAround || wasSym, posList)
                '.' | (symAround || wasSym) && num /= 0 -> (0, symAround, ((symLine', symCol'), num) : posList)
                '*' | num /= 0                          -> (0, True, ((line, col), num) : posList)
                _                                       -> (0, symAround, posList)
            in
        (col + 1, (symLine', symCol'), num', wasSym', posList')
    ) (0, (-1, -1), 0, False, []) (zip3 l1 l2 l3))

solve :: IO ()
solve = do
    inp <- readFile "Day3/inp.txt"
    let allLines = lines inp

    let paddedLines = map (++ ".") allLines
    let threeLines = zip3 (repeat '.' : paddedLines) paddedLines (tail paddedLines ++ [repeat '.'])
    let posGearsNums = concatMap findGearPosNums (zip [1..] threeLines)
    let gearNumMap = (Map.fromListWith (\(y1, c1) (y2, c2) -> (y1*y2, c1+c2)) . map (\(x, y) -> (x, (y, 1)))) posGearsNums
    -- Map.fromListWith is O(n log n) ... using a mutable version of Map would give O(n), but mutability...
    -- Also, log n is not too bad in practice, log n = 32 is ~2B entries in the map!

    print (Map.foldl' (\acc (v, c) -> if c == 2 then v + acc else acc) 0 gearNumMap)

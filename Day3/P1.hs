module Day3.P1 (solve) where

import Data.List (foldl')
import Data.Char (isDigit, digitToInt)


isSym :: Char -> Bool
isSym c =  case c of
    '.' -> False
    x | isDigit x -> False
    _ -> True

sumMiddleLine :: (String, String, String) -> Int
sumMiddleLine (l1, l2, l3) = (\(lsum, _, _) -> lsum) (foldl' (\(lsum, num, wasSym) (c1, c, c2) ->
        let symAround = isSym c1 || isSym c2 in
        case c of
            x   | isDigit x           -> (lsum, num * 10 + digitToInt x, wasSym || symAround)
            '.' | wasSym || symAround -> (lsum + num, 0, symAround)
            '.'                       -> (lsum, 0, symAround)
            _                         -> (lsum + num, 0, True)
    ) (0, 0, False) (zip3 l1 l2 l3))

solve :: IO ()
solve = do
    inp <- readFile "Day3/inp.txt"
    let allLines = lines inp

    let paddedLines = map (++ ".") allLines
    let threeLines = zip3 (repeat '.' : paddedLines) paddedLines (tail paddedLines ++ [repeat '.'])
    
    let lineSum = map sumMiddleLine threeLines

    print (sum lineSum)

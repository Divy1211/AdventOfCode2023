module Day4.P1 (solve, parseCards) where

import Data.List (foldl')
import Data.Char (isDigit, digitToInt)
import Data.Set (Set)
import qualified Data.Set as Set


parseCards :: String -> (Set Int, Set Int)
parseCards = (\(winNums, myNums, _, _) -> (winNums, myNums)) . foldl' (\(winNums, myNums, isMyNum, num) c ->
    let (num', winNums', myNums') = case c of
            x | isDigit x           -> (num * 10 + digitToInt c, winNums, myNums)
              | num /= 0 && isMyNum -> (0, winNums, Set.insert num myNums)
              | num /= 0            -> (0, Set.insert num winNums, myNums)
            _                       -> (0, winNums, myNums)
        in
    (winNums', myNums', isMyNum || c == '|', num')
    ) (Set.empty, Set.empty, False, 0)

calcScore :: String -> Int
calcScore str =
    let (winNums, myNums) = parseCards str in
    let commonNums = Set.intersection winNums myNums in
    let numCommonNums = length commonNums in
    
    case numCommonNums of
        0 -> 0
        x -> 2 ^ (x - 1)

solve :: IO ()
solve = do
    inp <- readFile "Day4/inp.txt"
    let allLines = lines inp
    let paddedLines = map ((++ " ") . dropWhile (/= ':')) allLines

    let cardScores = map calcScore paddedLines

    print (sum cardScores)

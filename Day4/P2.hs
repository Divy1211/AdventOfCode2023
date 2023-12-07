module Day4.P2 (solve) where

import qualified Data.Set as Set
import Day4.P1 (parseCards)


calcNumMatches :: String -> Int
calcNumMatches str =
    let (winNums, myNums) = parseCards str in
    let commonNums = Set.intersection winNums myNums in
    length commonNums

updateCounts :: Int -> [(Int, Int)] -> [(Int, Int)]
updateCounts count = map (\(cardMatches, cardCount) -> (cardMatches, cardCount + count))

totalCards :: [(Int, Int)] -> Int
totalCards ((matches, count) : rest) =
    case splitAt matches rest of
        ([], [])             -> count
        ([], rest')          -> count + totalCards rest'
        (cardsToCopy, [])    -> count + totalCards (updateCounts count cardsToCopy)
        (cardsToCopy, rest') -> count + totalCards (updateCounts count cardsToCopy ++ rest')

solve :: IO ()
solve = do
    inp <- readFile "Day4/inp.txt"
    let allLines = lines inp
    let paddedLines = map ((++ " ") . dropWhile (/= ':')) allLines

    let numMatchesCards = map calcNumMatches paddedLines

    print (totalCards (zip numMatchesCards (repeat 1)))

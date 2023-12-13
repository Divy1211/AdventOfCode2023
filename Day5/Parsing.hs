module Day5.Parsing (parseMap, parseSeeds, Range (..), inRange, overlap) where

import Data.List (foldl')
import Data.Char (isDigit, digitToInt)


data Range = Range {
    src :: Int,
    len :: Int,
    dst :: Int
} deriving (Show)

inRange :: Int -> Range -> Bool
inRange seed Range{src=s, len=l, dst=_} = s < seed && seed < (s + l)

overlap :: Range -> Range -> Bool
overlap Range{src=s1, len=l1, dst=_} Range{src=s2, len=l2, dst=_} = s1 < s2 + l2 && s2 < s1 + l1

parseMap :: String -> [Range]
parseMap = map parseMapLine . drop 1 . lines

parseMapLine :: String -> Range
parseMapLine = (\(range, _, _) -> range) . foldl' (\(range, num, whichNum) c ->
    let range' = case whichNum of
            1 -> range {dst = num}
            2 -> range {src = num}
            3 -> range {len = num}
            _ -> range -- semantically unreachable
        in
    case c of
        x | isDigit x     -> (range, num * 10 + digitToInt x, whichNum)
        _ -> (range', 0, whichNum + 1)
    ) (Range {src = 0, len = 0, dst = 0}, 0, 1) . (++ [' '])

parseSeeds :: String -> [Int]
parseSeeds = (snd . foldl(\(num, ls) i -> case i of
        x | isDigit x -> (num * 10 + digitToInt x, ls)
        _ -> (0, num : ls)
    ) (0, [])) . (++ [' ']) . drop 7
module Day5.P2 (solve) where

import Data.List (foldl')
import Data.List.Split (splitOn)
import Day5.Parsing (parseSeeds, Range (..), parseMap, overlap)


seedsToRanges :: [Int] -> [Range]
seedsToRanges ls = case ls of
    [] -> []
    [src', len'] ->     [Range {src = src', len = len', dst = -1}]
    src' : len' : ls -> Range {src = src', len = len', dst = -1} : seedsToRanges ls

mapAndCut :: Range -> Range -> ([Range], [Range])
mapAndCut mappingRange seed =
    if overlap mappingRange seed
    then
        let (Range{src=mStart, len=mLen, dst=dest}, Range{src=sStart, len=sLen, dst=_}) = (mappingRange, seed) in
        let (mEnd, sEnd) = (mStart + mLen, sStart + sLen) in
        let (mapped, remaining)
                | mStart <= sStart && sEnd <= mEnd = ([(dest + sStart - mStart, sLen)], [])
                | mStart <= sStart && mEnd < sEnd = ([(dest + sStart - mStart, mEnd - sStart)], [(mEnd, sEnd - mEnd)])
                | sStart < mStart && mEnd < sEnd = ([(dest, mLen)], [(sStart, mStart - sStart), (mEnd, sEnd - mEnd)])
                | sStart < mStart && sEnd <= mEnd = ([(dest, sEnd - mStart)], [(sStart, mStart - sStart)])
        in
        let constructRange = (\(x, y) -> Range{src = x, len = y, dst = -1}) in
        (map constructRange mapped, map constructRange remaining)
    else ([], [seed])

applyMapToRanges :: [Range] -> [Range] -> ([Range], [Range])
applyMapToRanges map' seedRanges = foldl'(\(mapped, remaining) mappingRange ->
        let mappedAndRemaining = map (mapAndCut mappingRange) remaining in
        foldl' (\(allMapped, allRemaining) (mapped', remaining') ->
                (mapped' ++ allMapped, remaining' ++ allRemaining)
            ) (mapped, []) mappedAndRemaining
    ) ([], seedRanges) map'

applyMapsToRanges :: [[Range]] -> [Range] -> [Range]
applyMapsToRanges maps seedRanges = uncurry (++) (foldl' (\(mapped, remaining) map' ->
        applyMapToRanges map' (mapped ++ remaining)
    ) ([], seedRanges) maps)

solve :: IO ()
solve = do
    inp <- readFile "Day5/inp.txt"
    let (seedsLine : mapLines) = splitOn "\n\n" inp

    let seedRanges = seedsToRanges (reverse (parseSeeds seedsLine))
    let maps = map parseMap mapLines

    let mapped = applyMapsToRanges maps seedRanges

    print (minimum (map src mapped))

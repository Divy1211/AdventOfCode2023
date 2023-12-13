module Day7.P1 (solve) where

import Data.List (sort)
import Day7.Hand1 (Hand (..))
import qualified Day7.Hand1 as Hand


handBid :: String -> (Hand, Int)
handBid str = do
    let (hand, bid) = splitAt 6 str
    (Hand.fromStr hand, read bid :: Int)

solve :: IO ()
solve = do
    inp <- readFile "Day7/inp.txt"
    let allLines = lines inp

    let handBids = map snd $ sort $ map handBid allLines
    let winnings = sum $ zipWith (*) handBids [1..]

    print winnings
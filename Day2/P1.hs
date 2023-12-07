module Day2.P1 (solve, parseGame, Game (num, maxOcc), Occurrence (r, g, b)) where

import Data.List (foldl')
import Data.Char (isDigit, digitToInt)


data Occurrence = Occurrence {
   r :: Int,
   g :: Int,
   b :: Int
}

data Game = Game {
    num :: Int,
    maxOcc :: Occurrence
}

maxOccurrence :: String -> Occurrence
maxOccurrence = snd . foldl' (\(acc, occ) c ->
    case c of
        x | isDigit x -> ((acc * 10) + digitToInt x, occ)
        
        'r' -> (0, occ {r = max acc (r occ)})
        'g' -> (0, occ {g = max acc (g occ)})
        'b' -> (0, occ {b = max acc (b occ)})
        _   -> (acc, occ)
    ) (0, Occurrence {r = 0, g = 0, b = 0})

parseGame :: String -> Game
parseGame str = let (gameIdx, rest) = span isDigit str in Game {
    num = read gameIdx :: Int,
    maxOcc = maxOccurrence rest
}

solve :: IO ()
solve = do
    inp <- readFile "Day2/inp.txt"
    let allLines = lines inp
    let (maxR, maxG, maxB) = (12, 13, 14)
    
    let games = map (parseGame . drop 5) allLines
    let possibleGames = map num (filter (\game -> let m = maxOcc game in (r m <= maxR) && (g m <= maxG) && (b m <= maxB)) games)

    print (sum possibleGames)
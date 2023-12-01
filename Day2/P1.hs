module Day2.P1 (solve, firstDigitR, getNum) where

import Data.Char (isDigit, isAlpha)

firstDigitR :: String -> Char
firstDigitR = foldr (\i acc ->
        if isDigit acc
            then acc
            else if isDigit i
                then i
                else acc ) 'a'

getNum :: (Char, String) -> Int
getNum (dig, str) = read (dig : [firstDigitR str]) :: Int


solve :: IO ()
solve = do
    inp <- readFile "Day2/inp.txt"
    let allLines = lines inp
    
    let nums = map (getNum . (\str -> (head str, str)) . dropWhile isAlpha) allLines
    print (sum nums)

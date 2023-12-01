module Day1.P1 (solve, firstDigitR, getNum) where

import Data.Char (isDigit, isAlpha)

firstDigitR :: String -> Char
firstDigitR =
    foldr (\i acc ->
        if isDigit acc
            then acc
            else if isDigit i
                then i
                else acc
    ) 'a' -- we are guaranteed a number in the input strs, this will never return 'a'

getNum :: String -> Int
getNum str = read (head str : [firstDigitR str]) :: Int


solve :: IO ()
solve = do
    inp <- readFile "Day1/inp.txt"
    let allLines = lines inp
    
    let nums = map (getNum . dropWhile isAlpha) allLines
    print (sum nums)

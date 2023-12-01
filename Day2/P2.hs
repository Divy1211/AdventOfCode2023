module Day2.P2 (solve) where

import Data.List (isPrefixOf)
import Data.Char (isDigit)

firstDigitL :: String -> Char
firstDigitL str = do
    let tailStr = tail str
    case head str of
        x | isDigit x -> x
        'o' | "ne" `isPrefixOf` tailStr -> '1'
        't' | "wo" `isPrefixOf` tailStr -> '2'
        't' | "hree" `isPrefixOf` tailStr -> '3'
        'f' | "our" `isPrefixOf` tailStr -> '4'
        'f' | "ive" `isPrefixOf` tailStr -> '5'
        's' | "ix" `isPrefixOf` tailStr -> '6'
        's' | "even" `isPrefixOf` tailStr -> '7'
        'e' | "ight" `isPrefixOf` tailStr -> '8'
        'n' | "ine" `isPrefixOf` tailStr -> '9'
        _ -> firstDigitL tailStr

firstDigitR :: String -> Char
firstDigitR str = do
      let headStr = tail str
      case head str of
          x | isDigit x -> x
          'e' | reverse "on" `isPrefixOf` headStr -> '1'
          'o' | reverse "tw" `isPrefixOf` headStr -> '2'
          'e' | reverse "thre" `isPrefixOf` headStr -> '3'
          'r' | reverse "fou" `isPrefixOf` headStr -> '4'
          'e' | reverse "fiv" `isPrefixOf` headStr -> '5'
          'x' | reverse "si" `isPrefixOf` headStr -> '6'
          'n' | reverse "seve" `isPrefixOf` headStr -> '7'
          't' | reverse "eigh" `isPrefixOf` headStr -> '8'
          'e' | reverse "nin" `isPrefixOf` headStr -> '9'
          _ -> firstDigitR headStr


solve :: IO ()
solve = do
    inp <- readFile "./Day2/inp.txt"
    let allLines = lines inp

    let firstDigs = map firstDigitL allLines
    let secondDigs = map (firstDigitR . reverse) allLines

    let nums = zipWith (\x y -> read (x : [y]) :: Int) firstDigs secondDigs
    print (sum nums)
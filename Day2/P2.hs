module Day2.P2 (solve) where

import Day2.P1 (parseGame, Game (num, maxOcc), Occurrence (r, g, b))


solve :: IO ()
solve = do
    inp <- readFile "Day2/inp.txt"
    let allLines = lines inp

    let games = map (parseGame . drop 5) allLines
    let powerGames = map (\game -> let m = maxOcc game in r m * g m * b m) games

    print (sum powerGames)

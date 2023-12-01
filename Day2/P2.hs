module Day2.P2 (solve) where


solve :: IO ()
solve = do
    inp <- readFile "./Day2/inp.txt"
    let allLines = lines inp

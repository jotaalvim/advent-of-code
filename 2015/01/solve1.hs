count :: Int -> Char -> Int
count n '(' = n + 1
count n ')' = n - 1
count n _   = n 

parte1 :: String -> Int
parte1 = foldl count 0 

base :: Int -> String -> Int
base n [] = 0
base n (p:t) 
    | n < 0 = 0
    | p == '(' = 1 + base (n+1) t
    | p == ')' = 1 + base (n-1) t

parte2 :: String -> Int 
parte2 = base 0 

main = do
    input <- readFile "input1.txt"
    putStrLn $ "Parte 1: " ++ show (parte1 input) 
    putStrLn $ "Parte 2: " ++ show (parte2 input) 


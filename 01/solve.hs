
part1 :: [Int] -> Int 
part1 l = sum [ 1 | (a,b) <- zip l $ tail l , a < b ] 

part2 :: [Int] -> Int
part2 = part1 . sum3

sum3 :: [Int] -> [Int]
sum3 (a:b:c:t) = a+b+c : sum3 (b:c:t)
sum3 _ = []

main :: IO ()
main = do
    input <- fmap (read :: String -> Int ) <$> lines <$> readFile "input.txt"
    --input <- readFile "input.txt"
    --let ln = lines input
    --    lf = map (read :: String -> Int) ln
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )

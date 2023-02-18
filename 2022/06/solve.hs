import Data.List

different :: Eq a => [a] -> Bool
-- different l = nub l == l
different = nub >>= (==)

count :: Int -> String -> Int
count n l 
    | different j = n
    | True        = 1 + count n (tail j ++ b)
    where (j,b) = splitAt n l
 
part1 = count 4

part2 = count 14

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )

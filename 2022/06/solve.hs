import Data.List

--Prelude Pointfree> pointfree' "different l = nub l == l"
--Just "different = (==) =<< nub"

different :: Eq a => [a] -> Bool
different l = nub l == l

count :: String -> Int -> Int
count l n
    | different (a:as) = n
    | otherwise   = 1 + count (as ++ b) n
    where (a:as,b) = splitAt n l

part1 = flip count 4

part2 = flip count 14

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )

import Data.List.Split

type Interval = (Int,Int)

type Assignment = (Interval,Interval)

type Ids = [Assignment]

sample :: String
sample = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"

-- part1 

overlap :: Assignment -> Bool
overlap ((a,b),(c,d)) = (a <= c && b >= d) || (c <= a && d >= b)

count = length . filter (==True)

part1 :: Ids -> Int
part1 = count . map overlap

-- part2

overlap2 :: Assignment -> Bool
overlap2 ((a,b),(c,d)) = (c <= b && c >= a) || (d <= b && d >= a) || overlap ((a,b),(c,d))


part2 :: Ids -> Int
part2 = count . map overlap2

-- parsing

readAss :: [[String]] -> Assignment
readAss [[x1,x2],[y1,y2]] = (
        (read x1 :: Int, 
         read x2 :: Int),
        (read y1 :: Int, 
         read y2 :: Int))

parseInput :: String -> Ids
parseInput = map (readAss . map (splitOn "-")  . splitOn ",") . lines

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )

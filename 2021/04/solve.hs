import Data.List.Split
import Data.List

type Bingo = [[Int]] 

parse :: [String] -> ([Int],[Bingo])
parse (l:t) = (pl,takes5 t)
    where pl = map read $ splitOn "," l

takes5 :: [String] -> [Bingo]
takes5 [] = []
takes5 (a:b:c:d:e:t) = bingo : takes5 t
    where 
        bingo = [ map read $ words line | line <- [a,b,c,d,e]] :: Bingo

bingo :: Bingo -> [Int] -> Bool
bingo b bl = any (line bl) b || any (line bl) (transpose b)

line :: [Int] -> [Int] -> Bool
line bn l = all (\x -> elem x bn) l

sumBingo :: Bingo -> [Int] -> Int
sumBingo b l = sum ll * last l
    where ll = filter (\x -> notElem x l) $ concat b 
          
f :: Bingo -> [Int] -> Int
f b l | bingo b l = sumBingo b l | otherwise = 0

filterBingo :: [Bingo] -> [Int] -> Int -> Int
filterBingo bs l n
    | null fff  = filterBingo bs l (n+1)
    | otherwise = head fff
    where 
        fff = filter (/=0) $  map (\bin->f bin (take n l) ) bs

part1 :: ([Int],[Bingo]) -> Int 
part1 (bl,lbin) = filterBingo lbin bl 0


     
filterNotBingo :: [Bingo] -> [Int] -> Int -> Int
filterNotBingo bs l n
    | not $ null fff  = filterNotBingo fff l (n+1)
    | otherwise = head fff
    where 
        fff = filter (==0) $  map (\bin -> f bin (take n l) ) bs

part2 :: ([Int],[Bingo]) -> Int 
part2 (bl,lbin) = filterNotBingo lbin bl 0

main :: IO ()
main = do 
    input<- parse <$> filter (not.null) <$> lines <$> readFile "input.txt"
    putStrLn $ "Part 1: "++ show (part1 input)
    putStrLn $ "Part 1: "++ show (part2 input)

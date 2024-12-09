import Data.List.Split
import Data.List

--import Text.Regex.Posix

import Cp

parseInput = id 
           . filter (not . null) 
           . splitOn "\n"

data Direction = U | D | L | R deriving (Show, Eq,Ord)

rotate U = R
rotate D = L
rotate L = U
rotate R = D

forward (x,y) U = (x  , y-1)
forward (x,y) D = (x  , y+1)
forward (x,y) L = (x-1, y  )
forward (x,y) R = (x+1, y  )


getC c m = [ (x,y) | x <- [0.. length (head m) -1] 
                   , y <- [0.. length       m  -1] 
                   , m !! y !! x == c ]

move d p a cols rows walls
    | nx > cols - 1 || nx < 0 || ny > rows - 1 || ny < 0 = (p:a)
    | otherwise = move nextd nextp (p:a) cols rows walls
    where  
        pn   = forward p d
        (nextp@(nx,ny), nextd) = if elem pn walls then (p, rotate d) else ( pn, d)

part1 m = length $ nub $ move U i [] cols rows walls
    where cols  = length $ head  m
          rows  = length          m
          i     = head $ getC '^' m
          walls =        getC '#' m

-- the worst case you have to pass 2 times on the same position to be a loop
loop (a:b:t) = a == b || loop t
loop _ = False

-- does not terminate? return True if it halts
termination d p a cols rows walls
    | nx > cols - 1 || nx < 0 || ny > rows - 1 || ny < 0 = False
    | otherwise = loop newacc || termination nextd nextp newacc cols rows walls
    where  
        newacc = insert  (p,d) a
        pn     = forward p d
        (nextp@(nx,ny), nextd) = if elem pn walls then (p, rotate d) else ( pn, d)


--length $ filter id $ 
--part2 m = length [ (x,y) | x <- [0..cols-1], y <-[0..rows-1], (x,y) /= i, not (elem (x,y) walls) ]
part2 m = length $ filter id [ termination U i [] cols rows ((x,y):walls) | x <- [0..cols-1], y <-[0..rows-1], (x,y) /= i, not (elem (x,y) walls) ]
--part2 m = length $ filter id [ termination U i [] cols rows ((x,y):walls) | x <- [0..cols-1], y <-[0..rows-1]]
    where cols  = length $ head m
          rows  = length        m
          i     = head $ getC '^' m
          walls =        getC '#' m

       

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input)
    putStrLn $ "Part 2: " ++ show ( part2 input)

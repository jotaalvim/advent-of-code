import Data.List.Split
import Data.List hiding (insert)
import Data.Map (Map)
import qualified Data.Map as Map

infixr 8 ><
(f >< g) (x,y) = (f x,g y)
dup a = (a,a)

------ Parsing ----------------------------------------------------------------

parseS = head . elemIndices 'S' . head 
parseT = filter (not.null) . map ( elemIndices '^' )

parseInput :: String -> (Int, [[Int]])
parseInput = (parseS >< parseT) . dup . lines

-------------------------------------------------------------------------------
-- hit splitter
hitS :: [Int] -> Int -> [Int]
hitS l i | elem i l  = [ i - 1 , i + 1]
         | otherwise = [ i ]

cascade :: [[Int]] -> [Int] -> Int -> Int
cascade []            beams acc = acc
cascade (s:sps) beams acc = cascade sps newBeams (acc + hits)
    where newBeams = nub    $ concatMap (hitS s) beams
          hits     = length $ intersect beams s

part1 (s,m) = cascade m [s] 0

-------------------------------------------------------------------------------
-- got inspiration from this visualization
-- https://www.reddit.com/r/adventofcode/comments/1pgnmou/2025_day_7_lets_visualize/
quantumHit l (k,v) | elem k l  = [(k-1,v), (k+1,v) ]
                   | otherwise = [(k,v) ]

quantumCascade :: [[Int]] -> Map Int Int -> Int
quantumCascade []      beams = sum $ Map.elems beams
quantumCascade (s:sps) beams = quantumCascade sps newBeams
    where nextBeams  = concatMap (quantumHit s) $ Map.toList beams
          newBeams   = Map.fromListWith(+) nextBeams

part2 (s,m) = quantumCascade m initial
    where initial = Map.insert s 1 Map.empty
         
-------------------------------------------------------------------------------

main :: IO ()
main = do
    input <- parseInput <$> readFile "inputExample.txt"
    --input <- parseInput <$> readFile "input.txt"

    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )


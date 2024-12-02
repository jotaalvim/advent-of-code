import Data.List.Split
import Data.List
import Cp

removeNth :: Int -> [] a -> [] a
removeNth = \n -> \list ->
      case n of 
          0 -> tail list
          otherwise -> head list: removeNth (n-1) (tail list)


type Report = [ Int ] 

parseReport :: String -> Report
parseReport = map read . words

parseInput =  map parseReport
           . filter (not . null) 
           . splitOn "\n"

-- remove 2 elements ar a time and if it passes in the traversal


traverseR  = all (uncurry safe) . uncurry zip . (id >< tail) . dup

safe n1 n2 = n2 - n1 <= 3 && n2 > n1 

part1 = length . filter safeReport

safeReport r  = traverseR r || traverseR (reverse r) 

safeReport2 r = traverseR r || traverseR (reverse r) || any traverseR newR || any traverseR (map reverse newR)
    where
        newR = genP r

genP l = [ removeNth n l | n <- [0.. length l-1]]

part2 = length . filter safeReport2

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )

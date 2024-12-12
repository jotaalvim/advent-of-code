import Data.List.Split
import Data.List
import Data.Char

parseInput = map read . words . head 
           . filter (not . null) 
           . splitOn "\n"

rules n | n == 0    = [ 1 ]
        | even size = [ read lh, read rh]
        | otherwise = [ n*2024 ]
    where string  = show n
          size    = length string
          (lh,rh) = splitAt (div size 2) string

blink = (=<<) rules
part1 = length . (!! 25) . iterate blink 

insertM n e  [] = [ (e,n) ]
insertM n e  ((e2,n2):t) 
    | e == e2   = (e, n+n2) : t
    | otherwise = (e2,n2  ) : insertM n e  t

makeNew []         a = a
makeNew ((e,n):t)  a = makeNew t $ foldr (insertM n) a (rules e)

part2 = sum . map snd . (!! 75) . iterate (flip makeNew []) . foldr (insertM 1) []

main :: IO ()
main = do
    input  <- parseInput  <$> readFile "inputExample.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )

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

runN f a 0 = a
runN f a n = runN f (f a) (n-1)
            
part2 l = sum $ map snd $ runN (flip makeNew []) i 75
    where i = foldr (insertM 1) [] l

main :: IO ()
main = do
    input  <- parseInput  <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )

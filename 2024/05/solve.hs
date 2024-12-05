import Data.List.Split
import Data.List
import Text.Regex.Posix
import Cp

parseUpdates = map (\x -> read $ "[" ++ x ++ "]")
             . filter (not . null) 
             . splitOn "\n"

parseRules = map ( (read >< read) . mktuple . splitOn "|")
           . splitOn "\n"

parseInput = (parseRules >< parseUpdates)
           . mktuple
           . filter (not . null) 
           . splitOn "\n\n"

verify []    _ = True
verify (u:v) r = notFront (getAll u r) v && verify v r

getAll x r = [ a | (a,b) <- r , x == b]

-- ninguém da lista da esquerda pode estar na lista da direita
notFront l l2  = all (not . flip elem l2 ) l

middle [m] = m
middle l   = middle $ tail $ init l

genUp i x l = p ++ [x] ++ f where (p,f) = splitAt i l
genMUp x l  = [ genUp i x l | i <- [0..length l -1] ]

filterUp f r = filter (f . flip verify r)

bubble1 []       r = []
bubble1 [a]      r = [a]
bubble1 (x:y:zs) r = if verify [x,y] r
                     then x : bubble1 (y:zs) r
                     else y : bubble1 (x:zs) r

bubbleAll l r = if verify l r 
                then l 
                else bubbleAll ( bubble1 l r ) r

part1 = (sum . map middle) ... (filterUp id)

part2 r = sum . map (middle . flip bubbleAll r) . filterUp not r
        

main :: IO ()
main = do
    (r,u) <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 r u)
    putStrLn $ "Part 2: " ++ show ( part2 r u)

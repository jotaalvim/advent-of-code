import Data.List.Split
import Data.List

data Result = Win | Loss | Draw deriving(Eq,Show)

data Choice = B | A | C | X | Y | Z deriving(Eq,Show)

win :: Choice -> Choice -> Bool
win a b = vs a b == Win

lost :: Choice -> Choice -> Bool
lost a b = vs a b == Loss

draw :: Choice -> Choice -> Bool
draw a b = vs a b == Draw

-- versus
vs :: Choice -> Choice -> Result
vs A Z = Loss
vs A Y = Win
vs A X = Draw
vs B X = Loss
vs B Z = Win
vs B Y = Draw
vs C Y = Loss
vs C X = Win
vs C Z = Draw

-- read Choice, string to choice
readC :: String -> Choice
readC "A" = A
readC "B" = B
readC "C" = C
readC "X" = X
readC "Y" = Y
readC "Z" = Z

points :: Choice -> Int
points X = 1
points Y = 2
points Z = 3
--X A = 1 rock
--Y B = 2 paper
--Z C = 3 scissors

play :: Choice -> Choice -> Int
play a b
    | win  a b = points b + 6
    | lost a b = points b + 0
    | draw a b = points b + 3
-- win  = 6 points
-- loss = 0 points
-- draw = 3 points

part1 :: [(Choice,Choice)] -> Int
part1 = sum . map (uncurry play) 


-- Y = Draw
-- X = Loss
-- Z = Win 
move :: Choice -> Choice -> Choice
move A X = Z
move A Y = X
move A Z = Y
move B X = X 
move B Y = Y
move B Z = Z
move C X = Y
move C Y = Z
move C Z = X

replace :: (Choice,Choice) -> (Choice,Choice)
replace (a,b) = (a , move a b)
--replace  =  split p1 (uncurry move) 

part2 :: [(Choice,Choice)] -> Int
part2 = part1 . map replace 


-- parsing

evens xs = [x | (x,i) <- zip xs [0..], even i]
odds  xs = [x | (x,i) <- zip xs [0..], odd i]

parseInput :: String -> [(Choice,Choice)]
parseInput s = zip (evens l) (odds l)
    where l  = concat $ map (map readC) $ map words $ lines s

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )

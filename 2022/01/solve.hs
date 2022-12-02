import Data.List.Split
import Data.List

type Cal = Int
type Elf = [Cal]
type Elves = [Elf]

biggestElf :: Elves -> Elf
biggestElf [e] = e
biggestElf (e1:t) = maxElf e1 $ biggestElf t

maxElf :: Elf -> Elf -> Elf
maxElf a b 
    | sum a > sum b = a
    | otherwise = b

part1 :: Elves -> Cal
part1 = sum . biggestElf

part2 :: Elves -> Cal
part2 l = sum e1 + sum e2 + sum e3
    where 
        e1 = biggestElf l
        l2 = delete e1 l
        e2 = biggestElf l2
        l3 = delete e2 l2
        e3 = biggestElf l3

-- parsing

parseInput :: String -> Elves
parseInput = map ( map read ) . map lines . splitOn "\n\n"

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )


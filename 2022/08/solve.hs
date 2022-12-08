import Data.Char
--import Data.List.Split
--https://williamyaoh.com/posts/2019-04-11-cheatsheet-to-regexes-in-haskell.html
--import Text.Regex.TDFA

-- NA = not appliable

data State = Visible | NotVisible | NA deriving(Eq,Show)

data Tree = Tree State Int deriving(Eq,Show)

-- part1

break m x y = 
    where splitAt y m 

part1 :: [[Tree]] -> Int
part1 = undefined

-- part2

-- parsing

initTree :: Char -> Tree
initTree c = Tree NA $ digitToInt c

parseInput :: String -> [[Tree]]
parseInput = map ( map initTree ) . lines

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    print input
    --putStrLn $ "Part 1: " ++ show ( part1 input )
    --putStrLn $ "Part 2: " ++ show ( part2 input )


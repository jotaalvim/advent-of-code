import Data.List.Split
import Data.List
import Text.Regex.Posix
import Cp

example  = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
example2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

mktuple [a,b] = (a,b)

matches   e = e =~  "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)" :: [[String]]
matchesDo e = e =~ "do\\(\\).*"  :: [[String]]

prepare l   = "do()" ++ l 

parseInput2 = parseInput $
              concat . concat . concat
            . map matchesDo 
            . splitOn "don't()"
            . prepare

parseInput f = map ( mktuple . map read . tail)
           . matches
           . f
           . concat
           . filter (not . null) 
           . splitOn "\n"


part1 = sum . map mul
part2 = part1

main :: IO ()
main = do
    input  <- parseInput id  <$> readFile "input.txt"
    input2 <- parseInput2    <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input2 )

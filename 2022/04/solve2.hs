--https://williamyaoh.com/posts/2019-04-11-cheatsheet-to-regexes-in-haskell.html
import Text.Regex.TDFA
--import Text.Regex.TDFA.Text ()

type Interval = (Int,Int)

type Assignment = (Interval,Interval)

type Ids = [Assignment]

sample :: String
sample = "222-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"

-- part1 

contains:: Assignment -> Bool
contains((a,b),(c,d)) = (a <= c && b >= d) || (c <= a && d >= b)

count = length . filter (==True)

part1 :: Ids -> Int
part1 = count . map overlap

-- part2

overlap :: Assignment -> Bool
overlap ((a,b),(c,d)) = (c <= b && c >= a) || (d <= b && d >= a) || contains((a,b),(c,d))


part2 :: Ids -> Int
part2 = count . map overlap

-- parsing

pairs l = zip (evens l) (odds l)
--pairs = uncurry zip . split evens odds

evens xs = [x | (x,i) <- zip xs [0..], even i]
odds  xs = [x | (x,i) <- zip xs [0..], odd i]

parseInput :: String -> Ids
parseInput s = pairs $ pairs n
    where n = map (read :: String -> Int ) (getAllTextMatches (s =~ "[0-9]+") :: [String])


main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    print input
    --putStrLn $ "Part 1: " ++ show ( part1 input )
    --putStrLn $ "Part 2: " ++ show ( part2 input )

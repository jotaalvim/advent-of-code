import Data.List.Split
import Data.List
import Cp

mktpl [a,b] = ([a],[b])

combineT (l,r) (a,b) = (l++a,r++b)

parseNumber :: String -> ( LocationId , LocationId ) 
parseNumber = mktpl . map read . words

parseInput :: String -> ( LocationId , LocationId )
parseInput = foldl combineT ([],[]) 
           . map parseNumber 
           . filter (not . null) 
           . splitOn "\n"


type LocationId = [ Int ]


part1 :: ( LocationId, LocationId) -> Int
part1 = sum . map ( abs . uncurry (-)) . uncurry zip . (sort >< sort)


fetch x = length . filter (==x)

countA (l, r) = sum $ map (\x -> x * fetch x r) l

part2 = countA

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )

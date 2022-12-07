--import Data.Map (Map)
--import qualified Data.Map as Map
--import Data.List.Split
--https://williamyaoh.com/posts/2019-04-11-cheatsheet-to-regexes-in-haskell.html
--import Text.Regex.TDFA

--parseInput :: String -> Ship
parseInput  = map parseMove . map odds . map words . lines . (!! 1) . splitOn "\n\n"

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input s)
    putStrLn $ "Part 2: " ++ show ( part2 input s)


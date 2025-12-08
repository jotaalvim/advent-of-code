import Data.List.Split
import Data.List

infixr 8 ...
infixr 8 ><

(...) = (.).(.)  -- is the same as .:
(f >< g) (x,y) = (f x,g y)
dup a = (a,a)

------ Parsing ----------------------------------------------------------------

parseInput :: String -> [String]
parseInput = filter (not . null) . splitOn "\n"

-------------------------------------------------------------------------------

biggest :: String -> Int
biggest = head . flip concatMap "9876543210" . flip elemIndices  

getBigIndeces :: String -> [Int]
getBigIndeces i 
    | b == length i -1 = [ biggest $ init i ] ++ [ b ] -- the last digit is the biggest one
    | otherwise = [b] ++ [  succ b + ( biggest $ drop (succ b) i) ]
    where b = biggest i


part1 = sum . map read 
      . map (\(a,b) -> map (\e -> b !! e) a ) 
      . map ( getBigIndeces >< id ) 
      . map dup

part2 = id

-------------------------------------------------------------------------------

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    -- putStrLn $ "Part 2: " ++ show ( part2 input )


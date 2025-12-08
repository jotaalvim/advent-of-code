import Data.List.Split
import Data.List

infixr 8 ><
(f >< g) (x,y) = (f x,g y)
dup a = (a,a)

------ Parsing ----------------------------------------------------------------

parseInput :: String -> [String]
parseInput = filter (not . null) . splitOn "\n"

-------------------------------------------------------------------------------

-- hardcoded solution for 2 digits  used in part1
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
--part1 = sum . map read . map (generalSolution 2 "")

generalSolution :: Int -> String -> String -> String
generalSolution 0 a l = a
generalSolution n a l = generalSolution (n-1) (a ++ [big] ) (drop (succ ind ) l)
    where big = maximum $ take (length l-n+1) l
          (Just ind) = elemIndex big l

part2 = sum . map read . map (generalSolution 12 "")
 

-------------------------------------------------------------------------------

main :: IO ()
main = do
    input <- parseInput <$> readFile "inputExample.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )


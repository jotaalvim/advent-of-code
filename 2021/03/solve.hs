import Data.Char 

bin2Int ::  String  -> Int
bin2Int = foldl (\acc x -> acc * 2 + digitToInt x) 0

count :: String -> Char
count =  chose . foldl (count2) (0,0)
    where 
        count2 (a,b) '1' = (a,b+1)
        count2 (a,b) '0' = (a+1,b)

chose :: (Int,Int) -> Char
chose (a,b) | a <= b = '1' | otherwise = '0' 

rep :: Char -> Char
rep '0' = '1'
rep '1' = '0'

gamma :: [String] -> String
gamma (h:t) = [ count $ map (!!k) (h:t) | k <- [0..(length h) -1 ]]

epsilon :: String -> String 
epsilon = map rep

part1 :: [String] -> Int
part1 i = e*g
    where
        ga = gamma i
        g = bin2Int ga 
        e = bin2Int $ epsilon ga

rating :: [String] -> Int -> String
rating [x] _ = x
rating s n = rating (filter ( (s!!n) == $ !! n ) s) (n+1)
    where g = gamma s


main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    --putStrLn $ "Part 2: " ++ show ( part2 input )

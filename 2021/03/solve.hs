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

epsilon :: [String] -> String 
epsilon = map rep . gamma

part1 :: [String] -> Int
part1 i = e*g
    where
        g = bin2Int $ gamma i
        e = bin2Int $ epsilon i

rating :: [String] -> ([String] -> String) -> Int -> String
rating [x] f _ = x
rating s f n = rating (filter ((==((f s)!!n)) . (!!n)) s) f (n+1)

part2 :: [String] -> Int
part2 i = o * co2
    where 
        o   = bin2Int $ rating i   gamma 0 
        co2 = bin2Int $ rating i epsilon 0 

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )


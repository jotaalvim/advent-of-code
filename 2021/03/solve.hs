bin2Int :: Int ->  String  -> Int
bin2Int _ ""      = 0
bin2Int a ('1':t) = 2^a + bin2Int (a+1) t
bin2Int a (_:t)   =       bin2Int (a+1) t

finalB = bin2Int 0 . reverse 

count :: String -> (Int,Int)
count ""      = (0  ,0)
count ('0':t) = (1+p,q) where (p,q) = count t
count ('1':t) = (p,1+q) where (p,q) = count t

chose :: (Int,Int) -> Char
chose (a,b) | a < b = '1' | otherwise = '0' 

rep :: Char -> Char
rep '0' = '1'
rep '1' = '0'

gamma :: [String] -> String
gamma (h:t) = [ chose $ count $ map (!!k) (h:t) | k <- [0..(length h) -1 ]]

epsilon :: String -> String 
epsilon = map rep

part1 :: [String] -> Int
part1 i = e*g
    where
        ga = gamma i
        g = finalB ga 
        e = finalB $ epsilon ga


main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"

    putStrLn $ "Part 1: " ++ show ( part1 input )
    --putStrLn $ "Part 2: " ++ show ( part2 input )

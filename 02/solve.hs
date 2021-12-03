vector :: (Int,Int) -> (String,Int) -> (Int,Int)
vector (h,d) ("forward",x) = (h+x,d)
vector (h,d) ("down",   x) = (h,d+x)
vector (h,d) ("up",     x) = (h,d-x)

vector2 :: (Int,Int,Int) -> (String,Int) -> (Int,Int,Int)
vector2 (h,d,a) ("forward",x) = (h+x,d+a*x,  a)
vector2 (h,d,a) ("down",   x) = (h  ,d    ,a+x)
vector2 (h,d,a) ("up",     x) = (h  ,d    ,a-x)

parte1 :: [(String,Int)] -> Int
parte1 l = h*d
    where (h,d) = foldl vector (0,0) l

parte2 :: [(String,Int)] -> Int
parte2 l = h*d
    where (h,d,_) = foldl vector2 (0,0,0) l

main = do 
    input <- 
            fmap (\[a,b] -> (a,read b :: Int)) 
        <$> fmap (words) 
        <$> lines 
        <$> readFile "input.txt"

    putStrLn $ "Parte 1: "++ show (parte1 input)
    putStrLn $ "Parte 2: "++ show (parte2 input)

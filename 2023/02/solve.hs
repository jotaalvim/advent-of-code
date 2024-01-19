{-# LANGUAGE TupleSections #-}
import Cp 
import Data.List.Split
import Data.List

-- parsing

toInt :: String -> Int 
toInt = read 

-- constansts
z = (0,0)   
colors = ["blue","green","red"]

mk2 [a,b] = (a,b)

mkTuple [a,b,c] = (c,b,a)

dif = map ((0,)) . (colors \\ ) . map snd

parseInput =  map ( id     >< (map ( mkTuple . map (fst) . sortOn snd) )) 
            . map ( id     >< (map ( conc . (id >< dif) . dup ) ) )
            . map ( id     >< (map ( map ( toInt >< id ))))
            . map ( id     >< (map ( map mk2) ))
            . map ( toInt  >< (map ( map words . splitOn ",")) )
            . map ( \[a,b] -> ( last ( words a) ,splitOn ";" b )) 
            . map ( splitOn ":" ) 
            . splitOn "\n"

predicate (r,g,b) = r <= 12 && g <= 13 && b <= 14

maxC (a,b,c) (d,e,f) = (max a d, max b e, max c f)
mulC (a,b,c) = a*b*c

returnR = sum . map fst

part1 = sum . map fst . filter (  (all predicate) . snd )
part2 = sum . map ( snd . (id >< (mulC . (foldl maxC (0,0,0)))))

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )

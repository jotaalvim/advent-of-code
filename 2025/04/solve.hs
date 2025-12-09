import Data.List.Split
import Data.List

infixr 8 ><
(f >< g) (x,y) = (f x,g y)
dup a = (a,a)

------ Parsing ----------------------------------------------------------------

parseInput :: String -> [String]
parseInput = filter (not . null) 
           . splitOn "\n"

-------------------------------------------------------------------------------

update m (x,y) = b ++ [b2 ++ ['x'] ++ rs] ++ r
    where (b,line:r) = splitAt y m
          (b2,_ :rs) = splitAt x line

changeMap = foldl update

eight width height (x,y) = 
    [ (i,j) | (i,j) <- [(x-1,y),(x+1,y),(x,y+1),(x,y-1),(x-1,y-1),(x+1,y-1),(x-1,y+1),(x+1,y+1)]
            , i >= 0    , j >= 0 
            , i <= width, j <= height ]

forklifts w h m = [ (x,y) | y <-[0..h] ,x <-[0..w] , m!!y!!x == '@']

less4  m = (<4) . length . filter ('@'==) . map (\(x,y) -> m!!y!!x ) 

-- return the coords of rolls of paper accessible
acessForklift m = map snd
                $ filter (less4 m . fst) 
                $ map ( (eight w h >< id) . dup ) 
                $ forklifts w h m
    where w = length (head m) -1
          h = length m -1
 
myFix m acc 
    | m == acc  = acc
    | otherwise = myFix (changeMap m $ acessForklift m) m 

part1 = length . acessForklift

part2 m = start - end
    where w = length (head m) -1
          h = length m -1
          start = length $ forklifts w h m 
          end   = length $ forklifts w h $ myFix m []

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"

    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )


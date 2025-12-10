import Data.List.Split
import Data.List

infixr 8 ...
infixr 8 ><

(...) = (.).(.)  -- is the same as .:
(f >< g) (x,y) = (f x,g y)
dup a = (a,a)
mktp [a,b] = (a,b)

------ Parsing ----------------------------------------------------------------

parseInput :: String -> ([(Int,Int)] ,[Int])
parseInput = id
           . ( map ( mktp . map read)   >< map read )
           . ( map (splitOn "-")        >< id )
           . ( filter (not.null)        >< filter (not.null) )
           . ( splitOn "\n"             >< splitOn "\n" )
           . mktp
           . filter (not . null) 
           . splitOn "\n\n"

------- Part 1 ----------------------------------------------------------------
ranges :: [(Int,Int)] -> Int -> Bool
ranges l x = any (\(a,b) -> a <= x && x <= b ) l

part1 (r,ids) = length $ filter (ranges r) ids

-------------------------------------------------------------------------------
intersection (x1,x2) (a,b) = a <= x1 && x1 <= b 
                          || a <= x2 && x2 <= b 
                          || x1 <= a && b <= x2 
                          || x1 >= a && x2 <= b

------- Part 2 ----------------------------------------------------------------

merge (x1,x2) (x3,x4) = (min x1 x3, max x2 x4)

mergeRanges [] = []
mergeRanges [a] = [a]
mergeRanges (r1:r2:rs) 
    | intersection r1 r2 =     mergeRanges ( merge r1 r2 :rs)
    | otherwise          = r1: mergeRanges (r2:rs)

part2 (r,ids) = sum $ map (\(a,b)-> b - a + 1) $ mergeRanges $ sort r

-------------------------------------------------------------------------------

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"

    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )


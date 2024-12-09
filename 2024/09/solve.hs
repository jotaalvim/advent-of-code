import Data.List.Split
import Data.List

import Data.Char
import Cp

parseInput = id -- ( free spots, [(   id, number of space ) ] ) 
           . ( (0:) . map digitToInt  >< map (id >< digitToInt) )
           . (free                    >< zip [0..] . files)
           . dup
           . head 
           . filter (not . null) 
           . splitOn "\n"

parseInput2 = id
            . uncurry merge
            . (tail . zip (repeat (-1))  >< id)


merge []      f     = f
merge (b:bs) (f:fs) = f:b:merge bs fs

free (a:b:t) = b : free t 
free _       = []

files []      = []
files [a]     = [a]
files (a:b:t) = a: files t

-------------------------------------------------------------------------------

points bp fid s = sum $ take s [ p * fid | p <- [bp,bp+1..] ]

part1 (  f   , [] ) b = 0
part1 ((f:fs), l  ) bp
    | f == 0      = (points bp fid ispace) +  part1 (fs, tail l) (bp + ispace )
    | f >  lspace = (points bp lid lspace) +  part1 ((f-lspace): fs, init l) (bp + lspace  )
    | f <= lspace = (points bp lid f     ) +  part1 (0:fs, init l ++ [(lid,lspace-f)]) (bp + f )
    where (fid, ispace ) = head l
          (lid, lspace ) = last l

-------------------------------------------------------------------------------
replaceId (i,s) [] = []
replaceId (i,s) ((-1,s2):(i2,s9):(-1,s3):t) 
    | i == i2 = (-1, s2+s3+s):t
    | otherwise = (-1,s2):(i2,s9): replaceId (i,s) ((-1,s3):t)

replaceId (i,s) ((i2,s2):(-1,s3):t) 
    | i == i2 = (-1, s3+s):t
    | otherwise = (i2,s2): replaceId (i,s) ((-1,s3):t)

replaceId (i,s) ((-1,s2):(i2,s3):t) 
    | i == i2 = (-1, s2+s):t
    | otherwise = (-1,s2):(i2,s3): replaceId (i,s) t

replaceId (i,s) ((i2,s2):t) 
    | i == i2 = (-1,s):t 
    | otherwise = (i2,s2):replaceId (i,s) t

replace _ _ [] = [] 
replace (li,ls) (pi,ps) ( (id2,ps2) :t )
    | pi == id2 && ps == ps2 && ps2-ls == 0 = (li,ls): replaceId (li,ls) t
    | pi == id2 && ps == ps2 && ps2 > ls    = (li,ls): (-1,ps2 -ls) : replaceId (li,ls) t
    | otherwise = (id2,ps2): replace (li,ls) (pi,ps) t

part2 :: [(Int, Int)] -> Int 
part2 l = solve2 l $ reverse $ filter ( (0<=) . fst) l 

checksum []        bp = 0
checksum ((-1,s):t) bp =                 checksum t (bp + s)
checksum ((i, s):t) bp = points bp i s + checksum t (bp + s)

solve2 l [] =  checksum l 0
solve2 l ((ni,ns):nt)
    | null pos  = solve2 l nt
    | otherwise = solve2 (replace (ni,ns) (head pos) l) nt
    where
        pos = filter (\(i,s)-> i == -1 && s >= ns ) $ takeWhile ((/= ni) . fst) l


main :: IO ()
main = do
    input  <- parseInput  <$> readFile "input.txt"
    let input2 = parseInput2 input
    putStrLn $ "Part 1: " ++ show ( part1 input 0 )
    putStrLn $ "Part 2: " ++ show ( part2 input2 )

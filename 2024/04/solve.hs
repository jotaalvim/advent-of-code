import Data.List.Split
import Data.List
import Text.Regex.Posix

example = [ "1...XXMAS.", 
            "2SAMXMS...",
            "3..S..A...",
            "4.A.A.MS.X",
            "XMASAMX5MM",
            "X6....XA.A",
            "S7S.S.S.SS",
            "8A.A.A.A.A",
            "9.M.M.M.MM",
            "ÇX.X.XMASX" ]

example2 =  [".M.S......",
             "..A..MSMS.",
             ".M.S.MAA..",
             "..A.ASMSM.",
             ".M.S.M....",
             "..........",
             "S.S.S.S.S.",
             ".A.A.A.A..",
             "M.M.M.M.M.",
             ".........."]



parseInput = filter (not . null) 
           . splitOn "\n"
allDiagonal m = [diagonalS m (i,0) | i <- [0.. length m -1]] 
            ++  [diagonalI m (0,i) | i <- [1.. length (head m) -1]]

diagonalS m (sx,sy) = [getV (i+sx,i+sy) m | i <- [0..rows]] where rows = length m - sx -1
diagonalI m (sx,sy) = [getV (i+sx,i+sy) m | i <- [0..cols]] where cols = length (head m) - sy -1

genX m =  [m, rm, ad, adr, adrev,adrevr,col,colr]
    where [rm, adr, adrevr, colr] = map (map reverse) [m, ad, adrev, col]
          col    = transpose m
          ad     = allDiagonal m
          adrev  = allDiagonal rm

countX e =  length $ concat $ (e =~ "XMAS" :: [[String]])

lookX l = (concat l =~ "M.S.A.M.S" :: [[String]])
       ++ (concat l =~ "S.M.A.S.M" :: [[String]])
       ++ (concat l =~ "M.M.A.S.S" :: [[String]])
       ++ (concat l =~ "S.S.A.M.M" :: [[String]])

getV (x,y) m = m !! y !! x

countM = sum . map countX

retiraBlocRow m = [ [take 3 (drop x m) ]          | x <- [0..length m -1]]
retiraBlocCol m = [ map (take 3) (map (drop x) m) | x <- [0..length (head m) -1]]

part1 = sum . map countM . genX

part2 = length 
      . concatMap lookX 
      . concatMap retiraBlocCol 
      . concat 
      . retiraBlocRow 

main :: IO ()
main = do
    input  <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )

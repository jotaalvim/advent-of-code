import Data.List.Split
import Data.List

area1 :: [Int] -> Int
area1 (l:w:h:_) = l     * w
area2 (l:w:h:_) =     h * w
area3 (l:w:h:_) = l * h

paper :: [Int] -> Int
paper l = 2*a1 + 2*a2 + 2*a3 + minimum [a1,a2,a3]
    where a1 = area1 l
          a2 = area2 l
          a3 = area3 l

part1 = sum . map paper

papeis :: [Int] -> Int
papeis l = product l + sum [ x*2 | x <- l, x /= maximum l ]

part2 = sum . map papeis

main = do
    input <- lines <$> readFile "input.txt"
    --["2x3x22","4x5x6"]
    --[["2","3","22"],["4","5","6"]]
    let l = map ( map read ) $ map (splitOn "x") input
    putStrLn $ "Solution to the part 1 : " ++ show ( part1 l )
    print l
    putStrLn $ "Solution to the part 2 : " ++ show ( part2 l )

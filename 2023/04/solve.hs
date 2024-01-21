import Cp 
import Data.List.Split
import Data.List

-- parsing -- 

toInt :: String -> Int 
toInt = read 

fromJust (Just a) = a
mk2 [a,b] = (a,b)

parseInput =  map ( ( id           >< ( map toInt >< map toInt ) )
                  . ( id           >< ( words     >< words) )
                  . ( toInt        >< mk2 )
                  . ( last . words >< (splitOn "|"))
                  . ( mk2  . (splitOn ":") ) )
                  . splitOn "\n"

-- Solution -- 

-- Part 1 -- 
points 0  = 0
points n  = 2^ pred n

repetidos = nub >>= flip (\\)

part1     = sum . map ( points . length . repetidos . conc . snd )

-- Part 2 -- 
part2  c  = length $ cataCards c c $ map fst c

pedaco (n, (a,b)) = [ succ n .. n + (length  (repetidos (a++b))) ]

next_cards cards = map ( (id >< (fromJust . flip lookup cards)) . dup )

cataCards cc [] b = b
cataCards cc c b  = cataCards cc nc ((map fst nc) ++ b) where 
          nc = next_cards cc $ concatMap pedaco c

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )

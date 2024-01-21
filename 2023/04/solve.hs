import Cp 
import Data.List.Split
import Data.List

-- parsing -- 

toInt :: String -> Int 
toInt = read 

mk2 [a,b] = (a,b)

parseInput =  map ( ( id           >< ( map toInt >< map toInt ) )
                  . ( id           >< ( words     >< words) )
                  . ( toInt        >< mk2 )
                  . ( last . words >< (splitOn "|"))
                  . ( mk2  . (splitOn ":") ) )
                  . splitOn "\n"

-- Solution -- 

repetidos = nub >>= flip (\\)

points 0 = 0
points n = 2^ pred n

part1 = sum . map ( points . length . repetidos . conc . snd  )
part2 = id


main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )

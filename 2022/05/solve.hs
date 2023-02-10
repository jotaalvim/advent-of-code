import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split
--https://williamyaoh.com/posts/2019-04-11-cheatsheet-to-regexes-in-haskell.html
--import Text.Regex.TDFA

type From   = Int
type To     = Int
type Amount = Int
type Move   = (Amount,(From, To))

type Ship = Map Int String

s :: Ship
--s :: Map Int [Char]
s = Map.fromList [(1,"ZTFRWJG"),(2,"GWM"),(3,"JNHG"),(4,"JRCNW"),(5,"WFSBGQVM"),(6,"SRTDVWC"),(7,"HBNCDZGV"),(8,"SJNMGC"),(9,"GPNWCJDL")] 

-- part1

shiplast :: Ship -> [Char]
shiplast s = [ last (s Map.! n) | n <- [1..9]]

--move :: ([a] -> [a]) -> Map Int [Char] -> Move -> Map Int [Char]
move :: Ord b => ([a] -> [a]) -> Map b [a] -> (Int, (b, b)) -> Map b [a]
move k ship (a,(f,t)) = newShip2
    where containers = k $ take a  $ reverse $ ship Map.! f
          newF       = reverse $ drop a $ reverse $ ship Map.! f
          newShip    = Map.insert f newF ship
          to         = ship Map.! t
          newShip2   = Map.insert t (to++containers) newShip

part1 :: [Move] -> Ship -> [Char]
part1 l s = shiplast $ foldl (move id) s l

-- part2

part2 :: [Move] -> Ship -> [Char]
part2 l s = shiplast $ foldl (move reverse) s l

-- parsing

parseMove :: [String] -> Move
parseMove [a,b,c] = ( read a, (read b, read c))

odds l = [x | (x,n)<- zip l [0..], odd n ]

--parseInput :: String -> Ship
parseInput  = map parseMove . map odds . map words . lines . (!! 1) . splitOn "\n\n"

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input s)
    putStrLn $ "Part 2: " ++ show ( part2 input s)


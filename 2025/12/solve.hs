import Data.List.Split
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

infixr 8 ><
(f >< g) (x,y) = (f x,g y)
mktp [a,b] = (a,b)
dup a = (a,a)

------ Parsing ----------------------------------------------------------------

takeIndex l = [ (l!!i,i)| i <- [0..length l-1]] 

getC m = [ (x,y)  | x<-[0..length(head m)-1],y<-[0..length m -1],m!!y!!x == '#']

parseList = map (     mktp     >< takeIndex )
          . map ( map read     >< map read )
          . map ( splitOn "x"  >< words)
          . map ( mktp . splitOn ":")
          . lines

parseShape = Map.fromList
           . zip [0..]
           . map ( getC )
           . map ( filter (not.null) )
           . map ( lines .last . splitOn ":")

parseInput :: String -> (Map Int [(Int, Int)], [((Int, Int), [(Int, Int)])])
parseInput =  id
           . (parseShape >< parseList)
           . (init       >< last)
           . dup 
           . splitOn "\n\n"

------- Part 1 ----------------------------------------------------------------

shapeSize s l = sum $ map (\(n,i) -> n * length (s Map.! i) ) l

lineFits shapes ((x,y), lt) =  x*y >= shapeSize shapes lt

part1 ( shapes, presents ) = length $ filter (lineFits shapes) presents

part2  = id
       . const ()

-------------------------------------------------------------------------------

main :: IO ()
main = do
    --input <- parseInput <$> readFile "inputExample.txt"
    input <- parseInput <$> readFile "input.txt"

    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )


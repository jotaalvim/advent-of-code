import Data.List.Split
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

infixr 8 ><
(f >< g) (x,y) = (f x,g y)
mktp [a,b] = (a,b)

------ Parsing ----------------------------------------------------------------

parseInput :: String -> Map String [String]
parseInput = Map.fromList
           . map ( id >< words )
           . map (mktp . splitOn ":")
           . lines

------- Part 1 ----------------------------------------------------------------

(++++) (a,b,c,d) (q,w,e,r) = (a+q,b+w,c+e,d+r)

spreadCounter "dac" (a,f,d,b) next  
    | f == 0             = map (\n -> (n, (0,0,a,0))) next 
    | f /= 0             = map (\n -> (n, (0,0,0,f))) next 
    
spreadCounter "fft" (a,f,d,b) next 
    | d == 0             = map (\n -> (n, (0,a,0,0))) next 
    | d /= 0             = map (\n -> (n, (0,0,0,d))) next 


spreadCounter _     (a,f,d,b) next = map (\n -> (n, (a,f,d,b))) next

progress :: [String] -> Map String (Integer,Integer,Integer,Integer) 
                     -> Map String [String] 
                     -> Map String (Integer,Integer,Integer,Integer) 
progress []    count m = count
progress (h:t) count m = progress (nub (t ++ next)) newCount m
    where next     = m Map.! h 
          updatedV = spreadCounter h (count Map.! h) next 
          --updatedV = map (\n -> (n, count Map.! h)) next
          newCount = Map.fromListWith (++++) $ Map.toList count ++ updatedV

fromTo f t r = progress [ f ] zeros newR
    where zeros = Map.insert f (1,0,0,0)
                $ Map.fromList 
                $ zip (concat (Map.elems r) ++ Map.keys r ++ [t]) $ repeat (0,0,0,0)
          newR  = Map.insert t [] r

part1  = id --(Map.! "out") . fromTo "you" "out"

part2  = (Map.! "out") . fromTo "svr" "out"
-------------------------------------------------------------------------------

main :: IO ()
main = do
    --input <- parseInput <$> readFile "inputExample.txt"
    input <- parseInput <$> readFile "input.txt"

    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )


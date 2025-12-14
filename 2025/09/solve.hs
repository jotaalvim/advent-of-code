import Data.List.Split
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
infixr 7 ...
infixr 8 ><
(...) = (.).(.)
(f >< g) (x,y) = (f x,g y)
dup a = (a,a)

------ Parsing ----------------------------------------------------------------

parseInput :: String -> [(Int, Int)]
parseInput = map ( read . ("(" ++) . (++ ")") ) . lines

-------------------------------------------------------------------------------
area (x1,y1) (x2,y2) = (abs (x1-x2 ) +1) * (abs (y1 - y2 ) +1 )

pairs []    = []
pairs (h:t) = map ( (,) h ) t ++ pairs t

part1 = last . sort . map (uncurry area) . pairs

-------------------------------------------------------------------------------
-- functions to compress and decompress coordinates
l2m = flip zip [0..] . sort . nub ... map 

dict :: [(Int,Int)] -> ( Map Int Int, Map Int Int )
dict = ( Map.fromList . l2m fst >< Map.fromList . l2m snd ) . dup

swapR = Map.fromList . uncurry zip . (Map.elems >< Map.keys) . dup 

compress   (mx,my) = map ( (      mx Map.!) >< (      my Map.!))
decompress (mx,my) = map ( (swapR mx Map.!) >< (swapR my Map.!)) 

-------------------------------------------------------------------------------

edges [] = []
edges [a] = [a]
edges ((x1,y1):(x2,y2):ps)
    | x1 == x2 = [(x1,ny) | ny <- enumFromTo (min y1 y2) (max y1 y2)] ++ edges ((x2,y2):ps)
    | y1 == y2 = [(nx,y1) | nx <- enumFromTo (min x1 x2) (max x1 x2)] ++ edges ((x2,y2):ps)
    

part2 l = id
      -- $  decompress d
      $ nub $ edges
      -- $ uncurry (++) $ (id >< singleton . head) $ dup
      $ compress d l
    where d = dict l
         
-------------------------------------------------------------------------------

main :: IO ()
main = do
    input <- parseInput <$> readFile "inputExample.txt"

    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )


import Data.List.Split
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
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

part1 = foldl ( (. uncurry area) . max ) 0 . pairs

-------------------------------------------------------------------------------
-- functions to compress and decompress coordinates
l2m = flip zip [0..] . sort . nub ... map 

dict :: [(Int,Int)] -> ( Map Int Int, Map Int Int )
dict = ( Map.fromList . l2m fst >< Map.fromList . l2m snd ) . dup

swapR = Map.fromList . uncurry zip . (Map.elems >< Map.keys) . dup 

compress   (mx,my) = map ( (      mx Map.!) >< (      my Map.!))
decompress (mx,my) = map ( (swapR mx Map.!) >< (swapR my Map.!)) 

-------------------------------------------------------------------------------

edges [ ] = [ ]
edges [a] = [a]
edges ((x1,y1):(x2,y2):ps)
    | x1 == x2  = [(x1,ny) | ny <- enumFromTo (min y1 y2) (max y1 y2)] ++ edges ((x2,y2):ps)
    | y1 == y2  = [(nx,y1) | nx <- enumFromTo (min x1 x2) (max x1 x2)] ++ edges ((x2,y2):ps)

drawP p m = if elem p m then "#" else " "

draw m = [ concat [ drawP (x,y) m | x<-[-1..mx+1]] | y<-[-1..my+1]] 
    where my = maximum $ map snd m
          mx = maximum $ map fst m

replaceM (x,y) m = pl ++ [(px++"•"++xs)]  ++ ls
    where (pl,rl:ls) = splitAt y m
          (px,rx:xs) = splitAt x rl

extractMiddle m = [(x-1,y-1) | x <- [0..length (head m) -1],y <- [0..length m -1 ], m !!y!!x /= '•']

bfs []        v m = m
bfs ((x,y):t) v m = bfs (nub $ (t ++ around)) newV newM
    where 
        newM = if m!!y!!x == ' ' then replaceM (x,y) m else m
        newV = Set.insert (x,y) v
        around =  filter (\(a,b)-> m!!b!!a /= '#')
                $ filter (not . (flip Set.member v)) 
                $ filter (\(x,y) -> x>=0 && y>=0 && x<=maxX && y<=maxY) 
                  [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
        maxX = pred $ length $ head m 
        maxY = pred $ length m 

mkRct (x1,y1) (x2,y2) = nub $ concat $ [[(x,miny),(x,maxy)] | x <- [minx..maxx]] ++ [[(minx,y),(maxx,y)] | y <- [miny..maxy]]
    where (minx,maxx,miny,maxy) = (min x1 x2, max x1 x2, min y1 y2, max y1 y2)

allRectangles m  = foldl (\(v,[o1,o2]) (p1,p2) -> if area p1 p2 > v   
                                                  then (area p1 p2, [p1,p2]) 
                                                  else (v,[o1,o2])
                         ) (0,[(0,0),(0,0)]) m

filterRectangles m middle = filter (\(p1,p2) -> all (flip elem middle) $ mkRct p1 p2)  p
    where p = pairs m -- all possible pairs

part2 l = id
      $ allRectangles 
      $ map (\(p1,p2)-> (head $ decompress d [p1], head $ decompress d [p2] ))
      $ filterRectangles c
      $ extractMiddle
      $ bfs [(0,0)]  Set.empty
      $ draw m
    where d = dict l
          c = compress d l
          m = nub $ edges $ (\l -> l ++ [head l]) c
-------------------------------------------------------------------------------

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"

    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )

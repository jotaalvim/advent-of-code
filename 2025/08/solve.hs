import Data.List.Split
import Data.List 
import Data.Set (Set)
import qualified Data.Set as Set

infixr 8 ><
(f >< g) (x,y) = (f x,g y)

------ Parsing ----------------------------------------------------------------

data Point = Point { x :: Int, y :: Int, z :: Int } deriving (Show, Eq, Ord)

parseInput :: String -> [Point]
parseInput = map (\[a,b,c] -> Point a b c )
           . map ( map read . splitOn ",")
           . lines

-------------------------------------------------------------------------------

distance p1 p2 = sqrt $ fromIntegral $ (x p1-x p2)^2+(y p1-y p2)^2+(z p1-z p2)^2

getDistaces [] = []
getDistaces (h:t)  = map ( (,) h ) t ++ getDistaces t

join :: [Set Point] -> (Point,Point) -> [Set Point]
join cir (p1,p2) = if null fits 
                   then [Set.insert p1 ( Set.insert p2 Set.empty        )] ++ other
                   else [Set.insert p1 ( Set.insert p2 (Set.unions fits))] ++ other
    where  (fits,other) = partition (\c -> Set.member p1 c || Set.member p2 c) cir
    
part1  = product . take 3 
       . reverse . sort . map Set.size
       . foldl join [] 
       . take 1000
       . sortOn (uncurry distance) 
       . getDistaces 

-------------------------------------------------------------------------------
findSingle acc total (h:t) = if length next == 1 && null newt
                             then h
                             else findSingle next newt t
    where next = join acc h 
          newt = delete (fst h) $ delete (snd h) total

part2 l = uncurry (*) . (x >< x)
       $ findSingle [] l
       $ sortOn (uncurry distance) 
       $ getDistaces l
-------------------------------------------------------------------------------

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )

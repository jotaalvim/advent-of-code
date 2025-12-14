import Data.List
import Data.List.Split
import Data.List.Utils
import DurationMonad
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.LinearProgramming
import qualified Numeric.LinearProgramming as LP

------ Parsing ----------------------------------------------------------------

parseLine :: String -> ( [Int], [[Int]], [Int])
parseLine l = ( newLights , read newButtons, read voltages)
    where [lights, rest]  = splitOn "]" l
          [buttons,rest2] = splitOn "{" rest
          newButtons = ("["++) $ (++"]") $ intercalate "," $ words $ replace ")" "]" $ replace "(" "[" buttons
          voltages   =  "[" ++ init rest2 ++ "]"
          newLights  = elemIndices '#' $ tail lights

parseInput = map (parseLine)  . lines

-------------------------------------------------------------------------------

toogle b lights = sort $ union lights b \\ intersect lights b

toogleM :: Duration [Int] -> [Int] -> Duration [Int]
toogleM dur b =  wait1 $ toogle b <$> dur

toogleAll :: Duration [Int] -> [[Int]] ->  [Duration [Int]]
toogleAll = map . toogleM 

-- go durations buttons visited finalState - it's a poor implementation of bfs
go ds lb v f
     | elem f (map getValue ds)  = snd $ head $ sortOn snd 
                                       $ map (\d-> (getValue d, getDuration d))
                                       $ filter ( (f ==) . getValue ) ds
     | otherwise = go unseenDur lb newV f
    where candidates = ds >>= flip toogleAll lb 
          unseenDur  = filter (not . flip Set.member v . getValue) candidates
          newV   = foldl (flip Set.insert) v $ map getValue unseenDur


part1 = id --sum . map (\(d,lb,_) -> go [ return [] ] lb Set.empty d)

-------------------------------------------------------------------------------

elemB e l = if elem e l then 1 else 0

construct btt v = Dense [ map (elemB i) btt :==: (v!!i)  | i <- [0.. length v -1]]

eqSolver btt v = value
    where constraints = construct btt v
          Optimal (value,p) = LP.simplex (LP.Minimize (map (const 1) btt )) constraints []
part2  =  map (\(_,lb,v) -> eqSolver lb (map fromIntegral v))
         
-------------------------------------------------------------------------------

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"

    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )


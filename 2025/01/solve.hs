import Data.List.Split
import Data.List

infixr 8 ...
infixr 8 ><

(...) = (.).(.)  -- is the same as .:
(f >< g) (x,y) = (f x,g y)
dup a = (a,a)

------ Parsing ----------------------------------------------------------------

parseRot :: String -> Rotation
parseRot ('R':n) = R (read n)
parseRot ('L':n) = L (read n)

parseInput :: String -> [Rotation]
parseInput = map parseRot
           . filter (not . null) 
           . splitOn "\n"

-------------------------------------------------------------------------------
--          (loops, position)
type Dial = (Int  , Int     )  

data Rotation = R Int | L Int 

rotates :: Dial -> Rotation -> Dial
rotates (l,p) (R n) = fixCountsR   $ (abs >< id) $ divMod (p + n) 100
rotates (l,p) (L n) = fixCountsL p $ (abs >< id) $ divMod (p - n) 100

-- finishing on zeros does not click
fixCountsR (nl, 0 ) = (nl - 1, 0)   
fixCountsR p = p

-- starting  on zeros does not click
fixCountsL 0 (nl, np) = ( nl -1, np)
fixCountsL _ p = p

countsZero = filter $ (== 0) . snd 

-------------------------------------------------------------------------------

part1 =   length
      .   countsZero
      ... scanl rotates

part2 =   uncurry (+)
      .   ( sum . map fst >< length . countsZero )
      .   dup
      ... scanl rotates

-------------------------------------------------------------------------------

-- initial state
d = (0,50)

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"

    putStrLn $ "Part 1: " ++ show ( part1 d input )
    putStrLn $ "Part 2: " ++ show ( part2 d input )


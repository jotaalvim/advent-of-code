import Data.List.Split (splitOn)
import Data.List.Extra hiding (splitOn)
import Data.List

infixr 8 ><
(f >< g) (x,y) = (f x,g y)
mktp [a,b] = (a,b)

------ Parsing ----------------------------------------------------------------

parseFunction "*" = (*)
parseFunction "+" = (+)

parseInput :: String -> ( [[String]] , [String] )
parseInput = ( map words . lines ><  words) . mktp . splitOn "\n\n"

parseInput2 = (id >< head)
            . ( filter (not . null) >< filter (not . null) )
            . (splitOn "\n" >< splitOn "\n" ) . mktp
            .  splitOn "\n\n"

-------------------------------------------------------------------------------

collOfNum i = all ( (' '/=) .  (!! i) )

fill n padding s | padding     = s ++ take ( n - length s) (repeat ' ')
                 | not padding =      take ( n - length s) (repeat ' ') ++ s

fillLine l padding = map ( fill n padding ) l
    where n = maximum $ map length l

getPadding (m, l) = map ( flip collOfNum m ) $ sort $ elemIndices '+' l ++ elemIndices '*' l

tidyInput (m,f) = [(transpose intM !!i,f!!i) | i <- [0..length f -1]]
    where intM = map (map read) m

tidyInvertedInput :: ([[String]], b) -> [Bool] -> ([[Int]], b)
tidyInvertedInput (m,f) p = (map (map read) >< id) $ (newm, f)
    where newm = map transpose $ map (uncurry fillLine) $ zip ( transpose m) p

problem a (m,f) =  (foldl1 ( parseFunction f ) m ) : a

part1     = sum . foldl problem [] . tidyInput
part2 i a = sum $ foldl problem [] $ uncurry zip inp
    where inp = tidyInvertedInput i ( getPadding a)

-------------------------------------------------------------------------------

main :: IO ()
main = do
    let file = "input.txt"
    input    <- parseInput  <$> readFile file
    allLines <- parseInput2 <$> readFile file

    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input allLines )


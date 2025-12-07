import Data.List.Split
import Data.List

infixr 8 ><
(f >< g) (x,y) = (f x,g y)
mkp [a,b] = (a,b)

------ Parsing ----------------------------------------------------------------

parseInput :: String -> [ (Int,Int) ]
parseInput = map (read >< read)
           . map mkp 
           . map (splitOn "-")
           . splitOn ","

-------------------------------------------------------------------------------
half :: String -> (String, String)-- f >>= k = \ r -> k (f r) r
half = (flip div 2) . length >>= splitAt 


range = uncurry enumFromTo

repeats p "" = False
repeats p (h:t) = if pattern p (h:t) then True else repeats (p++[h]) t

pattern p "" = True
pattern p s | isPrefixOf p s = pattern p $ drop (length p) s
            | otherwise      = False

silly = uncurry (==) . half

silly2 (h:t) = repeats [h] t


counts f = sum . map read . filter f . map show . concatMap range

part1 = counts silly

part2 = counts silly2
-------------------------------------------------------------------------------

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )


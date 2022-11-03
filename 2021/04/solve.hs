import Data.List.Split
import Data.List


data BingoNumber = Drawn Int | NotDrawn Int 
    deriving(Show, Eq)

type BingoBoard = [[BingoNumber]]


isDrawn :: BingoNumber -> Bool
isDrawn (Drawn _) = True
isDrawn _ = False


takeNum :: Int -> BingoNumber -> BingoNumber
takeNum _ (Drawn n) = Drawn n
takeNum x (NotDrawn n)
    | x == n = Drawn n
    | otherwise = NotDrawn n


takeNumBoard :: Int -> BingoBoard -> BingoBoard 
takeNumBoard x = map  $ map $ takeNum x


checkRow :: BingoBoard -> Bool
checkRow = any $ all (isDrawn)


checkColumn :: BingoBoard -> Bool
checkColumn = checkRow . transpose 


takeNumBoards :: Int -> [BingoBoard] -> [BingoBoard]
takeNumBoards x = map takeNumBoard x


--part1 :: ([Int],[Bingo]) -> Int 
--part1 ((n:ns),boards)

-- part 2

-- parsing 

par a b x = (a x, b x)

td :: [a] -> ([a],[a])
td = par (take 5) (drop 5)

parseBoard :: [[BingoNumber]] -> [BingoBoard]
parseBoard [] = []
parseBoard (a:b:c:d:e:t) = [a,b,c,d,e]: parseBoard t

parseNum :: [String] -> [[BingoNumber]]
parseNum l  = map (map ( NotDrawn . (\x -> read x ::Int) ) ) $ map words l

parseList :: String -> [Int]
parseList = map read . words 

parseInput :: String -> ([Int],[BingoBoard])
parseInput i = (parseList h,parseBoard $ parseNum t)
    where (h:t) = filter (not.null) $ lines i


main :: IO ()
main = do 
    input <- parseInput <$> readFile "input.txt"

    --putStrLn $ "Part 1: "++ show (part1 input)
    --putStrLn $ "Part 1: "++ show (part2 input)
    putStrLn $ show input

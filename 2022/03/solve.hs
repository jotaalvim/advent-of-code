import Data.Char

type Item = String

common :: (Item,Item) -> Char
common (h:t, l)
    | elem h l  = h
    | otherwise = common (t,l)

split :: (a -> b) -> (a -> c) -> a -> (b,c)
split f g x = (f x, g x)

half :: Item -> (Item,Item)
half = uncurry splitAt . split (flip div 2 . length) id
--half i = splitAt (div (length i) 2) i

priority :: Char -> Int
priority c
    | isLower c = ord c - 96
    | isUpper c = ord c - 38
--priority c = cond isLower (ord c -96) (ord c - 38)

part1 :: [Item] -> Int
part1 = sum . map (priority . common . half )

-- part 2

common3 :: [Item] -> Item
common3 [] = []
common3 ("":b:c:t)= common3 t
common3 ((x:xs):b:c:t)
    | elem x b && elem x c = x:common3 t
    | otherwise = common3 (xs:b:c:t)

part2 :: [Item] -> Int
part2 =  sum . map priority . common3


-- parsing

parseInput :: String -> [Item]
parseInput = lines

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )

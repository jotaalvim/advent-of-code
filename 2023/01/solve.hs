import Data.List.Split
import Data.List
import Data.Char
import List
import Cp
-- filtra digitos 
fd = cataList $ either nil filtro 

filtro = cond (isDigit . p1) cons p2

todas = cataList $  either nil (cons . (fd >< id ))

retirar = cataList $ either nil (cons . (junta >< id ) )

junta = cons . (Cp.split head  (singl . last))

part1 = sum . map (read :: String -> Int) . retirar . todas 

part2 = part1 . map troca

teste = ["two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen"]

troca "" = ""
troca s@(h:t)
    | isPrefixOf "one"   s = '1': troca (drop 3 s)
    | isPrefixOf "two"   s = '2': troca (drop 3 s)
    | isPrefixOf "three" s = '3': troca (drop 5 s)
    | isPrefixOf "four"  s = '4': troca (drop 4 s)
    | isPrefixOf "five"  s = '5': troca (drop 4 s)
    | isPrefixOf "six"   s = '6': troca (drop 3 s)
    | isPrefixOf "seven" s = '7': troca (drop 5 s)
    | isPrefixOf "eight" s = '8': troca (drop 5 s)
    | isPrefixOf "nine"  s = '9': troca (drop 4 s)
    | otherwise = h: troca t 

-- parsing

parseInput :: String -> [String]
parseInput = splitOn "\n"

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show ( part1 input )
    putStrLn $ "Part 2: " ++ show ( part2 input )

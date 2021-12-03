import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Data.List.Split (splitOn)
import Data.List (transpose)

funSum :: [Int] -> Int
funSum [] = 0
funSum (x:xs)
  | x == 1 = 1 + funSum xs
  | otherwise = -1 + funSum xs

binToInt :: [Int] -> Int
binToInt x = foldr (\c s -> s * 2 + c) 0 (reverse x)

gammaBit :: Int -> Int 
gammaBit x
  | x > 0 = 1
  | otherwise = 0

epsilonBit :: Int -> Int 
epsilonBit x
  | x <= 0 = 1
  | otherwise = 0

gamma :: [Int] -> Int
gamma x = binToInt [gammaBit g | g <- x]

epsilon :: [Int] -> Int
epsilon x = binToInt [epsilonBit g | g <- x]

parseLine :: String -> [Int]
parseLine = map ((read :: String -> Int) . (: []))

solve :: [[Int]] -> Int
solve s = gamma x * epsilon x
  where x = map funSum (transpose s)

bitsAtIx :: Int -> [[Int]] -> [Int]
bitsAtIx ix l = [x !! ix | x <- l]

mostFreq :: Int -> [[Int]] -> Int
mostFreq ix l
  | funSum (bitsAtIx ix l) >= 0 = 1
  | otherwise = 0

most :: Int -> [[Int]] -> Int
most ix l
  | null (tail step) = binToInt (head step)
  | otherwise = most (ix + 1) step
  where step = [x | x <- l, (x !! ix) == mostFreq ix l]

leastFreq :: Int -> [[Int]] -> Int
leastFreq ix l
  | funSum (bitsAtIx ix l) < 0 = 1
  | otherwise = 0

least :: Int -> [[Int]] -> Int
least ix l
  | null (tail step) = binToInt (head step)
  | otherwise = least (ix + 1) step
  where step = [x | x <- l, (x !! ix) == leastFreq ix l]

solve2 :: [[Int]] -> Int 
solve2 s = most 0 s * least 0 s

main :: IO ()
main = do  
        let list = []
        handle <- openFile "input3.txt" ReadMode
        contents <- hGetContents handle
        let instructions = [parseLine l | l <- lines contents]
        print (solve instructions)
        print (solve2 instructions)
        hClose handle   

-- 2967914
-- 7041258

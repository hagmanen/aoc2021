import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Control.Monad ()

solveForHead :: Int -> [Int] -> Int
solveForHead target [] = 0
solveForHead target (x:xs) = head ([x * n | n <- xs, x  + n == target] ++ [0])

inc :: Int -> Int -> Int
inc a b = if a < b then 1 else 0

sumIncs :: Int -> Int -> [Int] -> Int 
sumIncs s _ [] = s
sumIncs s p (x:xs) = sumIncs (s + inc p x) x xs

solve :: [Int] -> Int
solve [] = 0
solve (x:xs) = sumIncs 0 x xs

calcInc :: Int-> [Int] -> Int
calcInc x xs = if length xs == 3 then
  inc ( x + sum (take 2 xs)) (sum xs)
  else 0

solve2 :: Int -> [Int] -> Int
solve2 s [] = s
solve2 s (x:xs) = solve2 (s + calcInc x (take 3 xs)) xs

main :: IO ()
main = do  
        let list = []
        handle <- openFile "input1.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = f singlewords
        print (solve list)
        print (solve2 0 list)
        hClose handle   

f :: [String] -> [Int]
f = map read

-- 1616
-- 1645

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

solve2S3 :: Int -> Int -> Int -> Int -> [Int] -> Int 
solve2S3 s _ _ _ [] = s
solve2S3 s p1 p2 p3 (x:xs) = solve2S3 (s + inc (p1 + p2 + p3) (p2 + p3 + x)) p2 p3 x xs

solve2S2 :: Int -> Int -> Int -> [Int] -> Int 
solve2S2 s _ _ [] = s
solve2S2 s p1 p2 (x:xs) = solve2S3 s p1 p2 x xs

solve2S1 :: Int -> Int -> [Int] -> Int 
solve2S1 s _ [] = s
solve2S1 s p (x:xs) = solve2S2 s p x xs

solve2 :: [Int] -> Int
solve2 [] = 0
solve2 (x:xs) = solve2S1 0 x xs

main :: IO ()
main = do  
        let list = []
        handle <- openFile "input1.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = f singlewords
        print (solve list)
        print (solve2 list)
        hClose handle   

f :: [String] -> [Int]
f = map read

-- 1616
-- 1645

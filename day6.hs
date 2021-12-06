import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Data.List.Split (splitOn)
import Data.Map (fromListWith, toList)

create :: Int -> [Int]
create f = [0, 0, 0, 0, 0, 0, f, 0, f]

produce :: Int -> [Int] -> Int
produce 0 fs = sum fs
produce _ [] = 0
produce s (f:fs) = produce (s-1) (zipWith (+) (fs ++ [0]) (create f))

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

oneAtTime :: Int -> [Int]
oneAtTime x = (\y -> fromEnum (x == y)) <$> [0..8]

time2list :: [Int] -> [Int] -> [Int]
time2list ts l = foldl (\ l t -> zipWith (+) l (oneAtTime t)) l ts

solve :: Int -> [Int] -> Int
solve c ns = produce c (time2list ns (replicate 9 0))

main :: IO ()
main = do  
        handle <- openFile "input6.txt" ReadMode
        contents <- hGetContents handle
        let nrs = f (splitOn "," contents)
        print (solve 80 nrs)
        print (solve 256 nrs)
        hClose handle   

f :: [String] -> [Int]
f = map read

-- 375482
-- 1689540415957

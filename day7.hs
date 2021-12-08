import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Data.List.Split (splitOn)

cost :: Int -> [Int] -> Int -> Int
cost _ [] c = c
cost t (p:ps) c = cost t ps (c + abs (p - t))

solve :: [Int] -> Int
solve ns = minimum [cost x ns 0| x <- [(minimum ns)..(maximum ns)]]

cost2 :: Int -> [Int] -> Int -> Int
cost2 _ [] c = c
cost2 t (p:ps) c = cost2 t ps (c + div (n*(n+1))  2)
  where n = abs (p - t)

solve2 :: [Int] -> Int
solve2 ns = minimum [cost2 x ns 0| x <- [(minimum ns)..(maximum ns)]]

main :: IO ()
main = do  
        handle <- openFile "input7.txt" ReadMode
        contents <- hGetContents handle
        let nrs = map read (splitOn "," contents) :: [Int]
        print (solve nrs)
        print (solve2 nrs)
        hClose handle   

-- 340052
-- 92948968

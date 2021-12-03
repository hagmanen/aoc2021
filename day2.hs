import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Data.List.Split (splitOn)

parseInstruction :: String -> (String, Int)
parseInstruction s = (head ss, read $ last ss :: Int)
  where ss = splitOn " " [c | c <- s, c /= '+']

dx :: (String, Int) -> Int
dx (c, a)
  | c == "forward" = a
  | otherwise = 0

dy :: (String, Int) -> Int
dy (c, a)
  | c == "up" = -a
  | c == "down" = a
  | otherwise = 0

solve :: Int -> Int -> [(String, Int)] -> Int
solve x y [] = x * y
solve x y (i:is) = solve (x + dx i) (y + dy i) is

solve2 :: Int -> Int -> Int -> [(String, Int)] -> Int
solve2 a x y [] = x * y
solve2 a x y (i:is)
  | fst i == "forward" = solve2 a (x + snd i) (y + a * snd i) is
  | fst i == "up" = solve2 (a - snd i) x y is
  | otherwise = solve2 (a + snd i) x y is

main :: IO ()
main = do  
        let list = []
        handle <- openFile "input2.txt" ReadMode
        contents <- hGetContents handle
        let instructions = [parseInstruction l | l <- lines contents]
        print (solve 0 0 instructions)
        print (solve2 0 0 0 instructions)
        hClose handle   

-- 1936494
-- 1997106066

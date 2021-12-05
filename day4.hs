import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Data.List.Split (splitOn)
import Data.List ( transpose )
import Data.ByteString.Builder (byteString)

parseNrs :: String -> [Int]
parseNrs s = [read x | x <- splitOn "," s]

parseRow :: String -> [(Int, Bool)]
parseRow r = [(read n, False) | n <- splitOn " " r, n /= ""]

parseBoard :: String -> [[(Int, Bool)]]
parseBoard b = [parseRow r | r <- splitOn "\n" b]

parseBoards :: [String] -> [[[(Int, Bool)]]]
parseBoards s = [parseBoard b | b <- s]

parse :: [String] -> ([Int], [[[(Int, Bool)]]])
parse (nrs:boards) = (parseNrs nrs, parseBoards boards)
parse _ = ([1], [[[(1,False)]]])

resultNr :: (Int, Bool) -> Int 
resultNr (n, False) = n
resultNr _ = 0

resultRow :: [(Int, Bool)] -> Int
resultRow = foldl (\s nr -> s + resultNr nr) 0 

result :: [[(Int, Bool)]] -> Int
result  = foldl (\s row -> s + resultRow row) 0 

rotateLeft :: [[a]] -> [[a]]
rotateLeft = reverse . transpose

bingoRow :: [(Int, Bool)] -> Bool
bingoRow = all snd

bingoRows :: [[(Int, Bool)]] -> Bool
bingoRows [] = False
bingoRows (r:rs)
  | bingoRow r = True
  | otherwise = bingoRows rs

bingo :: [[(Int, Bool)]] -> Bool
bingo b = bingoRows b  || bingoRows (rotateLeft b)

checkBoards :: [[[(Int, Bool)]]] -> (Bool, [[(Int, Bool)]])
checkBoards [] = (False, [[(0,False)]])
checkBoards (b:bs)
  | bingo b = (True, b)
  | otherwise = checkBoards bs

markNr :: Int -> (Int, Bool) -> (Int, Bool)
markNr nr bn
  | nr == fst bn = (nr, True)
  | otherwise = bn

markRow :: Int -> [(Int, Bool)] -> [(Int, Bool)]
markRow nr r = [markNr nr bn | bn <- r]

mark :: Int -> [[(Int, Bool)]] -> [[(Int, Bool)]]
mark nr b = [markRow nr r | r <- b]

solve :: [Int] -> [[[(Int, Bool)]]] -> Int
solve [] _ = 0
solve (n:ns) bs
  | fst res = n * result (snd res)
  | otherwise = solve ns nbs
  where nbs = [mark n b | b <- bs]
        res = checkBoards nbs

solve2 :: [Int] -> [[[(Int, Bool)]]] -> Int
solve2 [] _ = 0
solve2 ns [b] = solve ns [b]
solve2 (n:ns) bs = solve2 ns [b | b <- [mark n b | b <- bs], not (bingo b)]

main :: IO ()
main = do  
        handle <- openFile "input4.txt" ReadMode
        contents <- hGetContents handle
        let inst = parse (splitOn "\n\n" contents)
        print (uncurry solve inst)
        print (uncurry solve2 inst)
        hClose handle   

-- 64084
-- 12833

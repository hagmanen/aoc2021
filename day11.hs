import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Data.List.Index (setAt, modifyAt)
import Data.List(findIndex)
import Data.Maybe(isNothing)
import Data.List.Split(chunksOf)

string2Ints :: String -> [Int]
string2Ints = map (\x -> read [x] :: Int)

countFlashes :: [[Int]] -> Int
countFlashes m = sum [sum [if x == 0 then 1 else 0 | x <- row] | row <- m]

toCoord :: Maybe Int -> Int -> Maybe (Int, Int)
toCoord Nothing _ = Nothing
toCoord (Just x) l = Just (mod x l, div x l)

findFlash :: [[Int]] -> Maybe (Int, Int)
findFlash m = toCoord (findIndex (>9) (concat m)) (length (head m))

incAt :: (Int, Int) -> [[Int]] -> [[Int]]
incAt (x,y) m
  | x == -1 = m
  | y == -1 = m
  | x == w = m
  | y == length m = m
  | (m!!y)!!x == 0 = m
  | otherwise = chunksOf w (modifyAt (x + y * w) (+1) (concat m))
    where w = length (head m)

clearAt :: (Int, Int) -> [[Int]] -> [[Int]]
clearAt (x,y) m = chunksOf w (setAt (x + y * w) 0 (concat m))
  where w = length (head m)

doFlash :: Maybe(Int, Int) -> [[Int]] -> [[Int]]
doFlash Nothing m = m
doFlash (Just (x, y)) m = m9
  where m1 = incAt (x-1, y-1) m
        m2 = incAt (x, y-1) m1
        m3 = incAt (x+1, y-1) m2
        m4 = incAt (x-1, y) m3
        m5 = clearAt (x, y) m4
        m6 = incAt (x+1, y) m5
        m7 = incAt (x-1, y+1) m6
        m8 = incAt (x, y+1) m7
        m9 = incAt (x+1, y+1) m8

flash :: [[Int]] -> [[Int]]
flash m
  | isNothing f = m
  | otherwise = flash (doFlash f m)
  where f = findFlash m

step :: ([[Int]], Int) -> ([[Int]], Int)
step (m, f) = (m'', f + countFlashes m'')
  where m' = [[x+1 | x <- row] | row <- m]
        m'' = flash m'

solve :: ([[Int]], Int) -> Int -> ([[Int]], Int)
solve state 0 = state
solve state i = solve (step state) (i-1)

solve2 :: [[Int]] -> Int -> Int
solve2 m s
  | snd m' == length m * length (head m) = s
  | otherwise = solve2 (fst m') (s+1)
  where m' = step (m,0)

main :: IO ()
main = do  
        handle <- openFile "input11.txt" ReadMode
        contents <- hGetContents handle
        let input = [string2Ints row | row <- lines contents]
        print (snd (solve (input, 0) 100))
        print (solve2 input 1)
        hClose handle   

-- 1627
-- 329

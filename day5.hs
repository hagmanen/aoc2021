import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Data.List.Split (splitOn)
import Data.Map (fromListWith, toList)

parseCoord :: String -> (Int, Int)
parseCoord s = (read (head c), read (last c))
  where c = splitOn "," s

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine s = (parseCoord (head cs), parseCoord (last cs))
  where cs =  splitOn " -> " s

horizontalOrVertical :: ((Int, Int), (Int, Int)) -> Bool
horizontalOrVertical ((x1,y1),(x2,y2)) = x1 == x2 || y1 == y2

absRange :: Int -> Int -> [Int]
absRange a b
  | a < b = [a..b]
  | otherwise = [b..a]

lineToCoords :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
lineToCoords ((x1,y1),(x2,y2))
  | x1 == x2 || y1 == y2 = [(x, y) | x <- absRange x1 x2, y <- absRange y1 y2]
  | x1 - x2 == y1 - y2 && x1 - x2 > 0 = [(x1-d, y1-d) | d <- [0..x1-x2]]
  | x1 - x2 == y2 - y1 && x1 - x2 > 0 = [(x1-d, y1+d) | d <- [0..x1-x2]]
  | x1 - x2 == y1 - y2 && x1 - x2 < 0 = [(x1+d, y1+d) | d <- [0..x2-x1]]
  | x1 - x2 == y2 - y1 && x1 - x2 < 0 = [(x1+d, y1-d) | d <- [0..x2-x1]]
  | otherwise = []

linesToCoords :: [((Int, Int), (Int, Int))] -> [(Int, Int)]
linesToCoords = concatMap lineToCoords

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

filterDanger :: [(Int, Int)] -> [(Int, Int)]
filterDanger l = [fst x | x <- frequency l, snd x > 1]

solve :: [((Int, Int), (Int, Int))] -> Int
solve l = length (filterDanger (linesToCoords l))

main :: IO ()
main = do  
        handle <- openFile "input5.txt" ReadMode
        contents <- hGetContents handle
        let input = [parseLine l | l <- lines contents]
        print (solve [c | c <- input, horizontalOrVertical c])
        print (solve input)
        hClose handle   

-- 8111
-- 22088

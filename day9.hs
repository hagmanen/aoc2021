import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Data.List.Split (splitOn)
import Data.List (sort)
import Data.List.Index (setAt, modifyAt)
import Data.Bifunctor (first, second, bimap)

parseHights :: String -> [Int]
parseHights = map (\x -> read [x] :: Int)

getHight :: Int -> Int -> [[Int]] -> Int
getHight (-1) _ m = 10
getHight _ (-1) m = 10
getHight x y m
  | y == length m = 10
  | x == length (m !! y) = 10
  | otherwise = (m !! y) !! x

isLowPoint :: Int -> Int -> [[Int]] -> Bool
isLowPoint x y m = current < north && current < south && current < east && current < west 
  where current = getHight x y m
        north = getHight x (y+1) m
        south = getHight x (y-1) m
        east = getHight (x+1) y m
        west = getHight (x-1) y m

solve :: [[Int]] -> Int
solve [] = 0
solve m = sum [1 + getHight x y m | x <- [0..width], y <- [0..hight], isLowPoint x y m]
  where hight = length m - 1
        width = length (head m) - 1

parseBasins :: String -> [Bool]
parseBasins = map (\x -> (read [x] :: Int) == 9)

result :: [Int] -> Int
result l = product (take 3 (reverse (sort [x | x <-l, x/= 0])))

replace :: Int -> Int -> [Int] -> [Int]
replace old new l = [if x == old then new else x | x <- l]

incAt :: Int -> [Int] -> [Int]
incAt x = modifyAt x (+1)

clearAt :: Int -> [Int] -> [Int]
clearAt x = setAt x 0

mergeAndInc :: Int -> Int -> [Int] -> [Int]
mergeAndInc a b list = modifyAt b (+ ((list !! a) + 1)) (clearAt a list)

fillRow :: [Int] -> Int -> [Bool] -> ([Int],[Int]) -> ([Int],[Int])
fillRow [] _ _ above = above
fillRow (x:xs) left row above
  | row !! x =                fillRow xs 0 row (first (setAt x 0) above)
  | left == 0 && up == 0 =    fillRow xs next row (setAt x next (fst above), snd above ++ [1])
  | left == 0 || left == up = fillRow xs up row (second (incAt up) above)
  | left /= 0 && up == 0 =    fillRow xs left row (bimap (setAt x left) (incAt left) above)
  | otherwise =               fillRow xs up row (bimap (replace left up) (mergeAndInc left up) above)
  where next = length (snd above)
        up = fst above !! x

fill :: [[Bool]] -> ([Int],[Int]) -> Int
fill [] above = result (snd above)
fill (row:rows) above = fill rows above'
  where above' = fillRow [0..(length row - 1)] 0 row above

solve2 :: [[Bool]] -> Int
solve2 m = fill m (replicate (length (head m)) 0, [0])

main :: IO ()
main = do  
        handle <- openFile "input9.txt" ReadMode
        contents <- hGetContents handle
        print (solve [parseHights row | row <- lines contents])
        print (solve2 [parseBasins row | row <- lines contents])
        hClose handle   

-- 572
-- 847044

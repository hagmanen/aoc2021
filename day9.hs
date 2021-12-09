import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Data.List.Split (splitOn)
import Data.List (sort)
import Data.Sequence (update, fromList)
import Data.Foldable (toList)
import Distribution.Simple.LocalBuildInfo (absoluteComponentInstallDirs)
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

parseWalls :: String -> [Bool]
parseWalls = map (\x -> (read [x] :: Int) == 9)

result :: [Int] -> Int
result l = product (take 3 (reverse (sort l)))

setAbove :: Int -> Int -> [Int] -> [Int]
setAbove x val above = toList (update x val $ fromList above)

replaceAbove :: [Int] -> Int -> [Int] -> [Int]
replaceAbove old new l = [if x `elem` old then new else x | x <- l]

incBasin :: Int -> [Int] -> [Int]
incBasin x basins = toList (update x val $ fromList basins)
  where val = (basins !! x) + 1

clearBasins :: [Int] -> [Int] -> [Int]
clearBasins xs b = foldl (\ b x -> toList (update x 0 $ fromList b)) b xs

mergeBasins :: Int -> Int -> [Int] -> [Int]
mergeBasins a b basins = clearBasins [a,b] basins ++ [(basins !! a) + (basins !! b) + 1]

fillRow :: [Int] -> Int -> [Bool] -> ([Int],[Int]) -> ([Int],[Int])
fillRow [] _ _ above_basins = above_basins
fillRow (x:xs) prev row above_basins
  | row !! x =                      fillRow xs 0 row (first (setAbove x 0) above_basins)
  | prev == 0 && above_basin == 0 = fillRow xs next_basin row (setAbove x next_basin (fst above_basins), snd above_basins ++ [1])
  | prev == 0 && above_basin /= 0 = fillRow xs above_basin row (second (incBasin above_basin) above_basins)
  | prev /= 0 && above_basin == 0 = fillRow xs prev row (bimap (setAbove x prev) (incBasin prev) above_basins)
  | prev == above_basin = fillRow xs above_basin row (second (incBasin above_basin) above_basins)
  | otherwise = fillRow xs next_basin row (bimap (replaceAbove [above_basin,prev] next_basin) (mergeBasins prev above_basin) above_basins)
  where next_basin = length (snd above_basins)
        above_basin = fst above_basins !! x

fill :: [[Bool]] -> ([Int],[Int]) -> Int
fill [] above_basins = result (snd above_basins)
fill (row:rows) above_basins = fill rows above_basins'
  where above_basins' = fillRow [0..(length row - 1)] 0 row above_basins

solve2 :: [[Bool]] -> Int
solve2 m = fill m (replicate (length (head m)) 0, [0])

main :: IO ()
main = do  
        handle <- openFile "input9.txt" ReadMode
        contents <- hGetContents handle
        print (solve [parseHights row | row <- lines contents])
        print (solve2 [parseWalls row | row <- lines contents])
        hClose handle   

-- 572
-- 847044

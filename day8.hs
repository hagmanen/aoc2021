import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Data.List.Split (splitOn)
import Data.List (sortBy, sort)
import Data.List.Extra (comparingLength)
import Data.Sequence (update, fromList)
import Data.Foldable (toList)

sortChars :: String -> String
sortChars s = concat (sort (splitOn "" s))

parseSegments :: String -> [String]
parseSegments s = [sortChars s | s <- splitOn " " s]

parse :: [String] -> [([String], [String])]
parse [] = []
parse (x:xs) = (parseSegments (head parts), parseSegments (last parts)) : parse xs
  where parts = splitOn " | " x 

isEasy :: String -> Bool
isEasy s = length s `elem` [2, 3, 4, 7]

countEasy :: [String] -> Int
countEasy l = sum [fromEnum (isEasy s) | s <- l]

solve :: [[String]] -> Int
solve ls = sum [countEasy x | x <- ls]

myInsert :: String -> Int -> [String] -> [String]
myInsert s i decoder = toList (update i s $ fromList decoder)

containsChars :: String -> String -> Bool
containsChars s = all (`elem` s)

removeChars :: String -> String -> String
removeChars s cs = [c | c <- s, c `notElem` cs]

digitFor5 :: String -> [String] -> Int
digitFor5 s l
  | containsChars s (l !! 1) = 3
  | containsChars s (removeChars (l !! 4) (l !! 1)) = 5
  | otherwise = 2

digitFor6 :: String -> [String] -> Int
digitFor6 s l
  | containsChars s (l !! 4) = 9
  | containsChars s (l !! 1) = 0
  | otherwise = 6

createDecoder :: [String] -> [String] -> [String]
createDecoder [] decoder = decoder
createDecoder (s:ss) decoder 
  | l == 2 = createDecoder ss (myInsert s 1 decoder)
  | l == 3 = createDecoder ss (myInsert s 7 decoder)
  | l == 4 = createDecoder ss (myInsert s 4 decoder)
  | l == 5 = createDecoder ss (myInsert s (digitFor5 s decoder) decoder)
  | l == 6 = createDecoder ss (myInsert s (digitFor6 s decoder) decoder)
  | l == 7 = createDecoder ss (myInsert s 8 decoder)
  | otherwise = createDecoder ss decoder
    where l = length s

decodeSegment :: String -> [String] -> Int -> Int
decodeSegment _ [] _  = 0
decodeSegment s (d:ds) i 
  | s == d = i
  | otherwise = decodeSegment s ds (i + 1)

digitsToInt :: [Int] -> Int
digitsToInt = foldl1 (\x y -> 10*x+y)

decodeOutput :: [String] -> [String] -> Int
decodeOutput ss decoder = digitsToInt [decodeSegment s decoder 0 | s <- ss]

decode :: [String] -> [String]  -> Int
decode signal output = decodeOutput output (createDecoder (sortBy comparingLength signal) ["" | _ <- [0..9]])

solve2 :: [([String], [String])] -> Int
solve2 ls = sum [uncurry decode x | x <- ls]

main :: IO ()
main = do  
        handle <- openFile "input8.txt" ReadMode
        contents <- hGetContents handle
        let parts = parse (lines contents)
        print (solve [snd x | x <- parts])
        print (solve2 parts)
        hClose handle   

-- 421
-- 986163

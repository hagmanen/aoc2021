import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Data.List.Index (setAt, modifyAt)
import Data.List(findIndex)
import Data.Maybe(isNothing)
import Data.List.Split(chunksOf)

string2Ints :: String -> [Int]
string2Ints = map (\x -> read [x] :: Int)

countZeros :: [Int] -> Int
countZeros l = length $ filter (==0) l

incAt :: [Int] -> [Int] -> [Int]
incAt [] l = l
incAt (x:xs) l
  | (l!!x) == 0 = incAt xs l
  | otherwise = incAt xs (modifyAt x (+1) l)

clearAt :: Int -> [Int] -> [Int]
clearAt x = setAt x 0

xIf :: Int -> Bool -> [Int]
xIf _ False = []
xIf x True = [x]

neighbors :: Int -> Int -> [Int]
neighbors x w = concat [xIf (x - w) over,  xIf (x - w + 1) (over && right),
                        xIf (x + 1) right, xIf (x + 1 + w) (right && under),
                        xIf (x + w) under, xIf (x + w - 1) (under && left),
                        xIf (x - 1) left,  xIf (x - 1 - w) (left && over)]
  where over = div x w /= 0
        under = div x w /= (w-1)
        left = mod x w /= 0
        right = mod x w /= (w-1)

doFlash :: Maybe Int -> [Int] -> Int -> [Int]
doFlash Nothing l _ = l
doFlash (Just x) l w =  incAt n (clearAt x l)
  where n = neighbors x w

flash :: [Int] -> Int -> [Int]
flash l w
  | isNothing f = l
  | otherwise = flash (doFlash f l w) w
  where f = findIndex (>9) l

step :: ([Int], Int) -> Int -> ([Int], Int)
step (l, f) w = (l', f + countZeros l')
  where l' = flash [x+1 | x <- l] w

solve :: ([Int], Int) -> Int -> Int -> ([Int], Int)
solve state _ 0 = state
solve state w i = solve (step state w) w (i-1)

solve2 :: [Int] -> Int -> Int -> Int
solve2 l w s
  | snd l' == length l = s
  | otherwise = solve2 (fst l') w (s+1)
  where l' = step (l,0) w

main :: IO ()
main = do  
        handle <- openFile "input11.txt" ReadMode
        contents <- hGetContents handle
        let input = [string2Ints row | row <- lines contents]
        print (snd (solve (concat input, 0) (length (head input)) 100))
        print (solve2 (concat input) (length (head input)) 1)
        hClose handle   

-- 1627
-- 329

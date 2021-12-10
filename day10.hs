import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Data.List (sort)

isClosing :: Char -> Bool
isClosing c = c `elem` [')', ']', '>', '}']

invers :: Char -> Char
invers c
  | c == ')' = '('
  | c == ']' = '['
  | c == '}' = '{'
  | c == '>' = '<'
  | otherwise = 'E'

cost :: Char -> Int
cost c
  | c == ')' = 3
  | c == ']' = 57
  | c == '}' = 1197
  | c == '>' = 25137
  | otherwise = 0

illegal :: String -> String -> Char
illegal row stack
  | row == "" = 'o'
  | isClosing (head row)  && stack /= "" && invers (head row) == head stack = illegal (tail row) (tail stack)
  | isClosing (head row) = head row
  | otherwise = illegal (tail row) (head row : stack)

solve :: [String] -> Int
solve rows = sum [cost (illegal row "")| row <- rows]

incomplete :: String -> String -> String
incomplete row stack
  | row == "" = stack
  | isClosing (head row)  && stack /= "" && invers (head row) == head stack = incomplete (tail row) (tail stack)
  | isClosing (head row) = ""
  | otherwise = incomplete (tail row) (head row : stack)

compChar :: Char -> Int
compChar c
  | c == '(' = 1
  | c == '[' = 2
  | c == '{' = 3
  | c == '<' = 4
  | otherwise = 0

middle :: [a] -> [a]
middle xs = take (signum ((l + 1) `mod` 2) + 1) $ drop ((l - 1) `div ` 2) xs
  where l = length xs

compCost :: String -> Int -> Int
compCost "" co = co
compCost (c:cs) co = compCost cs ((5*co) + compChar c)

solve2 :: [String] -> Int
solve2 rows = head (middle (sort [r | r <- [compCost (incomplete row "") 0| row <- rows], r /= 0]))

solve3 :: [String] -> [Int]
solve3 rows = [compCost (incomplete row "") 0| row <- rows]

main :: IO ()
main = do  
        handle <- openFile "input10.txt" ReadMode
        contents <- hGetContents handle
        print (solve (lines contents))
        print (solve2 (lines contents))
        hClose handle   

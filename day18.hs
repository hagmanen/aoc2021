import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import Data.Char(isDigit)

data Expr = Num Int
          | Pair Expr Expr
          | Error String deriving Eq

instance Show Expr where
    show ex = expr2Str ex

parse :: String -> (Expr, String)
parse [] = (Error "Empty","")
parse (c:cs)
  | c == '[' = (Pair first second, rest''')
  | isDigit c  = (Num (read [c]), cs)
  | otherwise = (Error (c:cs),"")
    where (first, rest) = parse cs
          rest' = drop 1 rest
          (second, rest'') = parse rest'
          rest''' = drop 1 rest''

add :: Expr -> Expr -> Expr
add ex1 ex2 = reduce (Pair ex1 ex2)

reduce :: Expr -> Expr
reduce ex
  | is_exploded = reduce exploaded
  | is_split = reduce splited
  | otherwise = ex
    where (is_exploded, _, exploaded) = explode ex 0
          (is_split, splited) = split ex

left :: Expr -> Expr
left (Pair left right) = left
left ex = ex

right :: Expr -> Expr
right (Pair left right) = right
right ex = ex

setRight :: Expr -> Expr -> Expr
setRight (Num 0) ex = ex
setRight (Num i) (Num j) = Num (i+j)
setRight ex (Pair left right) = Pair left (setRight ex right)
setRight ex1 ex2 = ex2 

setLeft :: Expr -> Expr -> Expr
setLeft (Num 0) ex = ex
setLeft (Num i) (Num j) = Num (i+j)
setLeft ex (Pair left right) = Pair (setLeft ex left) right
setLeft ex1 ex2 = ex2 

expr2Str :: Expr -> String
expr2Str (Pair left right) = "[" ++ expr2Str left ++ "," ++ expr2Str right ++ "]"
expr2Str (Num n) = show n
expr2Str (Error s) = "Error: " ++ s

magnitude :: Expr -> Int
magnitude (Num n) = n
magnitude (Error s) = 0
magnitude (Pair left right) = (3 * magnitude left) + (2 * magnitude right)

explode :: Expr -> Int -> (Bool, Expr, Expr)
explode (Num n) _ = (False, Num 0, Num n)
explode (Pair ex1 ex2) depth
  | depth < 4 && l_is_exp = (True, Pair (left epl) (Num 0), Pair ex1' (setLeft (right epl) ex2))
  | depth < 4 && r_is_exp = (True, Pair (Num 0) (right epr),Pair (setRight (left epr) ex1) ex2')
  | depth >= 4 = (True, Pair ex1 ex2 , Num 0)
  | otherwise = (False, Num 0, Pair ex1 ex2)
  where (l_is_exp, epl, ex1') = explode ex1 (depth + 1)
        (r_is_exp, epr, ex2') = explode ex2 (depth + 1)
explode ex _ = (False, ex, ex)

split :: Expr -> (Bool, Expr)
split (Num n)
  | n >= 10  && even n = (True, Pair (Num n') (Num n'))
  | n >= 10 = (True, Pair (Num n') (Num (1+n')))
  | otherwise = (False, Num n)
  where n' = div n 2
split (Pair left right)
  | left_split = (True, Pair left' right)
  | right_split = (True, Pair left right')
  | otherwise = (False, Pair left right)
  where (left_split, left') = split left
        (right_split, right') = split right
split ex = (False, ex)

sumup :: Expr -> [Expr] -> Expr
sumup e [] = e
sumup se (e:es) = sumup (add se e) es

main :: IO ()
main = do
        handle <- openFile "input18.txt" ReadMode
        contents <- hGetContents handle
        let list = [fst e | e <- map parse (lines contents)]
            su = sumup (head list) (tail list)
            pairs =  [x| x <- mapM (const list) [1..2], head x /= head (tail x)]
        print (magnitude su)
        print (maximum [magnitude (add (head x) (head (tail x))) | x <- pairs])
        hClose handle

-- 4057
-- 4683

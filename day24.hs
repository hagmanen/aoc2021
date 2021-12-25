import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import Data.List.Split (splitOn)
import qualified Data.Set as Set

data State = State { w :: Int, x :: Int, y :: Int, z :: Int } deriving (Show, Eq, Ord)

data Reg = W | X | Y | Z | Const Int deriving Show

data Op = Inp Reg | Add Reg Reg | Mul Reg Reg | Div Reg Reg | Mod Reg Reg | Eql Reg Reg | Nop deriving Show

parseArg :: String -> Reg
parseArg "w" = W
parseArg "x" = X
parseArg "y" = Y
parseArg "z" = Z
parseArg i = Const (read i)

parseOp :: String -> Op
parseOp s
  | op == "inp" = Inp (parseArg (part !! 1))
  | op == "add" = Add (parseArg (part !! 1)) (parseArg (part !! 2))
  | op == "mul" = Mul (parseArg (part !! 1)) (parseArg (part !! 2))
  | op == "div" = Div (parseArg (part !! 1)) (parseArg (part !! 2))
  | op == "mod" = Mod (parseArg (part !! 1)) (parseArg (part !! 2))
  | op == "eql" = Eql (parseArg (part !! 1)) (parseArg (part !! 2))
  | otherwise = Nop
  where part = splitOn " " s
        op = head part

valid :: State -> Bool
valid s = z s == 0

set :: Int -> Reg -> State -> State
set w W s = State w (x s) (y s) (z s)
set x X s = State (w s) x (y s) (z s)
set y Y s = State (w s) (x s) y (z s)
set z Z s = State (w s) (x s) (y s) z
set _ (Const _) s = s

get :: Reg -> State -> Int
get W s = w s
get X s = x s
get Y s = y s
get Z s = z s
get (Const c) s = c

addO :: Reg -> Reg -> State -> State
addO r1 r2 s = set (get r1 s + get r2 s) r1 s

mulO :: Reg -> Reg -> State -> State
mulO r1 r2 s = set (get r1 s * get r2 s) r1 s

divO :: Reg -> Reg -> State -> State
divO r1 r2 s = set (div (get r1 s) (get r2 s)) r1 s

modO :: Reg -> Reg -> State -> State
modO r1 r2 s = set (mod (get r1 s) (get r2 s)) r1 s

eqlO :: Reg -> Reg -> State -> State
eqlO r1 r2 s = set (fromEnum (get r1 s == get r2 s)) r1 s

compute :: [Op] -> State -> ([Op], State)
compute [] s = ([], s)
compute (Inp x:os) s = (Inp x:os,s)
compute (Add r1 r2:os) s = compute os (addO r1 r2 s)
compute (Mul r1 r2:os) s = compute os (mulO r1 r2 s)
compute (Div r1 r2:os) s = compute os (divO r1 r2 s)
compute (Mod r1 r2:os) s = compute os (modO r1 r2 s)
compute (Eql r1 r2:os) s = compute os (eqlO r1 r2 s)
compute (Nop:os) s = compute os s

inputReg :: Op -> Reg
inputReg (Inp r) = r
inputReg _ = Const 0

exec :: [Op] -> State -> Int -> ([Op], State)
exec ops s i = (ops', set 0 W s')
  where (ops', s') = compute (tail ops) (set i (inputReg (head ops)) s)

execAll :: [Op] -> Set.Set State -> ([Op], Set.Set State)
execAll ops ss = (ops', us)
  where r = [exec ops s x | x <- [1..9], s <- Set.toList ss]
        ops' = fst (head r)
        us = Set.fromList [snd x| x <- r]

main :: IO ()
main = do
        handle <- openFile "input24.txt" ReadMode
        contents <- hGetContents handle
        let program = map parseOp (lines contents)
            state = State 0 0 0 0
            p1 = execAll program (Set.fromList [state])
            p2 = uncurry execAll p1
            p3 = uncurry execAll p2
            p4 = uncurry execAll p3
            p5 = uncurry execAll p4
            p6 = uncurry execAll p5
            p7 = uncurry execAll p6
            p8 = uncurry execAll p7
            p9 = uncurry execAll p8
            p10 = uncurry execAll p9
            p11 = uncurry execAll p10
            p12 = uncurry execAll p11
            p13 = uncurry execAll p12
            p14 = uncurry execAll p13
        print (length (snd p1))
        print (length (snd p2))
        print (length (snd p3))
        print (length (snd p4))
        print (length (snd p5))
        print (length (snd p6))
        print (length (snd p7))
        print (length (snd p8))
        print (length (snd p9))
        print (length (snd p10))
        print (length (snd p11))
        print (length (snd p12))
        print (length (snd p13))
        print (length (snd p14))
--        print (solve program state 0)
--        print (solve list (initialState list 1))
--        print (solve list (initialState list 3))
        hClose handle

-- 
-- 

import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import Data.List.Split (splitOn)

data Target = Target { x0 :: Int,
                       x1 :: Int,
                       y0 :: Int,
                       y1 :: Int} deriving Show

type Coord = (Int, Int)

data TargetHit  =  Before | Hit | Miss deriving  
                             (Read, Show, Eq, Ord, Enum, Bounded)

--target area: x=20..30, y=-10..-5
parseTarget :: String -> Target
parseTarget s = Target (read (head xs)) (read (last xs)) (read (head ys)) (read (last ys))
  where ps = [head (splitOn "," x) | x <- splitOn "=" s]
        xs = splitOn ".." (ps !! 1)
        ys = splitOn ".." (ps !! 2)

inTarget :: Coord -> Target -> Bool
inTarget (x, y) t = x >= x0 t && x <= x1 t && y >= y0 t && y <= y1 t

missedTarget :: Coord -> Target -> Bool
missedTarget (x, y) t = x > x1 t || y < y0 t

isHit :: Coord -> Target -> TargetHit
isHit c t
  | inTarget c t = Hit
  | missedTarget c t = Miss
  | otherwise  = Before

foo :: Int -> Int -> Int
foo t s
  | (t - s) < 0 = s
  | otherwise = foo (t - s) (s + 1)

maxHight :: Int -> Int
maxHight max_dy = sum [1..max_dy]

maxDX :: Target -> Int
maxDX = x1

minDX :: Target -> Int
minDX t = foo (x0 t) 1

maxDY :: Target -> Int
maxDY t = -(y0 t + 1)

minDY :: Target -> Int
minDY = y0

solve :: Target -> Int
solve t = maxHight $ maxDY t

coordAdd :: Coord -> Coord -> Coord
coordAdd (x1,y1) (x2,y2) = (x1+x2, y1+y2)

mutate :: Coord -> Coord
mutate (0, dy) = (0,dy-1)
mutate (dx,dy) = (dx-1, dy-1)

willHit :: Coord -> Coord -> Target -> Bool
willHit c dc t
  | isHit c t == Hit = True
  | isHit c t == Miss = False
  | otherwise = willHit (coordAdd c dc) (mutate dc) t

solve2 :: Target -> Int
solve2 t = length [(x,y) | x <- [minDX t..maxDX t], y <- [minDY t..maxDY t], willHit (0,0) (x,y) t]

main :: IO ()
main = do
        handle <- openFile "input17.txt" ReadMode
        contents <- hGetContents handle
        let target = parseTarget contents
        print (solve target)
        print (solve2 target)
        hClose handle

-- 3655
-- 1447

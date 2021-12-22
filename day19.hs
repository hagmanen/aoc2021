import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Data.List (permutations)

data Coord = Coord { x :: Int,
                     y :: Int,
                     z :: Int } deriving (Eq, Ord)

data Match = NoMatch | YesMatch Int Int deriving (Show, Eq)

instance Show Coord where
    show c = "[" ++ show (x c) ++ "," ++ show (y c) ++ "," ++ show (z c) ++ "]"

createCoord :: [String] -> Coord
createCoord ss = Coord (read (head ss)) (read (ss !! 1)) (read (ss !! 2))

toList :: Coord -> [Int]
toList c = [x c, y c, z c]

fromList :: [Int] -> Coord
fromList l = Coord (head l) (l !! 1) (l !! 2)

parse :: [String] -> [Coord] -> [Coord]
parse ss cs = foldr ((:) . createCoord . splitOn ",") cs ss

allFlips :: [[Int]]
allFlips = mapM (const [-1,1]) [1..3]

allPermutations :: Coord -> [[Int]]
allPermutations c = permutations (toList c)

multiC :: [Int] -> [Int] -> [Int]
multiC [x1,y1,z1] [x2,y2,z2] = [x1*x2,y1*y2,z1*z2]
multiC _ _ = [0,0,0]

multiS :: [Int] -> [[Int]] -> [[Int]] -> [[Int]]
multiS _ [] r = r
multiS c (f:fs) r = multiS c fs (r ++ [multiC c f])

multi :: [[Int]] -> [[Int]] -> [[Int]] -> [Coord]
multi [] _ r = [fromList x | x <- r]
multi (c:cs) fs r = multi cs fs (r ++ multiS c fs [])

allRotations :: Coord -> [Coord]
allRotations c = multi (allPermutations c) allFlips []

subCoord :: Coord -> Coord -> Coord
subCoord c1 c2 = Coord (x c1 - x c2) (y c1 - y c2) (z c1 - z c2)

addCoord :: Coord -> Coord -> Coord
addCoord c1 c2 = Coord (x c1 + x c2) (y c1 + y c2) (z c1 + z c2)

transformers ::[Coord] ->  Coord -> [Coord]
transformers cs c = [subCoord x c | x <- cs]

rotation :: [Coord] -> Int -> [Coord]
rotation cs n = [allRotations c !! n | c <- cs]

rot :: [Coord] -> (Coord, Int) -> [[Coord]]
rot cs (c, n) = [[addCoord t x | x <- cs] | t <- transformers cs c]

commonMatch :: Coord -> [Coord] -> [Coord] -> Int
commonMatch c cs1 cs2 = Set.size (Set.intersection (Set.fromList cs1) (Set.fromList cs2))


--                current ->Orig -> All rotations -> Match
maxCountCommon :: Coord -> [Coord] -> [[Coord]] -> Match
maxCountCommon _ _ [] = NoMatch
maxCountCommon c cs1 (cs2:cs2s)
  | match == NoMatch = maxCountCommon c cs1 cs2s
  | otherwise = match
    where match = commonMatch c cs1 cs2

findMatch2 :: [Int] -> Coord -> [Coord] -> [Coord] -> (Bool, Int, Int)
findMatch2 [] _ _ _ = (False, 0, 0)
findMatch2 (n:ns) c cs1 cs2
  | count = (True, trans, n)
  | otherwise = findMatch2 ns c cs1 cs2
    where (count, trans) = maxCountCommon cs1 (rot cs2 (c, n)) 0

findMatch1 :: Coord -> [Coord] -> [Coord] -> (Bool, (Int, Int))
findMatch1 _ [] _ = (False, (0, 0))
findMatch1 c cs1 cs2
  | m = (True, (t, n))
  | otherwise = (False, (0,0))
  where (m, t, n) = findMatch2 [0..41] c cs1 cs2

findMatch :: [Coord] -> [Coord] ->[Coord] -> (Int, Int)
findMatch [] _ _ = (0, -1)
findMatch (c1:cs1) ocs1 cs2
  | found = result
  | otherwise = findMatch cs1 ocs1 cs2
  where (found, result) = findMatch1 c1 ocs1 cs2 

s0Coords :: [Coord] -> (Coord, Int) -> [Coord]
s0Coords  cs (t,n) = [addCoord t c | c <- cs']
    where cs' = [allRotations c !! n | c <- cs]

main :: IO ()
main = do
        handle <- openFile "input19.txt" ReadMode
        contents <- hGetContents handle
        let info = splitOn "\n\n" contents
            probes = [parse (tail (lines s)) [] | s <- info]
--        print probes
        print (findMatch (head probes) (head probes) (head (tail probes)))
--        print (solve list (initialState list 1))
--        print (solve list (initialState list 3))
        hClose handle

-- 
-- 

import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import qualified Data.Set as Set

type Group = Set.Set (Int, Int)
data Cucumbers = Cucumbers {east :: Group, south :: Group} deriving Show

noCucumbers :: Cucumbers
noCucumbers = Cucumbers Set.empty Set.empty

parseRow :: String -> Int -> Int -> Cucumbers -> (Int, Cucumbers)
parseRow [] x y cs = (x, cs)
parseRow (c:cc) x y cs
  | c == 'v' = parseRow cc (x+1) y (Cucumbers (east cs) (Set.insert (x,y) (south cs)))
  | c == '>' = parseRow cc (x+1) y (Cucumbers (Set.insert (x,y) (east cs)) (south cs))
  | otherwise = parseRow cc (x+1) y cs

union :: Cucumbers -> Cucumbers -> Cucumbers
union a b = Cucumbers (Set.union (east a) (east b)) (Set.union (south a) (south b))

parseBoard :: [String] -> (Int, Int) -> Cucumbers -> ((Int, Int), Cucumbers)
parseBoard [] size cs = (size, cs)
parseBoard (r:rs) (x,y) cs = parseBoard rs (x', y+1) (cs `union` cs')
  where (x', cs') = parseRow r 0 y cs

stepSouth :: (Int, Int) -> (Int, Int) -> (Int, Int)
stepSouth (max_x, max_y) (x,y) = (x, mod (y+1) max_y)

stepEast :: (Int, Int) -> (Int, Int) -> (Int, Int)
stepEast (max_x, max_y) (x,y) = (mod (x+1) max_x, y)

occupied :: (Int, Int) -> Cucumbers -> Bool
occupied c cs = Set.member c (south cs) || Set.member c (east cs)

doSouthMove :: (Int, Int) -> [(Int,Int)] -> Cucumbers -> Cucumbers -> Cucumbers
doSouthMove max [] ref new = new
doSouthMove max (c:cs) ref new
  | ocu = doSouthMove max cs ref new
  | otherwise = doSouthMove max cs ref (Cucumbers (east new) (Set.insert n (Set.delete c (south new))))
  where n = stepSouth max c
        ocu = occupied n ref

doEastMove :: (Int, Int) -> [(Int,Int)] -> Cucumbers -> Cucumbers -> Cucumbers
doEastMove max [] ref new = new
doEastMove max (c:cs) ref new
  | ocu = doEastMove max cs ref new
  | otherwise = doEastMove max cs ref (Cucumbers (Set.insert n (Set.delete c (east new))) (south new))
  where n = stepEast max c
        ocu = occupied n ref

moveSouth :: Cucumbers -> (Int, Int) -> Cucumbers
moveSouth cs max = doSouthMove max (Set.toList (south cs)) cs cs

moveEast :: Cucumbers -> (Int, Int) -> Cucumbers
moveEast cs max = doEastMove max (Set.toList (east cs)) cs cs

move :: Cucumbers -> (Int, Int) -> Cucumbers
move cs max = moveSouth (moveEast cs max) max



solve :: Cucumbers -> (Int, Int) -> Int -> Int
solve c max i
  | east c' == east c && south c' == south c = i
  | otherwise = solve c' max (i+1)
  where c' = move c max 

main :: IO ()
main = do
        handle <- openFile "input25.txt" ReadMode
        contents <- hGetContents handle
        let (max, cucumbers) = parseBoard (lines contents) (0,0) noCucumbers
        print (solve cucumbers max 1)
--        print (solve list (initialState list 3))
        hClose handle

-- 
-- 

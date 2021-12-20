import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import qualified Data.Set as Set
import Data.Complex (imagPart)

type Coord = (Int, Int)

type Image = Set.Set (Int, Int)

toNum :: [Char] -> Int -> (Char -> Int) -> Int
toNum [] base map = 0
toNum s  base map = base * toNum (init s) base map + map(last s)

binToDec :: String -> Int
binToDec [] = 0
binToDec s  = toNum s 2 (\x -> if x == '0' then 0 else 1)

start :: Image -> Coord
start img = (minx, miny)
  where minx = minimum [fst x | x <- Set.toList img]
        miny = minimum [snd x | x <- Set.toList img]

end :: Image -> Coord
end img = (maxx, maxy)
  where maxx = maximum [fst x | x <- Set.toList img]
        maxy = maximum [snd x | x <- Set.toList img]

coordToBit :: Coord -> Image -> Char
coordToBit c img
  | Set.member c img = '1'
  | otherwise = '0'

coordAdd :: Coord -> Coord -> Coord
coordAdd (x1,y1) (x2,y2) = (x1+x2, y1+y2)

consideredCoords :: Coord -> [Coord]
consideredCoords c = [coordAdd c c' | c' <- [(-1,-1), (0,-1), (1,-1),(-1,0), (0,0), (1,0),(-1,1), (0,1), (1,1)]]

index :: Coord -> Image -> Int
index c img = binToDec [coordToBit c' img | c' <- consideredCoords c]

enhanced :: String -> Image -> Coord -> Bool
enhanced iea img c = e == '#'
  where i = index c img
        e = iea !! i

border :: Coord -> Coord -> Image
border (x1,y1) (x2,y2) = Set.fromList ([(x1,y)| y <- [y1..y2]] ++ [(x2,y)| y <- [y1..y2]] ++ [(x,y1)| x <- [x1..x2]] ++ [(x,y2)| x <- [x1..x2]])

fillBorder :: Bool -> (Coord,Coord) -> Image -> Image
fillBorder False  _ img = img
fillBorder True (s,e) img = Set.union img (border s e)

mutate :: String -> (Coord, Coord) -> Image -> Image
mutate iea (s,e) img = Set.fromList [(x,y)| x <- [fst s..fst e], y <- [snd s..snd e], enhanced iea img (x,y)]

incSize :: (Coord, Coord) -> (Coord, Coord)
incSize ((x1,y1), (x2,y2)) = ((x1-1,y1-1), (x2+1,y2+1))

solve :: String -> Image -> (Coord, Coord) -> Int -> Int
solve _ img _ 0 = Set.size img
solve iea img se n = solve iea img''' se' (n-1)
  where se' = incSize se
        img' = fillBorder (odd n && head iea == '#') se' img
        img'' = fillBorder (odd n && head iea == '#') (incSize se') img'
        img''' = mutate iea se' img''

rowToImage :: String -> Int -> Int -> Image -> Image
rowToImage [] _ _ img = img
rowToImage (c:cs) x y img
  | c == '#' = rowToImage cs (x+1) y (Set.insert (x,y) img)
  | otherwise = rowToImage cs (x+1) y img

inputToImage :: [String] -> Int -> Image -> Image
inputToImage [] _ img = img
inputToImage (row:rows) y img = inputToImage rows (y+1) (rowToImage row 0 y img)

main :: IO ()
main = do
        handle <- openFile "input20.txt" ReadMode
        contents <- hGetContents handle
        let iea = head (lines contents)
            image = inputToImage (drop 2 (lines contents)) 0 Set.empty
        print (solve iea image (start image, end image) 2)
        print (solve iea image (start image, end image) 50)
        hClose handle

-- 5347
-- 17172

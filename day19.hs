import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import Data.List (sort, nub)
import Data.List.Split (splitOn)
import Debug.Trace (trace)

type Coord = (Int,Int,Int)

allBeacons :: [[Coord]] -> ([Coord], [Coord])
allBeacons scanners = accMatches (sort (head scanners), [(0,0,0)]) (tail scanners)

merge :: [Coord] -> [Coord] -> [Coord]
merge (a:as) (b:bs)
  | a == b = a:merge as bs
  | a < b = a:merge as (b:bs)
  | otherwise = b:merge (a:as) bs
merge as [] = as
merge [] bs = bs

accMatches :: ([Coord], [Coord]) -> [[Coord]] -> ([Coord], [Coord])
accMatches matched scanners | trace ("Unmatched scanners: " ++ show (length scanners) ++ ", Matched scanners: " ++ show (length (snd matched))) False = undefined
accMatches ([],_) _ = error "Ran out of root nodes"
accMatches matched_beacons [] = matched_beacons
accMatches matched_beacons (scanner:scanners)
  | m = accMatches (matched_beacons', offsets) scanners
  | otherwise = accMatches matched_beacons (scanners ++ [scanner])
  where (m, rotated_beacons) = match (fst matched_beacons) (allRotations scanner)
        matched_beacons' = merge (fst matched_beacons) (snd rotated_beacons)
        offsets = fst rotated_beacons : snd matched_beacons

applyOffset :: Coord -> [[Coord]] -> [[Coord]]
applyOffset offset = map (map (add offset))

match :: [Coord] -> [[Coord]] -> (Bool, (Coord, [Coord]))
match _ [] = (False, ((0,0,0),[]))
match matched_beacons (rotated_beacons:rbs)
  | m = (True, new_beacons)
  | otherwise = match matched_beacons rbs
  where (m, new_beacons) = matchOffset matched_beacons (allOffsets matched_beacons rotated_beacons)

matchOffset :: [Coord] -> [(Coord, [Coord])] -> (Bool, (Coord, [Coord]))
matchOffset _ [] = (False, ((0,0,0),[]))
matchOffset matched_beacons (off:offs)
  | c >= 12 = (True, off)
  | otherwise = matchOffset matched_beacons offs
  where c = countMatches matched_beacons (snd off) 0

countMatches :: [Coord] -> [Coord] -> Int -> Int
countMatches (a:as) (b:bs) m
  | a == b = countMatches as bs (m+1)
  | a < b = countMatches as (b:bs) m
  | otherwise = countMatches (a:as) bs m
countMatches _ _ m = m

sub :: Coord -> Coord -> Coord
sub (x1,y1,z1) (x2,y2,z2) = (x1-x2, y1-y2, z1-z2)

add :: Coord -> Coord -> Coord
add (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)

allOffsets :: [Coord] -> [Coord] -> [(Coord, [Coord])]
allOffsets matched_beacons scanner = map (\o -> (o, map (add o) scanner)) offsets
  where offsets = nub [sub m b| m <- matched_beacons, b <- scanner]

allRotations :: [Coord] -> [[Coord]]
allRotations scanner = map (\f -> sort $ map f scanner) cubeRotations

cubeRotations :: [Coord -> Coord]
cubeRotations = concat [t1,t2,t3,t4,t5,t6]
  where t1 = repeatF 4 xTurn
        t2 = map (. zTurn) t1
        t3 = map (. (zTurn . zTurn)) t1
        t4 = map (. (zTurn . zTurn . zTurn)) t1
        t5 = map (. yTurn) t1
        t6 = map (. (yTurn . yTurn . yTurn)) t1

repeatF :: Int -> (Coord -> Coord) -> [Coord -> Coord]
repeatF n f = scanl (\acc _ -> f . acc) id [0..(n-2)]

xTurn :: Coord -> Coord
xTurn (x,y,z) = (x,z,-y)

yTurn :: Coord -> Coord
yTurn (x,y,z) = (-z,y,x)

zTurn :: Coord -> Coord
zTurn (x,y,z) = (y,-x,z)

parseCoord :: String -> Coord
parseCoord s = (head ns, ns !! 1, last ns)
  where ns = map read (splitOn "," s)

parseScanner :: String -> [Coord]
parseScanner s = map parseCoord (tail (lines s))

parse :: String -> [[Coord]]
parse s = map parseScanner (splitOn "\n\n" s)

dist :: Coord -> Coord -> Int
dist (x1,y1,z1) (x2,y2,z2) = abs (x1-x2) + abs (y1-y2) + abs (z1-z2)

dists :: [Coord] -> [Int]
dists ss = [dist x y | x <- ss, y <- ss]

main :: IO ()
main = do
        handle <- openFile "input19.txt" ReadMode
        contents <- hGetContents handle
        let scanners = parse contents
            (beacons,offsets) = allBeacons scanners
        print (length beacons)
        print (maximum (dists offsets))
        hClose handle

-- 465
-- 12149

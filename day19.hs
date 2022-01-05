import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import Data.List (sort, nub)
import Data.List.Split (splitOn)
import Debug.Trace (trace)

type Coord = (Int,Int,Int)
type Scanner = (Coord, [Coord])

solve :: [[Coord]] -> [Scanner] -> [Scanner] -> [Scanner]
solve unmatched matched done | trace ("Unmatched scanners: " ++ show (length unmatched) ++
                                      ", Matched scanners: " ++ show (length matched) ++
                                      ", Done scanners: " ++ show (length done)) False = undefined
solve [] matched done = matched ++ done
solve _ [] _ = error "Empty list of matched scanners"
solve unmatched (ref:refs) done = solve unmatched' matched' (ref:done)
  where (matched', unmatched') = matchWithScanner ref unmatched refs []

matchWithScanner :: Scanner -> [[Coord]] -> [Scanner] -> [[Coord]] -> ([Scanner], [[Coord]])
matchWithScanner _ [] matched unmatched = (matched, unmatched)
matchWithScanner ref (s:ss) matched unmatched
  | m = matchWithScanner ref ss (matched_scanner:matched) unmatched
  | otherwise = matchWithScanner ref ss matched (s:unmatched)
  where (m, matched_scanner) = match (snd ref) (allRotations s)

match :: [Coord] -> [[Coord]] -> (Bool, Scanner)
match _ [] = (False, ((0,0,0),[]))
match ref (rotated_beacons:rbs)
  | m = (True, new_beacons)
  | otherwise = match ref rbs
  where (m, new_beacons) = matchOffset ref (allOffsets ref rotated_beacons)

matchOffset :: [Coord] -> [(Coord, [Coord])] -> (Bool, (Coord, [Coord]))
matchOffset _ [] = (False, ((0,0,0),[]))
matchOffset ref (off:offs)
  | c >= 12 = (True, off)
  | otherwise = matchOffset ref offs
  where c = countMatches ref (snd off) 0

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
allOffsets ref scanner = map (\o -> (o, map (add o) scanner)) offsets
  where offsets = nub [sub m b| m <- ref, b <- scanner]

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
        let unmatched = parse contents
            ref = ((0,0,0), sort (head unmatched)) :: Scanner
            scanners = solve (tail unmatched) [ref] []
        print (length (nub (concat [snd x | x <- scanners])))
        print (maximum (dists [fst x | x <- scanners]))
        hClose handle

-- 465
-- 12149

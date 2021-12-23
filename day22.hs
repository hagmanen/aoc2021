import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import Data.List.Split (splitOn)
import qualified Data.Set as Set

type CubeSet = Set.Set (Int, Int, Int)
type Cuboid = ((Int, Int), (Int, Int), (Int, Int))
type Inst = (Bool, Cuboid)

parseRange :: String -> (Int, Int)
parseRange s = (read (head ns), read (last ns))
  where ns = splitOn ".." (drop 2 s)

parseCuboid :: String -> Cuboid
parseCuboid s = (parseRange (head rs), parseRange (rs !! 1), parseRange (last rs))
  where rs = splitOn "," s

parseRow :: String -> Inst
parseRow s = (on, cuboid)
  where ss = splitOn " " s
        on = "on" == head ss
        cuboid = parseCuboid (last ss) 

parse :: [String] -> [Inst] -> [Inst]
parse rows inst = foldl (\ inst row -> inst ++ [parseRow row]) inst rows

limit :: Int -> Int
limit v
  | v < -50 = -50
  | v > 50 = 50
  | otherwise = v

validRange :: (Int, Int) -> (Bool, (Int, Int))
validRange (from, to)
  | from < -50 && to < -50 = (False, (0,0))
  | from > 50 && to > 50 = (False, (0,0))
  | otherwise = (True, (limit from, limit to))

validInst :: Inst -> (Bool, Inst)
validInst (on, (xr, yr, zr)) = (vx && vy && vz, (on, (xr', yr', zr')))
  where (vx, xr') = validRange xr
        (vy, yr') = validRange yr
        (vz, zr') = validRange zr

insertCuboid :: Cuboid -> CubeSet -> CubeSet
insertCuboid ((x1,x2), (y1,y2), (z1,z2)) cubes = Set.union cubes (Set.fromList [(x,y,z)| x <- [x1..x2], y <- [y1..y2], z <- [z1..z2]])

removeCuboid :: Cuboid -> CubeSet -> CubeSet
removeCuboid ((x1,x2), (y1,y2), (z1,z2)) cubes = Set.difference cubes (Set.fromList [(x,y,z)| x <- [x1..x2], y <- [y1..y2], z <- [z1..z2]])

alterCuboid :: Inst -> CubeSet -> CubeSet
alterCuboid (on, cuboid) cubes
  | on = insertCuboid cuboid cubes
  | otherwise = removeCuboid cuboid cubes

solve :: [Inst] -> CubeSet -> Int
solve [] cubes = Set.size cubes
solve (inst:insts) cubes = solve insts (alterCuboid inst cubes)

main :: IO ()
main = do
        handle <- openFile "input22.txt" ReadMode
        contents <- hGetContents handle
        let insts = [snd vi | vi <- [validInst i | i <- parse (lines contents) []], fst vi]
        print (solve insts Set.empty)
        hClose handle

-- 568000
-- 

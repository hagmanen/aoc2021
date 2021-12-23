import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import Data.List.Split (splitOn)
import qualified Data.Set as Set

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

type ZItem = (Int, Int)
type ZList = [ZItem]
type YItem = ((Int, Int), ZList)
type YList = [YItem]
type XItem = ((Int, Int), YList)
type XList = [XItem]

rangeSize :: (Int, Int) -> Int
rangeSize (from,to) = (to - from) + 1

countZ :: ZList -> Int
countZ zlist = sum [rangeSize range| range <- zlist]

countY :: YList -> Int
countY ylist = sum [rangeSize range * countZ zlist | (range, zlist) <- ylist]

count :: XList -> Int
count xlist = sum [rangeSize range * countY ylist | (range, ylist) <- xlist]

instToXList :: Inst -> XList
instToXList (on, (xr, yr, zr))
  | on = [(xr, [(yr, [zr])])]
  | otherwise = []

instToYList :: Inst -> YList
instToYList (on, (xr, yr, zr))
  | on = [(yr, [zr])]
  | otherwise = []

instToZList :: Inst -> ZList
instToZList (on, (xr, yr, zr))
  | on = [zr]
  | otherwise = []

xList :: Int -> Int -> YList -> XList
xList from to ylist
  | to < from = []
  | otherwise = [((from, to), ylist)]

yList :: Int -> Int -> ZList -> YList
yList from to zlist
  | to < from = []
  | otherwise = [((from, to), zlist)]

zList :: Int -> Int -> ZList
zList from to
  | to < from = []
  | otherwise = [(from, to)]

xListI :: Int -> Int -> Inst -> XList
xListI from to (on, (xr, yr, zr))
  | on = xList from to [(yr, [zr])]
  | otherwise = []

yListI :: Int -> Int -> Inst -> YList
yListI from to (on, (xr, yr, zr))
  | on = yList from to [zr]
  | otherwise = []

zListI :: Int -> Int -> Inst -> ZList
zListI from to (on, (xr, yr, zr))
  | on = zList from to
  | otherwise = []

restInstX :: Int -> Int -> Inst -> Inst
restInstX from to (on, ((bfrom, bto), yr, zr)) = (on, ((from, to), yr, zr))

restInstY :: Int -> Int -> Inst -> Inst
restInstY from to (on, (xr, yr, zr)) = (on, (xr, (from, to), zr))

restInstZ :: Int -> Int -> Inst -> Inst
restInstZ from to (on, (xr, yr, zr)) = (on, (xr, yr, (from, to)))

zOver :: Int -> Int -> Inst -> ZList
zOver from to (on, _)
  | to < from || not on = []
  | otherwise = [(from, to)]

alterCuboidZZ :: ZItem -> Inst -> (Bool, ZList, Inst)
alterCuboidZZ zitem inst
  | ato < bfrom = (done, [zitem], inst)
  | bto < afrom = (done, instToZList inst ++ [zitem], inst)
  | otherwise  = (done, zlist', inst')
  where (afrom,ato) = zitem
        (on, (xr, yr, (bfrom, bto))) = inst
        done = bto <= ato
        fa = zList afrom (bfrom-1)
        fb = zListI bfrom (afrom-1) inst
        over = zOver (max afrom bfrom) (min ato bto) inst
        inst' = restInstZ (ato+1) bto inst
        aa = zList (bto+1) ato
        zlist' = fa ++ fb ++ over ++ aa

alterCuboidZ :: ZList -> Inst -> ZList -> ZList
alterCuboidZ [] inst zout = zout ++ instToZList inst
alterCuboidZ (zr:zrs) inst zout
  | done = zout ++ zout' ++ zrs
  | otherwise = alterCuboidZ zrs inst' (zout ++ zout')
  where (done, zout', inst') = alterCuboidZZ zr inst

yOver :: Int -> Int -> ZList -> Inst -> YList
yOver from to zlist inst
  | to < from = []
  | otherwise = [((from, to), alterCuboidZ zlist inst [])]

alterCuboidYY :: YItem -> Inst -> (Bool, YList, Inst)
alterCuboidYY yitem inst
  | ato < bfrom = (done, [yitem], inst)
  | bto < afrom = (done, instToYList inst ++ [yitem], inst)
  | otherwise  = (done, ylist', inst')
  where ((afrom,ato), zlist) = yitem
        (on, (xr, (bfrom, bto), zr)) = inst
        done = bto <= ato
        fa = yList afrom (bfrom-1) zlist
        fb = yListI bfrom (afrom-1) inst
        over = yOver (max afrom bfrom) (min ato bto) zlist inst
        inst' = restInstY (ato+1) bto inst
        aa =yList (bto+1) ato zlist
        ylist' = fa ++ fb ++ over ++ aa

alterCuboidY :: YList -> Inst -> YList -> YList
alterCuboidY [] inst yout = yout ++ instToYList inst
alterCuboidY (yr:yrs) inst yout
  | done = yout ++ yout' ++ yrs
  | otherwise = alterCuboidY yrs inst' (yout ++ yout')
  where (done, yout', inst') = alterCuboidYY yr inst

xOver :: Int -> Int -> YList -> Inst -> XList
xOver from to ylist inst
  | to < from = []
  | otherwise = [((from, to), alterCuboidY ylist inst [])]

alterCuboidX :: XItem -> Inst -> (Bool, XList, Inst)
alterCuboidX xitem inst
  | ato < bfrom = (done, [xitem], inst)
  | bto < afrom = (done, instToXList inst ++ [xitem], inst)
  | otherwise  = (done, xlist', inst')
  where ((afrom,ato), ylist) = xitem
        (on, ((bfrom, bto), yr, zr)) = inst
        done = bto <= ato
        fa = xList afrom (bfrom-1) ylist
        fb = xListI bfrom (afrom-1) inst
        over = xOver (max afrom bfrom) (min ato bto) ylist inst
        inst' = restInstX (ato+1) bto inst
        aa =xList (bto+1) ato ylist
        xlist' = fa ++ fb ++ over ++ aa

alterCuboid :: XList -> Inst -> XList -> XList
alterCuboid [] inst xout = xout ++ instToXList inst
alterCuboid (xr:xrs) inst xout
  | done = xout ++ xout' ++ xrs
  | otherwise = alterCuboid xrs inst' (xout ++ xout')
  where (done, xout', inst') = alterCuboidX xr inst

solve :: [Inst] -> XList -> Int
solve [] cubes = count cubes
solve (inst:insts) cubes = solve insts (alterCuboid cubes inst [])

main :: IO ()
main = do
        handle <- openFile "input22.txt" ReadMode
        contents <- hGetContents handle
        let insts = parse (lines contents) []
        print (solve ([snd vi | vi <- [validInst i | i <- insts], fst vi]) [])
        print (solve insts [])
        hClose handle

-- 568000
-- 1177411289280259

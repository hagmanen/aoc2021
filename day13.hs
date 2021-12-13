import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.List.Index (setAt,modifyAt)
import Data.Bifunctor(bimap)

parseCoord :: String -> (Int, Int)
parseCoord s = (head s', last s')
  where s' = map read (splitOn "," s) :: [Int]

parseCoords :: Set.Set (Int,Int) -> [String] -> Set.Set (Int,Int)
parseCoords = foldl (\ s c -> Set.insert (parseCoord c) s)

parseFold :: String -> (Int, Int)
parseFold s
  | c == 'x' = (n, 0)
  | otherwise = (0, n)
  where s' = splitOn "=" (drop 11 s)
        c = head (head s')
        n = read (last s') :: Int

parseFolds :: [(Int,Int)] -> [String] -> [(Int,Int)]
parseFolds = foldl (\ fs l -> fs ++ [parseFold l])

parse :: String -> (Set.Set (Int,Int), [(Int,Int)])
parse s = (parseCoords Set.empty (lines (head s')), parseFolds [] (lines (last s')))
  where s' = splitOn "\n\n" s

foldValue :: Int -> Int -> Int
foldValue f n
  | f /= 0 && n > f = (2 * f) - n
  | otherwise = n

foldPaper :: (Int,Int) -> Set.Set (Int,Int) -> Set.Set (Int,Int)
foldPaper (x,y) = Set.map (bimap (foldValue x) (foldValue y))

solve :: Set.Set (Int,Int) -> [(Int,Int)] -> Set.Set (Int,Int)
solve = foldl (flip foldPaper)

markX :: Int -> String -> String
markX x = setAt x '#'

markSpot :: (Int, Int) -> [String] -> [String]
markSpot (x,y) = modifyAt y (markX x)

markSpots :: [String] -> [(Int,Int)] -> [String]
markSpots = foldl (flip markSpot)

makeOutput :: Set.Set (Int,Int) -> [String]
makeOutput s = markSpots blank l
  where l = Set.toList s
        w = maximum [x | (x,y) <- l] + 1
        h = maximum [y | (x,y) <- l] + 1
        blank = replicate h (replicate w '.')

main :: IO ()
main = do
        handle <- openFile "input13.txt" ReadMode
        contents <- hGetContents handle
        let info = parse contents
        print (Set.size (solve (fst info) [head (snd info)]))
        mapM_ print (makeOutput (uncurry solve info))
        hClose handle

-- 701
-- FPEKBEJL

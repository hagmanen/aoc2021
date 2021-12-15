import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import Data.List.Split (splitOn)

type Coord = (Int, Int)
type RiskMap = HashMap.HashMap (Int, Int) Int
type ScoreMap = HashMap.HashMap (Int, Int) Int

parseRow :: String -> Int -> Int -> RiskMap -> RiskMap
parseRow [] _ _ rm = rm
parseRow (c:cs) x y rm = parseRow cs (x+1) y (HashMap.insert key risk rm)
  where key = (x,y)
        risk = read [c]

parse :: [String] -> Int -> RiskMap -> RiskMap
parse [] _ rm = rm
parse (row:rows) y rm = parse rows (y+1) (parseRow row 0 y rm)

coordAdd :: Coord -> Coord -> Coord
coordAdd (x1, y1) (x2, y2) = (x1+x2, y1+y2)

availableSteps :: [Coord]
availableSteps = [(1,0), (0,1)]

moves :: Coord -> Int -> RiskMap -> [(Coord, Int)]
moves coord score risk = steps'
  where steps = [coordAdd coord s | s <- availableSteps]
        steps' = [(c, score + HashMap.lookupDefault 0 c risk)| c <- steps, HashMap.member c risk]

isImproved :: (Coord, Int) -> ScoreMap -> Bool
isImproved (coord, risk) score
  | HashMap.member coord score = risk < HashMap.lookupDefault 0 coord score
  | otherwise = True

updateScore :: [(Coord, Int)] -> ScoreMap -> ScoreMap
updateScore [] score = score
updateScore ((coord,risk):cs) score = updateScore cs (HashMap.insert coord risk score)

step :: [Coord] -> ScoreMap -> RiskMap -> ([Coord], ScoreMap) 
step [] score risk = ([], score)
step (c:cs) score risk = step cs' score' risk
   where step_moves = moves c (HashMap.lookupDefault 0 c score) risk
         improved = [sm | sm <- step_moves, isImproved sm score]
         cs' = cs ++ [fst c| c <- improved]
         score' = updateScore improved score

solve :: RiskMap -> Coord -> Int
solve risk end = result
  where (_, score) = step [(0,0)] HashMap.empty risk
        result = HashMap.lookupDefault 0 end score

incRow :: [Int] -> Int -> Int -> [Int]
incRow rs s i =  concat [[1 + mod (r+j-1) 9 | r <- rs] | j <- [s..i]]

parseRawRow :: String -> [Int]
parseRawRow s = [read [c]| c <- s]

intListToRisk :: [Int] -> Int -> Int -> RiskMap -> RiskMap
intListToRisk [] _ _ rm = rm
intListToRisk (c:cs) x y rm = intListToRisk cs (x+1) y (HashMap.insert key risk rm)
  where key = (x,y)
        risk = c

intMatrixToRisk :: [[Int]] -> Int -> RiskMap -> RiskMap
intMatrixToRisk [] _ rm = rm
intMatrixToRisk (row:rows) y rm = intMatrixToRisk rows (y+1) (intListToRisk row 0 y rm)

bigRisk :: [String] -> RiskMap
bigRisk ss = result
  where risk = [parseRawRow s | s <- ss]
        big_risk = [incRow r 0 4 | r <- risk] ++
                   [incRow r 1 5 | r <- risk] ++
                   [incRow r 2 6 | r <- risk] ++
                   [incRow r 3 7 | r <- risk] ++
                   [incRow r 4 8 | r <- risk]
        result = intMatrixToRisk big_risk 0 HashMap.empty 
        
main :: IO ()
main = do
        handle <- openFile "input15.txt" ReadMode
        contents <- hGetContents handle
        let text = lines contents
            riskmap = parse text 0 HashMap.empty
            end = coordAdd (length text, length (head text)) (-1,-1)
            big_risk = bigRisk text
            big_end = ((5*(1+fst end))-1, (5*(1+snd end))-1)
        print (solve riskmap end)
        print (solve big_risk big_end)
        hClose handle

-- 458
-- 2820

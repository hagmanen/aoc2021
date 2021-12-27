import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import Data.List.Split (splitOn)
import qualified Data.Set as Set

type Params = (Int, Int, Int)

parseParam :: Int -> [String] -> Int
parseParam n ss = read (drop 6 (ss !! n))

parseStep :: [String] -> Params
parseStep ss = (parseParam 4 ss, parseParam 5 ss, parseParam 15 ss)

step :: Int -> Int -> Params -> Int
step w z (a, b, c) = (div z a * ((25 * d) + 1)) + (d * (w + c))
  where d = fromEnum (mod z 26 + b /= w)

superFilter :: [(Int, Int)] -> Set.Set Int -> [(Int, Int)]
superFilter [] _ = []
superFilter ((key, z):ss) zs
  | Set.member z zs = superFilter ss zs
  | otherwise = (key, z):superFilter ss (Set.insert z zs)

reverseIf :: Bool -> [a] -> [a]
reverseIf b l
  | b = reverse l
  | otherwise = l

steps :: [(Int, Int)] -> Params -> Bool -> [(Int, Int)]
steps state params largest = superFilter [((key * 10) + w, step w z params) | (key, z) <- state, w <- reverseIf largest [1..9]] Set.empty

solve :: [(Int, Int)] -> [Params] -> Bool -> [(Int, Int)]
solve r [] _ = r
solve r (p:ps) l = solve (steps r p l) ps l

main :: IO ()
main = do
        handle <- openFile "input24.txt" ReadMode
        contents <- hGetContents handle
        let program = map (parseStep . lines) (tail (splitOn "inp" contents))
        print (head [fst r | r <- solve [(0,0)] program True, snd r == 0])
        print (head [fst r | r <- solve [(0,0)] program False, snd r == 0])
        hClose handle

-- 79997391969649
-- 16931171414113

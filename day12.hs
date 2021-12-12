import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import qualified Data.HashMap.Strict as HashMap
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Data.Char(isLower)

rms :: [String] -> [String]
rms ss = [s | s <- ss, s /= "start"]

appendL :: Maybe [String] -> String -> [String]
appendL Nothing s = rms [s]
appendL (Just v) s = v ++ rms [s]

append :: String -> String -> HashMap.HashMap String [String] -> HashMap.HashMap String [String]
append k v hm = HashMap.insert k (appendL (HashMap.lookup k hm) v) hm

parse :: [String] -> HashMap.HashMap String [String] -> HashMap.HashMap String [String]
parse [] hm = hm
parse (r:rs) hm = parse rs (append (last p) (head p) (append (head p) (last p) hm))
  where p = splitOn "-" r

ll :: Maybe [String] -> [String]
ll Nothing = []
ll (Just l) = l

insertLower :: String -> (Set.Set String, Int) -> (Set.Set String, Int)
insertLower s ss
  | Set.member s (fst ss) = (fst ss, snd ss - 1)
  | isLower (head s) = (Set.insert s (fst ss), snd ss)
  | otherwise = ss

findPaths :: String -> HashMap.HashMap String [String] -> (Set.Set String, Int) -> Int
findPaths "end" _ _ = 1
findPaths p hm v = sum ([findPaths x hm v' | x <- ll (HashMap.lookup p hm), snd v' /= 0 || not (Set.member x (fst v'))])
  where v' = insertLower p v

solve :: HashMap.HashMap String [String] -> Int -> Int
solve hm c = findPaths "start" hm (Set.empty, c)

main :: IO ()
main = do  
        handle <- openFile "input12.txt" ReadMode
        contents <- hGetContents handle
        let setup = parse (lines contents) HashMap.empty
        print (solve setup 0)
        print (solve setup 1)
        hClose handle   

-- 5252
-- 147784

import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import Data.List.Split (splitOn)

type Seq = (Char, Char)
type SeqCount = (Seq, Int)
type SeqCounts = HashMap.HashMap Seq Int
type RuleMap = HashMap.HashMap Seq (Seq, Seq)
type ResultMap = HashMap.HashMap Char Int

parseSeq :: String -> [Seq] -> [Seq]
parseSeq [] ss = ss
parseSeq (c:cs) ss = parseSeq cs (ss ++ [(c, nc)])
  where nc = if null cs then '.' else head cs
        
toSeqCount :: [Seq] -> SeqCounts
toSeqCount xs = HashMap.fromListWith (+) [(x, 1) | x <- xs]

toRule :: Char -> Char -> Char -> (Seq ,(Seq, Seq))
toRule c1 c2 c3 = ((c1,c2), ((c1,c3), (c3,c2)))

parseRules :: [String] -> RuleMap
parseRules s = rules'
  where rules = [(head (head p), last (head p), head (last p)) | p <- map (splitOn " -> ") s]
        rules' = HashMap.fromList [toRule c1 c2 c3 | (c1,c2,c3) <- rules]

parse :: [String] -> (SeqCounts, RuleMap)
parse s = (toSeqCount(parseSeq (head s) []), parseRules (drop 2 s))

incMaybe :: Int -> Maybe Int -> Maybe Int
incMaybe n Nothing = Just n
incMaybe n (Just i) = Just (n + i)

mutate :: Seq -> Int -> Maybe (Seq, Seq) -> SeqCounts -> SeqCounts
mutate s c Nothing sc = HashMap.alter (incMaybe c) s sc
mutate s c (Just (s1,s2)) sc = sc''
  where sc' = HashMap.alter (incMaybe c) s1 sc
        sc'' = HashMap.alter (incMaybe c) s2 sc'

step :: [SeqCount] -> RuleMap -> SeqCounts -> SeqCounts
step [] _ scs = scs
step (s:ss) r scs = step ss r (mutate key count rule scs)
    where key = fst s
          count = snd s
          rule = HashMap.lookup key r

toResult :: [SeqCount] -> ResultMap -> ResultMap
toResult [] r = r
toResult (c:cs) r = toResult cs (HashMap.alter (incMaybe val) key r)
  where val = snd c
        seq = fst c
        key = fst seq

toScore :: ResultMap -> Int
toScore r = score
  where maxC = maximum [snd x | x <- HashMap.toList r]
        minC = minimum [snd x | x <- HashMap.toList r]
        score = maxC - minC

solve :: SeqCounts -> RuleMap -> Int -> Int
solve c _ 0 = toScore (toResult (HashMap.toList c) HashMap.empty)
solve c r n = solve c' r (n-1)
  where c' = step (HashMap.toList c) r HashMap.empty 

main :: IO ()
main = do
        handle <- openFile "input14.txt" ReadMode
        contents <- hGetContents handle
        let info = parse (lines contents)
            start = fst info
            rules = snd info
        print (solve start rules 10)
        print (solve start rules 40)
        hClose handle

-- 2223
-- 

import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import Numeric(readHex)

data Package = Package { version :: Int,
                         type_id :: Int,
                         packages :: [Package],
                         value :: Int} deriving Show

toKBaseNum :: Int -> Int -> (Int -> Char) -> [Char]
toKBaseNum x base map | x < base  = [map x]
                      | otherwise = toKBaseNum (x `div` base) base map ++ [map(x `mod` base)]

toNum :: [Char] -> Int -> (Char -> Int) -> Int
toNum [] base map = 0
toNum s  base map = base * toNum (init s) base map + map(last s)

decToBin :: Int -> String
decToBin x = toKBaseNum x 2 (\x -> if x == 1 then '1' else '0')

binToDec :: String -> Int
binToDec [] = 0
binToDec s  = toNum s 2 (\x -> if x == '0' then 0 else 1)

hex2binC :: Char -> String
hex2binC '0' = "0000"
hex2binC '1' = "0001"
hex2binC '2' = "0010"
hex2binC '3' = "0011"
hex2binC '4' = "0100"
hex2binC '5' = "0101"
hex2binC '6' = "0110"
hex2binC '7' = "0111"
hex2binC '8' = "1000"
hex2binC '9' = "1001"
hex2binC 'A' = "1010"
hex2binC 'B' = "1011"
hex2binC 'C' = "1100"
hex2binC 'D' = "1101"
hex2binC 'E' = "1110"
hex2binC 'F' = "1111"
hex2binC c = "FUCK"

hex2bin :: String -> String
hex2bin s = concat [hex2binC c | c <- s]

parseValue :: String -> String -> (Int, String)
parseValue s v
  | last = (binToDec(v ++ group), rest)
  | otherwise = parseValue rest (v ++ group)
  where label = take 5 s
        rest = drop 5 s
        last = head label == '0'
        group = drop 1 label

parsePackages :: String -> [Package] -> [Package]
parsePackages s ps
  | null nps = ps
  | otherwise = parsePackages s' (ps ++ nps)
  where (nps, s') = bin2pack s

parseOperatorsLength :: Int -> String -> ([Package], Int, String)
parseOperatorsLength l s = (parsePackages (take l s) [], 0, drop l s)

parseOperatorsNr :: Int -> String -> [Package] -> ([Package], Int, String)
parseOperatorsNr 0 s ps = (ps, 0, s)
parseOperatorsNr n s ps = parseOperatorsNr (n-1) s' ps'
  where (nps, s') = bin2pack s
        ps' = ps ++ nps

parsePayload :: Bool -> String -> ([Package], Int, String)
parsePayload True s = ([], value, rest)
  where (value, rest) = parseValue s ""
parsePayload False s
  | head s == '0' = parseOperatorsLength (binToDec (take 15 (drop 1 s))) (drop 16 s)
  | otherwise = parseOperatorsNr (binToDec (take 11 (drop 1 s))) (drop 12 s) []

bin2pack :: String -> ([Package], String)
bin2pack s 
  | length s < 6 = ([],"")
  | otherwise = ([Package version type_id packages value], rest)
   where version = binToDec (take 3 s)
         type_id = binToDec (take 3 (drop 3 s))
         payload = drop 6 s
         (packages, value, rest) = parsePayload (type_id==4) payload

solve :: [Package] -> Int -> Int
solve [] s = s
solve (p:ps) s = solve ps' s'
  where s' = s + version p
        ps' = ps ++ packages p

greater :: [Int] -> Int
greater a
  | head a > last a = 1
  | otherwise = 0

less :: [Int] -> Int
less a
  | head a < last a = 1
  | otherwise = 0

equal :: [Int] -> Int
equal a
  | head a == last a = 1
  | otherwise = 0

solve2 :: Package -> Int
solve2 p
  | type_id p == 0 = sum [solve2 p | p <- packages p]
  | type_id p == 1 = product [solve2 p | p <- packages p]
  | type_id p == 2 = minimum [solve2 p | p <- packages p]
  | type_id p == 3 = maximum [solve2 p | p <- packages p]
  | type_id p == 4 = value p
  | type_id p == 5 = greater [solve2 p | p <- packages p]
  | type_id p == 6 = less [solve2 p | p <- packages p]
  | otherwise  = equal [solve2 p | p <- packages p]

main :: IO ()
main = do
        handle <- openFile "input16.txt" ReadMode
        contents <- hGetContents handle
        let decoded = bin2pack (hex2bin contents)
        print (solve (fst decoded) 0)
        print (solve2 (head (fst decoded)))
        hClose handle

-- 951
-- 902198718880

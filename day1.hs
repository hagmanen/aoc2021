import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )

solve :: [Int] -> ([Int], Int) -> Int
solve [] s = snd s
solve (x:xs) (p, c)
  | inc = solve xs (p', c + 1)
  | otherwise = solve xs (p', c)
  where p' = x : init p
        inc = sum p' > sum p

initialState :: [Int] -> Int -> ([Int], Int)
initialState [] _ = ([], 0)
initialState (x:xs) n = (replicate n (n*x), 0)

main :: IO ()
main = do
        handle <- openFile "input1.txt" ReadMode
        contents <- hGetContents handle
        let list = map read (lines contents)
        print (solve list (initialState list 1))
        print (solve list (initialState list 3))
        hClose handle

-- 1616
-- 1645

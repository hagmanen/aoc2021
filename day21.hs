import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Hashable as HashMap

roll :: Int -> (Int, Int)
roll n = (1 + mod n 100, n+1)

turn :: (Int, Int) -> (Int, Int)
turn (p, dice) = (mod (p + r1 + r2 + r3) 10, dice''')
   where (r1, dice') = roll dice
         (r2, dice'') = roll dice'
         (r3, dice''') = roll dice''

play :: ((Int, Int), (Int, Int), Int) -> ((Int, Int), (Int, Int), Int)
play ((p1p, p1s), (p2p, p2s), dice)
  | p1s' >= 1000 = ((p1p', p1s'), (p2p, p2s), dice')
  | otherwise = ((p1p', p1s'), (p2p', p2s'), dice'')
  where (p1p', dice') = turn (p1p, dice)
        p1s' = p1s + 1 + p1p'
        (p2p', dice'') = turn (p2p, dice')
        p2s' = p2s + 1 + p2p'

solve :: ((Int, Int), (Int, Int), Int) -> Int
solve ((p1p, p1s), (p2p, p2s), dice)
  | p1s >= 1000 = p2s * dice
  | p2s >= 1000 = p1s * dice
  | otherwise = solve (play ((p1p, p1s), (p2p, p2s), dice))

start :: String -> Int
start s = read (drop 28 s)

type PlayerState = (Int, Int)
type GameState = (PlayerState, PlayerState)

data DGameState = DGameState { game_states :: HashMap.HashMap GameState Int, player :: Bool, win_count :: (Int, Int)} deriving (Show, Eq)

getPlayerState :: Bool -> GameState -> GameState
getPlayerState False gs = gs
getPlayerState True gs = (snd gs, fst gs)

incWins :: Bool -> Int -> DGameState -> (Int, Int)
incWins False n gs = (fst (win_count gs) + n, snd (win_count gs))
incWins True n gs = (fst (win_count gs), snd (win_count gs) + n)

-- Move 'steps' forward and update score
dmove :: Int -> PlayerState -> PlayerState
dmove steps ps = (mod (fst ps + steps -1) 10 + 1, mod (fst ps + steps -1) 10 + 1 + snd ps)

-- Do 3 paralell moves with 1-3 steps
droll :: PlayerState -> [PlayerState]
droll ps = [dmove (x + y + z) ps | x <- [1..3], y <- [1..3], z <- [1..3]]

-- Do 3 paralell dice rolls
dturn :: PlayerState -> ([PlayerState], Int)
dturn ps = ([ps' | ps' <- pss, snd ps' < 21], length [ps' | ps' <- pss, snd ps' >= 21])
  where pss = droll ps

newGameState :: Bool -> PlayerState -> PlayerState -> GameState
newGameState False pl opo = (pl, opo)
newGameState True pl opo = (opo, pl)

newGameStates :: [PlayerState] -> PlayerState -> Bool -> Int -> HashMap.HashMap GameState Int -> HashMap.HashMap GameState Int
newGameStates [] _ _ _ gss = gss
newGameStates (ps:pss) opo pl count gss = newGameStates pss opo pl count (HashMap.insert new_game_state (old_count + count) gss)
  where new_game_state = newGameState pl ps opo
        old_count = HashMap.lookupDefault 0 new_game_state gss

dddplay :: GameState -> Int -> Bool -> DGameState -> DGameState
dddplay gs count pl ndgs = DGameState new_states (player ndgs) (incWins pl gained_wins ndgs)
  where (ps, opo) = getPlayerState pl gs
        (pss, wins) = dturn ps
        gained_wins = wins * count
        new_states = newGameStates pss opo pl count (game_states ndgs)


ddplay :: [(GameState, Int)] -> Bool -> DGameState -> DGameState
ddplay [] _ ndgs = ndgs
ddplay ((gs, count):gss) player ndgs = ddplay gss player (dddplay gs count player ndgs)

dplay :: DGameState -> DGameState -> DGameState
dplay dgs = ddplay (HashMap.toList (game_states dgs)) (player dgs)

solve2 :: DGameState -> Int
solve2 dgs
  | null (game_states dgs) = uncurry max (win_count dgs)
  | otherwise = solve2 (dplay dgs (DGameState HashMap.empty (not (player dgs)) (win_count dgs)))

startGameState :: Int -> Int -> GameState
startGameState p1p p2p = ((p1p, 0), (p2p, 0))

startState :: Int -> Int -> DGameState
startState p1p p2p = DGameState (HashMap.fromList [(startGameState p1p p2p, 1)]) False (0,0)

main :: IO ()
main = do
        handle <- openFile "input21.txt" ReadMode
        contents <- hGetContents handle
        let p1p = start (head (lines contents))
            p2p = start (lines contents !! 1)
        print (solve ((p1p-1, 0), (p2p-1, 0), 0))
        print (solve2 (startState p1p p2p))
        hClose handle

-- 900099
-- 306719685234774

import System.Random
import System.IO.Unsafe

data Maze = Maze 
    { cells :: [(Bool, Bool)]  -- [(rightWall, downWall)]
    , width :: Int
    , height :: Int
    }

rand :: Int -> Int
-- Returns a random integer from 0 to max-1
rand max = unsafePerformIO $ randomRIO (0, max-1)

shuffle :: [a] -> [a]
-- Randomly shuffles a list
shuffle = unsafePerformIO . shuffleM

shuffleM :: [a] -> IO [a]
-- DON'T BOTHER! Helper for shuffle
shuffleM [] = return []
shuffleM n = do 
    r <- fmap (flip mod $ length n) randomIO;
    n1 <- return $ n !! r;
    fmap ((:) n1) $ shuffleM $ (take r n) ++ (drop (r+1) n)

solvePerfect :: Maze -> (Int, Int) -> (Int, Int) -> [(Int,Int)]
-- Uses recursive DFS to solve a perfect maze
solvePerfect (maze c w h) (sx, sy) (gx, gy) 
    | (sx, sy) == (gx, gy) = [(sx, sy)]
    | fst (c !! (h * sx + sy + 1)) == False = [(sx, sy)] ++ solvePerfect maze (sx, sy + 1) (gx, gy)
    | snd (c !! (h * sx + sy + 1)) == False = [(sx, sy)] ++ solvePerfect maze (sx + 1, sy) (gx, gy)

buildLabyrinth w h sw | w > 1 = (False, False) : buildLabyrinth (w-1) h sw
                      | w == 1 && h > 1 = (False, False) : buildLabyrinth sw (h-1) sw
                      | w == 1 && h == 1 = [(False, False)]

makeMaze w h = Maze (buildLabyrinth w h w) w h

kruskal (Maze l w h) = Maze l w h

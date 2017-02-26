import System.Random
import System.IO.Unsafe
import qualified Data.Set as Set

data Maze = Maze 
    { cells :: [(Bool, Bool)]  -- [(rightWall, downWall)]
    , width :: Int
    , height :: Int
    } deriving (Show)

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

solvePerfect (Maze c w h) (sx, sy) (gx, gy) = dfs (Maze c w h) (sx, sy) (gx, gy) (-1, -1)

-- Uses recursive DFS to solve a perfect maze
dfs (Maze c w h) (sx, sy) (gx, gy) (px, py)
    | (sx, sy) == (gx, gy) = [(sx, sy)]
    | legal /= [] && try /= [] = [(sx, sy)] ++ try
    | otherwise = []
  where
    legal = legalMoves (Maze c w h) (sx, sy) (px, py)
    try = tryLegal (Maze c w h) legal (sx, sy) (gx, gy)
        
tryLegal _ [] _ _ = []
tryLegal (Maze c w h) (lh:lt) (sx, sy) (gx, gy) = if ret /= []
                                                  then ret
                                                  else tryLegal (Maze c w h) lt (sx, sy) (gx, gy)
                                                  where ret = dfs (Maze c w h) lh (gx, gy) (sx, sy)

legalMoves (Maze c w h) (sx, sy) (px, py) =
    goRight (Maze c w h) (sx, sy) (px, py) ++ goLeft (Maze c w h) (sx, sy) (px, py)
    ++ goDown (Maze c w h) (sx, sy) (px, py) ++ goUp (Maze c w h) (sx, sy) (px, py)

inMaze (Maze c w h) (x, y)
    | x >= 0 && x < w && y >= 0 && y < h = True
    | otherwise = False

goRight (Maze c w h) (sx, sy) (px, py)
    | (sx + 1, sy) /= (px, py) && inMaze (Maze c w h) (sx + 1, sy) && fst (c !! (w * sy + sx)) == False = [(sx + 1, sy)]
    | otherwise = []

goLeft (Maze c w h) (sx, sy) (px, py)
    | (sx - 1, sy) /= (px, py) && inMaze (Maze c w h) (sx - 1, sy) && fst (c !! (w * sy + (sx - 1))) == False = [(sx - 1, sy)]
    | otherwise = []

goDown (Maze c w h) (sx, sy) (px, py)
    | (sx, sy + 1) /= (px, py) && inMaze (Maze c w h) (sx, sy + 1) && snd (c !! (w * sy + sx)) == False = [(sx, sy + 1)]
    | otherwise = []

goUp (Maze c w h) (sx, sy) (px, py)
    | (sx, sy - 1) /= (px, py) && inMaze (Maze c w h) (sx, sy - 1) && snd (c !! (w * (sy - 1) + sx)) == False = [(sx, sy - 1)]
    | otherwise = []

buildLabyrinth w h sw | w > 1 = (True, True) : buildLabyrinth (w-1) h sw
                      | w == 1 && h > 1 = (True, True) : buildLabyrinth sw (h-1) sw
                      | w == 1 && h == 1 = [(True, True)]

makeMaze w h = Maze (buildLabyrinth w h w) w h

allWalls w h c | mod c w /= 0 && div (c-1) w < h-1 = (c-1, c) : (c-1, c-1+w) : allWalls w h (c+1)
               | mod c w /= 0 = (c-1, c) : allWalls w h (c+1)
               | div (c-1) w < h-1 = (c-1, c-1+w) : allWalls w h (c+1)
               | otherwise = []

walls w h = shuffle $ allWalls w h 1

initSet c n = if c < n
              then Set.fromList [c] : initSet (c+1) n
              else [Set.fromList [c]]

fixUnion [] _ _ = []
fixUnion (h:s) c p = if Set.member p c
                     then c : fixUnion s c (p+1)
                     else h : fixUnion s c (p+1)

union s h = fixUnion s (Set.union (s !! fst h) (s !! snd h)) 0

solveKruskal [] _ = []
solveKruskal (h:w) s | not(Set.member (fst h) (s !! snd h)) && 
                       not(Set.member (snd h) (s !! fst h)) = 
                       h : solveKruskal w (union s h)
                     | otherwise = solveKruskal w s

returnMaze [] _ _ _ = []
returnMaze (h:c) w p s | Set.member (p, p+1) s && Set.member (p, p+w) s = (False, False) : returnMaze c w (p+1) s
                       | Set.member (p, p+1) s = (False, snd h) : returnMaze c w (p+1) s
                       | Set.member (p, p+w) s = (fst h, False) : returnMaze c w (p+1) s
                       | otherwise = h : returnMaze c w (p+1) s 

kruskal (Maze c w h) = Maze (returnMaze c w 0 $ Set.fromList $ solveKruskal (walls w h) (initSet 0 (w*h-1))) w h

buildMaze (Maze c w h) w1 h1 s | h1 == 2*h+1 = ""
                               | (h1 == 0 || h1 == 2*h) && w1 == w-1 = "---+\n" ++ buildMaze (Maze c w h) 0 (h1+1) s
                               | (h1 == 0 || h1 == 2*h) && w1 == 0 = "+---+" ++ buildMaze (Maze c w h) (w1+1) h1 s
                               | h1 == 0 || h1 == 2*h = "---+" ++ buildMaze (Maze c w h) (w1+1) h1 s
                               | mod h1 2 == 0 && w1 == 0 && snd (c !! (w1 + ((div h1 2) - 1) * w)) == False = "+   +" ++ buildMaze (Maze c w h) (w1+1) h1 s
                               | mod h1 2 == 0 && w1 < w-1 && snd (c !! (w1 + ((div h1 2) - 1) * w)) == False = "   +" ++ buildMaze (Maze c w h) (w1+1) h1 s
                               | mod h1 2 == 0 && w1 == w-1 && snd (c !! (w1 + ((div h1 2) - 1) * w)) == False = "   +\n" ++ buildMaze (Maze c w h) 0 (h1+1) s
                               | mod h1 2 == 0 && w1 == 0 = "+---+" ++ buildMaze (Maze c w h) (w1+1) h1 s
                               | mod h1 2 == 0 && w1 < w-1 = "---+" ++ buildMaze (Maze c w h) (w1+1) h1 s
                               | mod h1 2 == 0 && w1 == w-1 = "---+\n" ++ buildMaze (Maze c w h) 0 (h1+1) s
                               | mod h1 2 == 1 && w1 == 0 && fst (c !! (w1 + (div h1 2) * w)) == False && 
                                 not(Set.member (w1, (div h1 2)) s) = "|    " ++ buildMaze (Maze c w h) (w1+1) h1 s
                               | mod h1 2 == 1 && w1 == 0 && fst (c !! (w1 + (div h1 2) * w)) == False = "| *  " ++ buildMaze (Maze c w h) (w1+1) h1 s
                               | mod h1 2 == 1 && w1 < w-1 && fst (c !! (w1 + (div h1 2) * w)) == False && 
                                 not(Set.member (w1, (div h1 2)) s) = "    " ++ buildMaze (Maze c w h) (w1+1) h1 s
                               | mod h1 2 == 1 && w1 < w-1 && fst (c !! (w1 + (div h1 2) * w)) == False = " *  " ++ buildMaze (Maze c w h) (w1+1) h1 s
                               | mod h1 2 == 1 && w1 == 0 && not(Set.member (w1, (div h1 2)) s) = "|   |" ++ buildMaze (Maze c w h) (w1+1) h1 s
                               | mod h1 2 == 1 && w1 == 0 = "| * |" ++ buildMaze (Maze c w h) (w1+1) h1 s
                               | mod h1 2 == 1 && w1 < w-1 && not(Set.member (w1, (div h1 2)) s) = "   |" ++ buildMaze (Maze c w h) (w1+1) h1 s
                               | mod h1 2 == 1 && w1 < w-1 = " * |" ++ buildMaze (Maze c w h) (w1+1) h1 s
                               | mod h1 2 == 1 && w1 == w-1 &&  not(Set.member (w1, (div h1 2)) s) = "   |\n" ++ buildMaze (Maze c w h) 0 (h1+1) s
                               | mod h1 2 == 1 && w1 == w-1 = " * |\n" ++ buildMaze (Maze c w h) 0 (h1+1) s

showMaze :: Maze -> [(Int, Int)] -> String
showMaze (Maze c w h) s = buildMaze (Maze c w h) 0 0 (Set.fromList s)

leftWall (Maze c w h) p s = if not(Set.member (p-1, p) s) && (mod p w == 0 || fst(c !! (p-1)) == True)
                            then 1
                            else 0

rightWall (Maze c w h) p s = if not(Set.member (p, p+1) s) && (mod (p+1) w == 0 || fst(c !! p) == True)
                            then 1
                            else 0

downWall (Maze c w h) p s = if not(Set.member (p, p+w) s) && (div p w >= h-1 || snd(c !! p) == True)
                            then 1
                            else 0

upWall (Maze c w h) p s = if not(Set.member (p-w, p) s) && (div p w < 1 || snd(c !! (p-w)) == True)
                            then 1
                            else 0

countWalls (Maze c w h) p s = leftWall (Maze c w h) p s + rightWall (Maze c w h) p s
                              + downWall (Maze c w h) p s + upWall (Maze c w h) p s

removeRandWall (Maze c w h) p | mod p w /= 0 && fst(c !! (p-1)) == True = [(p-1, p)]
                              | div p w > 0  && snd(c !! (p-w)) == True = [(p-w, p)]
                              | mod (p+1) w /= 0 && fst(c !! p) == True = [(p, p+1)]
                              | div p w < h-1 && snd(c !! p) == True = [(p, p+w)]

removeWalls (Maze c w h) p s = if p >= w*h 
                               then []
                               else       
                                   if countWalls (Maze c w h) p s == 3
                                   then rem ++ removeWalls (Maze c w h) (p+1) (Set.union s (Set.fromList rem))
                                   else removeWalls (Maze c w h) (p+1) s
                                   where rem = removeRandWall (Maze c w h) p 
                               
braid (Maze c w h) = Maze (returnMaze c w 0 $ Set.fromList $ removeWalls (Maze c w h) 0 (Set.fromList [])) w h

solveBraid (Maze c w h) (sx, sy) (gx, gy) = dfsBraid (Maze c w h) (sx, sy) (gx, gy) (-1, -1) (Set.fromList [])

dfsBraid (Maze c w h) (sx, sy) (gx, gy) (px, py) s
    | (sx, sy) == (gx, gy) = [(sx, sy)]
    | legal /= [] && try /= [] = [(sx, sy)] ++ try
    | otherwise = []
  where
    legal = legalMovesBraid (Maze c w h) (sx, sy) (px, py) s
    try = tryLegalBraid (Maze c w h) legal (sx, sy) (gx, gy) (Set.union s (Set.fromList [(sx, sy)]))
        
tryLegalBraid _ [] _ _ _ = []
tryLegalBraid (Maze c w h) (lh:lt) (sx, sy) (gx, gy) s = if ret /= []
                                                         then ret
                                                         else tryLegalBraid (Maze c w h) lt (sx, sy) (gx, gy) s
                                                         where ret = dfsBraid (Maze c w h) lh (gx, gy) (sx, sy) s

legalMovesBraid (Maze c w h) (sx, sy) (px, py) s =
    goRightBraid (Maze c w h) (sx, sy) (px, py) s ++ goLeftBraid (Maze c w h) (sx, sy) (px, py) s
    ++ goDownBraid (Maze c w h) (sx, sy) (px, py) s ++ goUpBraid (Maze c w h) (sx, sy) (px, py) s

goRightBraid (Maze c w h) (sx, sy) (px, py) s
    | not(Set.member (sx + 1, sy) s) && (sx + 1, sy) /= (px, py) && inMaze (Maze c w h) (sx + 1, sy) && fst (c !! (w * sy + sx)) == False = [(sx + 1, sy)]
    | otherwise = []

goLeftBraid (Maze c w h) (sx, sy) (px, py) s
    | not(Set.member (sx - 1, sy) s) && (sx - 1, sy) /= (px, py) && inMaze (Maze c w h) (sx - 1, sy) && fst (c !! (w * sy + (sx - 1))) == False = [(sx - 1, sy)]
    | otherwise = []

goDownBraid (Maze c w h) (sx, sy) (px, py) s
    | not(Set.member(sx, sy + 1) s) && (sx, sy + 1) /= (px, py) && inMaze (Maze c w h) (sx, sy + 1) && snd (c !! (w * sy + sx)) == False = [(sx, sy + 1)]
    | otherwise = []

goUpBraid (Maze c w h) (sx, sy) (px, py) s
    | not(Set.member(sx, sy - 1) s) && (sx, sy - 1) /= (px, py) && inMaze (Maze c w h) (sx, sy - 1) && snd (c !! (w * (sy - 1) + sx)) == False = [(sx, sy - 1)]
    | otherwise = []



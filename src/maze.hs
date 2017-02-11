import System.Random
import System.IO.Unsafe
import qualified Data.Set as Set

data Maze = Maze { cells :: [(Bool, Bool)]  -- [(rightWall, downWall)]
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
shuffleM n = do {
                r <- fmap (flip mod $ length n) randomIO;
                n1 <- return $ n !! r;
                fmap ((:) n1) $ shuffleM $ (take r n) ++ (drop (r+1) n)
             }

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

buildMaze (Maze c w h) w1 h1 | h1 == 2*h+1 = ""
                             | (h1 == 0 || h1 == 2*h) && w1 == w-1 = "---+\n" ++ buildMaze (Maze c w h) 0 (h1+1)
                             | (h1 == 0 || h1 == 2*h) && w1 == 0 = "+---+" ++ buildMaze (Maze c w h) (w1+1) h1
                             | h1 == 0 || h1 == 2*h = "---+" ++ buildMaze (Maze c w h) (w1+1) h1
                             | mod h1 2 == 0 && w1 == 0 && snd (c !! (w1 + ((div h1 2) - 1) * w)) == False = "+   +" ++ buildMaze (Maze c w h) (w1+1) h1
                             | mod h1 2 == 0 && w1 < w-1 && snd (c !! (w1 + ((div h1 2) - 1) * w)) == False = "   +" ++ buildMaze (Maze c w h) (w1+1) h1
                             | mod h1 2 == 0 && w1 == w-1 && snd (c !! (w1 + ((div h1 2) - 1) * w)) == False = "   +\n" ++ buildMaze (Maze c w h) 0 (h1+1)
                             | mod h1 2 == 0 && w1 == 0 = "+---+" ++ buildMaze (Maze c w h) (w1+1) h1
                             | mod h1 2 == 0 && w1 < w-1 = "---+" ++ buildMaze (Maze c w h) (w1+1) h1
                             | mod h1 2 == 0 && w1 == w-1 = "---+\n" ++ buildMaze (Maze c w h) 0 (h1+1)
                             | mod h1 2 == 1 && w1 == 0 && fst (c !! (w1 + (div h1 2) * w)) == False = "|    " ++ buildMaze (Maze c w h) (w1+1) h1
                             | mod h1 2 == 1 && w1 < w-1 && fst (c !! (w1 + (div h1 2) * w)) == False = "    " ++ buildMaze (Maze c w h) (w1+1) h1
                             | mod h1 2 == 1 && w1 == 0 = "|   |" ++ buildMaze (Maze c w h) (w1+1) h1
                             | mod h1 2 == 1 && w1 < w-1 = "   |" ++ buildMaze (Maze c w h) (w1+1) h1
                             | mod h1 2 == 1 && w1 == w-1 = "   |\n" ++ buildMaze (Maze c w h) 0 (h1+1)
                            
showMaze (Maze c w h) = buildMaze (Maze c w h) 0 0

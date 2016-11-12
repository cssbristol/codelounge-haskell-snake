module Main where

  import Snake hiding (replicate, length, any)
  import Data.List (intercalate)
  import Control.Concurrent
  -- import System.Console.ANSI
  import Control.Monad
  import System.IO
  import System.IO.NoBufferingWorkaround
  import System.Random

  -- The game needs to be turned into a grid will all of the correct elements
  -- in the right place.

  makeGrid :: Game -> Grid
  makeGrid ((snek, _), (fud, _), (w, h)) = [[a | x <- [0..w - 1], let a = if (x, y) == head snek then Head else if (x,y) `elem` snek then Snake else if (x, y) == fud then Food else Blank] | y <- [0..h - 1]]

  pretty :: Grid -> String
  pretty g = ((top++) . (++bottom) . (++"║\n") . ('║':) . concat . intercalate ["║\n║"]  . map (map show)) g
    where
      n = length (head g)
      top = ("╔" ++ (replicate n '═') ++ "╗\n")
      bottom = ("╚" ++ (replicate n '═') ++ "╝\n")

  -- IO Stuff

  clearScreen = putStr "\ESC[2J"

  moveCursor x y = mapM_ putStr ["\ESC[", show x, ";", show y, "H"]

  hideCursor = putStr "\ESC[?25l"

  showCursor = putStr "\ESC[?25h"

  main = do
    initGetCharNoBuffering
    hideCursor
    clearScreen
    gameLoop game
    showCursor

  turnChar 'w' = turn North
  turnChar 'a' = turn West
  turnChar 's' = turn South
  turnChar 'd' = turn East
  turnChar _ = Just

  gameLoop :: Game -> IO ()
  gameLoop (snake, food, size) = do
    let snake' = (eat food . move) snake
    food' <- if eatable food snake' then do
               x <- randomRIO (0, fst size - 1)
               y <- randomRIO (0, snd size - 1)
               return ((x, y), 1)
             else return food
    a <- getCharNoBuffering
    let snake'' = case a >>= flip turnChar (snake') of
                    Nothing -> snake'
                    Just s  -> s
    let game' = (snake'', food', size)
    let grid = (pretty . makeGrid) game'
    if dead snake'' size then
      putStrLn $ "You're dead, You scored: " ++ (show $ score snake'')
    else
      do
        moveCursor 1 1
        putStrLn ""
        putStrLn ("Current Score: " ++ (show $ score snake''))
        putStrLn grid
        threadDelay delay
        gameLoop game'

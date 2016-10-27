module Main where

  import Snake
  import Data.List
  import Control.Concurrent
  import System.Console.ANSI
  import Control.Monad
  import System.IO

  -- The game needs to be turned into a grid will all of the correct elements
  -- in the right place.

  makeGrid :: Game -> Grid
  makeGrid ((snek, _), (fud, _), (w, h)) = [[a | x <- [0..w - 1], let a = if (x, y) == head snek then Head else if (x,y) `elem` snek then Snake else if (x, y) == fud then Food else Blank] | y <- [0..h - 1]]

  pretty :: Grid -> String
  pretty g = (('|':) . (++"|") . concat . intercalate ["|\n|"] . (++[(replicate n "-")]) . ((replicate n "-"):) . map (map show)) g
    where
      n = length (head g)

  -- IO Stuff

  main = do
    threadDelay delay
    c <- newEmptyMVar
    hSetBuffering stdin NoBuffering
    forkIO $ do
      forever $ do
        a <- getChar
        putMVar c a
    gameLoop game c

  turnChar 'w' = turn North
  turnChar 'a' = turn West
  turnChar 's' = turn South
  turnChar 'd' = turn East
  turnChar _ = Just

  gameLoop :: Game -> MVar (Char) -> IO ()
  gameLoop (snake, food, size) c = do
    let snake' = (eat food . move) snake
    a <- tryTakeMVar c
    let snake'' = case (join $ (flip turnChar snake) <$> a) of
                    Nothing -> snake'
                    Just s  -> s
    let game' = (snake'', food, size)
    let grid = (pretty . makeGrid) game'
    if dead snake'' size then

      putStrLn $ "You're dead, You scored: " ++ (show $ score snake'')
    else
      do
        clearScreen
        putStrLn ""
        putStrLn grid
        threadDelay delay
        gameLoop game' c

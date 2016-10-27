module Snake where

  data Direction = North
                 | East
                 | South
                 | West
                 deriving (Eq, Show)

  -- A snake is a list of Coordinates. And the direction it's moving in.

  type Snake = ([(Int, Int)], Direction)

  -- Food is a single Coordinate and the size of the food.

  type Food = ((Int, Int), Int)

  data Piece = Snake
             | Food
             | Blank
             | Head
            --  deriving Show

  instance Show Piece where
    show Snake = "▇"
    show Food  = "■"
    show Blank = " "
    show Head  = "░"

  -- This stores the width and height of the grid
  type Size = (Int, Int)

  -- The grid is just a list of list of Pieces, that is contructed out of the
  -- data that is worked out.

  -- A game consists of the current snake, the current food and the size of the
  -- grid.

  type Grid = [[Piece]]

  type Game = (Snake, Food, Size)

  start :: Snake
  start = ([(0, 0),(1, 0),(2, 0),(3, 0),(4,0),(5,0),(6,0)], South)

  game :: Game
  game = (start, ((5, 10), 1), (60, 21))

  move :: Snake -> Snake
  move (s@((a, b):ss), direction) = (s':ss', direction)
    where
      ss' = init s
      s' = case direction of
        North -> (a, b - 1)
        East  -> (a + 1, b)
        South -> (a, b + 1)
        West  -> (a - 1, b)

  eat :: Food -> Snake -> Snake
  eat food@(_, n) snake@(ss, d)
    | eatable food snake = (ss ++ replicate n (-1, -1), d)
    | otherwise = snake

  -- These functions allow preventing the snake from turning against itself.

  opposite :: Direction -> Direction
  opposite North = South
  opposite East  = West
  opposite South = North
  opposite West  = East

  eatable :: Food -> Snake -> Bool
  eatable (f, _) (s:_,_) = f == s

  turn :: Direction -> Snake -> Maybe Snake
  turn d' (s, d)
    | d' == opposite d = Nothing
    | otherwise = Just (s, d')

  -- The snake should be dead if the head touches itself or any of the
  -- surrounding walls.

  dead :: Snake -> Size -> Bool
  dead (s@(x, y):ss, _) (w, d) = x > w - 1 || x < 0 || y > d - 1 || y < 0 || any (==s) ss



  score :: Snake -> Int
  score (ss, _) = length ss

  delay :: Int
  delay = 100000

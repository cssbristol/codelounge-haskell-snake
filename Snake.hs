module Snake where

  import Prelude hiding (any, replicate, length)

  -- The directions that the snake is capable of moving in, They line up to the
  -- normal compass points.
  data Direction = --Some code
                --Some more code
                 deriving (Eq, Show)

  -- A snake is a list of Coordinates. And the direction it's moving in.
  type Snake = --Some code

  -- Food is a single Coordinate and the size of the food. Although the size of
  -- the food is always one, perhaps this could be extended somehow.
  type Food = --Some code

  -- The different items that appear on the screen whilst playing, consisting of
  -- the different parts of the snake and the Food
  data Piece = --Some code

  -- The characters used when showing the different items on the screen.
  instance Show Piece where
    show Snake = "▮"
    show Food  = "▣"
    show Blank = " "
    show Head  = "▯"

  -- This stores the width and height of the grid
  type Size = --Some code

  -- The grid is just a list of list of Pieces, that is contructed out of the
  -- data that is worked out, although currently that is hidden in the other
  -- file

  length :: [a] -> Int
  length = --Some code

  any :: (a -> Bool) -> [a] -> Bool
  any p = --Some code

  replicate :: Int -> a -> [a]
  replicate n m = map (const m) [1..n]

  -- The grid perhaps doesn't need to stay in this part of the file.
  type Grid = [[Piece]]


  -- A game consists of a snake, the food on the screen and the size of the
  -- grid, they will always be given the current state of the game.
  type Game = (Snake, Food, Size)

  -- The starting config for the snake.
  start :: Snake
  start = ([(0, 0),(1, 0),(2, 0),(3, 0),(4,0),(5,0),(6,0)], South)

  -- The starting config for the whole game, the size of the grid can be altered
  -- here, although perhaps that should be moved.
  game :: Game
  game = (start, ((5, 10), 1), (60, 21))

  -- Makes a move by updating the snake that it is given based on the direction
  -- that it is currently travelling in. The checks for being dead and such will
  -- have already been made at this point so it isn't needed here.

  -- You may find that using the as pattern (@) is helpful
  -- The as pattern simply means that I want to have access to the non
  -- pattern matched thing as well as the pattern matched thing. E.g.: xs@x:xs'
  -- xs = list, x = head and xs' = tail.
  move :: Snake -> Snake
  --Some code

  -- Make the snake longer when given food. All of the new food could be placed
  -- in appropriate positions based on the direction of the snake, but far
  -- simpler is to just place them off the grid somewhere, and let the method
  -- for moving bring them on to the grid one by one.

  eat :: Food -> Snake -> Snake
  --Some code

  -- simply checks that the position of the food matches with the new position
  -- of the snake.

  eatable :: Food -> Snake -> Bool
  --Some code

  -- Returns the opposite direction when called.
  opposite :: Direction -> Direction
  --Some code

  -- Prevent turning 180 by making use of the opposite function
  turn :: Direction -> Snake -> Maybe Snake
  --Some code

  -- The snake should be dead if the head touches itself or any of the
  -- surrounding walls. Also if the head is touching any part of it's body.
  dead :: Snake -> Size -> Bool
  --Some code

  -- The score is just the length of the snake
  score :: Snake -> Int
  --Some code

  -- Necessary delay for the IO to run the game
  delay :: Int
  delay = 100000

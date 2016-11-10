module Snake where

  import Prelude hiding (any, replicate, length)

  -- The directions that the snake is capable of moving in, They line up to the
  -- normal compass points.
  data Direction = North
                 | East
                 | South
                 | West
                 deriving (Eq, Show)

  -- A snake is a list of Coordinates. And the direction it's moving in.
  type Snake = ([(Int, Int)], Direction)

  -- Food is a single Coordinate and the size of the food. Although the size of
  -- the food is always one, perhaps this could be extended somehow.
  type Food = ((Int, Int), Int)

  -- The different items that appear on the screen whilst playing, consisting of
  -- the different parts of the snake and the Food
  data Piece = Snake
             | Food
             | Blank
             | Head

  -- The characters used when showing the different items on the screen.
  instance Show Piece where
    show Snake = "▓"
    show Food  = "▒"
    show Blank = "░"
    show Head  = "█"

  -- This stores the width and height of the grid
  type Size = (Int, Int)

  -- The grid is just a list of list of Pieces, that is contructed out of the
  -- data that is worked out, although currently that is hidden in the other
  -- file

  length :: [a] -> Int
  length = foldr (\_ n -> n + 1) 0

  any :: (a -> Bool) -> [a] -> Bool
  any p = foldr (\x b -> p x || b) False

  replicate :: Int -> a -> [a]
  replicate n m = map (const m) [1..n]

  -- The grid perhaps doesn't need to stay in this part of the file.
  type Grid = [[Piece]]


  -- A game is consists of a snake, the food on the screen and the size of the
  -- grid, they will always be given the current state of the game.
  type Game = (Snake, Food, Size)

  size@(w, h) = (60, 21)

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

  -- The as pattern (@) simply means that I want to have access to the non
  -- pattern matched thing as well as the pattern matched thing. E.g.: xs@x:xs'
  -- xs = list, x = head and xs' = tail. Perhaps this is unecessary although I
  -- think it's not too big of a step.

  move :: Snake -> Snake
  move (s@((a, b):ss), direction) = (s':ss', direction)
    where
      ss' = init s
      s' = case direction of
        North -> wrap (a, b - 1)
        East  -> wrap (a + 1, b)
        South -> wrap (a, b + 1)
        West  -> wrap (a - 1, b)
      wrap (a, b) = (a `mod` w, b `mod` h)
      -- wrap = id -- Prevents wrapping


  -- Make the snake longer when given food. All of the new food could be placed
  -- in appropriate positions based on the direction of the snake, but far
  -- simpler is to just place them off the grid somewhere, and let the method
  -- for moving bring them on to the grid one by one.

  -- Currently this is a little superfluous as there is no food larger than 1,
  -- but it leaves the door open for adding perhaps 4 piece food later.

  -- Also I'm not sold on having them check for eatable here, I think that
  -- should be moved to the game loop part.
  eat :: Food -> Snake -> Snake
  eat food@(_, n) snake@(ss, d)
    | eatable food snake = (ss ++ replicate n (-1, -1), d)
    | otherwise = snake

  -- simply checks that the position of the food matches with the new position
  -- of the snake.

  eatable :: Food -> Snake -> Bool
  eatable (f, _) (s:_,_) = f == s

  -- Returns the opposite direction when called.
  opposite :: Direction -> Direction
  opposite North = South
  opposite East  = West
  opposite South = North
  opposite West  = East

  -- Prevents turning 180 by making use of the opposite function. Returns a
  -- maybe As I think that this is better practice? Could possibly return an
  -- unchanged snake, but that's up for discussion.
  turn :: Direction -> Snake -> Maybe Snake
  turn d' (s, d)
    | d' == opposite d = Nothing
    | otherwise = Just (s, d')

  -- The snake should be dead if the head touches itself or any of the
  -- surrounding walls. Also if the head is touching any part of it's body.
  dead :: Snake -> Size -> Bool
  -- dead (s@(x, y):ss, _) (w, d) = x > w - 1 || x < 0 || y > d - 1 || y < 0 || any (==s) ss
  dead (s@(x, y):ss, _) (w, d) = any (==s) ss -- Snek is not kill, on walls

  -- The score is just the length of the snake, although it would be pretty easy
  -- to let them score it however they want.
  score :: Snake -> Int
  score (ss, _) = length ss

  -- I don't know what the exact unit is, but it's pretty small.
  delay :: Int
  delay = 1000

  -- Considering making them put it all together in one big function here, which
  -- would essentially carry everything the gameloop does, ignoring any of the
  -- funky IO stuff which would just become parameters to the function.

module Snapshot where

import Fruit
import Level
import GameTypes
import Ghost

data Snapshot = Snapshot
  {
    score       :: Integer,
    highScore   :: Integer,
    level       :: Level,
    pacman      :: Either (L, Death) (L, Munch, Facing),
    fruit       :: Maybe (L, Bounce, Fruit),
    ghosts      :: [(L, Ghost, Toggle, Facing)],
    eyes        :: [(L, Facing)],
    blueGhosts  :: [L],
    dots        :: [L],
    energizers  :: [L],
    numberDusts :: [(L, Integer)],
    fruitLights :: [Fruit]
  } deriving (Show)

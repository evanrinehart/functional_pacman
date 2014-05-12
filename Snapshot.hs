module Snapshot where

import Fruit
import Level

data Snapshot = Snapshot
  {
    score :: Integer,
    level :: Level,
    fruits :: [Fruit]
  } deriving (Show)

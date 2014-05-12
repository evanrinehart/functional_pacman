module Snapshot where

import Fruit

data Snapshot = Snapshot
  {
    score :: Integer,
    fruits :: [Fruit]
  } deriving (Show)

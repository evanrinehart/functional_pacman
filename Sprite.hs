module Sprite where

import Graphics.DrawingCombinators

class Sprite a where
  image :: a -> Image Any
  image _ = scale 0.5 0.5 %% tint gray circle where
    gray = Color 0.5 0.5 0.5 1
    


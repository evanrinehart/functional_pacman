module Render where

import Graphics.DrawingCombinators
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Applicative
import Data.Monoid

import Snapshot

data RenderEnv = RenderEnv
  {
    font' :: Font,
    fff' :: R2 -> Affine
  }

type Render = WriterT (Image Any) (Reader RenderEnv)

draw :: RenderEnv -> Snapshot -> IO ()
draw env snap = do
  let (_, img) = runReader (runWriterT (drawSnapshot snap)) env
  render img

drawSnapshot :: Snapshot -> Render ()
drawSnapshot snap = do
  font <- asks font'
  writeAt (2,3) square
  let msg = scale 0.5 0.5 %% text font (show snap)
  writeAt (0,4) msg
  forM_ [0..23] $ \i -> do
    writeAt (i,0) (line (0,0) (0,1))
    writeAt (i,0) (scale 0.3 0.3 %% tint green (text font (show (floor i))))
  writeAt (4,4) yellowCircle

writeAt :: R2 -> Image Any -> Render ()
writeAt x img = do
  fff <- asks fff'
  tell (fff x %% img)

square :: Image Any
square =
  tint green $
  scale 0.3 0.3 %%
  convexPoly [(-1,-1),(-1,1),(1,1),(1,-1)]

yellowCircle :: Image Any
yellowCircle = scale 0.5 0.5 %% tint yellow circle

yellow = Color 1 1 0 1
green = Color 0 1 0 1
blue = Color 0 0 1 1

--------
--------

setup :: Int -> Int -> IO RenderEnv
setup w h = RenderEnv <$>
  openFont "DroidSansMono.ttf" <*>
  pure (mkGlobalTransform w h)

mkGlobalTransform :: Int -> Int -> R2 -> Affine
mkGlobalTransform w h x = fff where
  gameUnitsPerWindowWidth = 24
  gameAreaHeight = 32
  gameOrigin = (0,0)
  sss = 2 / gameUnitsPerWindowWidth
  w' = realToFrac w
  h' = realToFrac h
  aspect = h' / w'
  fff =
    translate (-1,-1) <>
    inverse (scale 1 aspect) <>
    scale sss sss <>
    translate x <>
    translate gameOrigin


{-
the game area is coordinatized by a grid
the grid is 24 units wide and 40 units tall
the area extends from the left side of the screen to the right side
the origin is in the bottom left corner, which is in the bottom
left corner of the screen. if the window is too wide to show the
entire vertical extent of the game area, then the top will be cut off.
if the screen is too narrow, the area above the game area will be
black.
-}

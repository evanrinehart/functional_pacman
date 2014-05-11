module Render where

import Graphics.DrawingCombinators
import Control.Monad.Reader
import Control.Applicative
import Data.Monoid

import Game

{-
coordinate system notes

the default rendering coordinate system is [-1,1] x [-1,1] on the window
the window pixels are actually [0,winw] x [0,winh]
and the game has its own coordinate system

we will consider the default coordinate system the space in which to
describe sprites and other objects that will be placed in the world

to place such an object on the screen, we must transform by choosing
these things:
how large is the object in game coordinates?
where is the object in game coordinates?

once we answer these, we can put the thing on the screen by using the
world transformation which takes things of size 1 (game units) and 
draws them N pixels tall, where N is the length of 1 game unit in pixels.
note that this transformation always takes a thing with game size 1x1
and shows it in a square area on the screen regardless of window dimensions.

game coord origin: 0,0 is the top left of the screen
game units: 1 unit is 

-}

data RenderEnv = RenderEnv
  {
    font' :: Font,
    winh' :: R,
    winw' :: R
  }

type Render = ReaderT RenderEnv IO

draw :: Snapshot -> Render ()
draw snap = do
  font <- asks font'
  winh <- asks winh'
  winw <- asks winw'
  let wt size xy foo = foo
  let square = regularPoly 4
  let msg = text font (show snap)
  let scene = wt 1 (1,1) square <> wt 1 (3,1) msg
  liftIO (render scene)


--------
--------


setup :: Int -> Int -> IO RenderEnv
setup w h = RenderEnv <$>
  openFont "DroidSansMono.ttf" <*>
  pure (realToFrac w) <*>
  pure (realToFrac h)

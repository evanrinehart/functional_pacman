module App where

import Control.Monad.Reader
import Control.Applicative
import Control.Concurrent.STM
import System.Exit
import qualified Graphics.UI.GLFW as GLFW

import qualified Game
import Game (Game, GameEnv, Snapshot)
import Render (RenderEnv)
import qualified Render
import Dpad (DpadState)
import qualified Dpad
import Joystick
import PressRelease
import Dir
import DeltaClock

-- app provides a vsync and keyboard interface
-- it is implemented with
--   a game simulation,
--   a renderer,
--   a clock,
--   and a dpad state machine

type App = ReaderT AppEnv IO

data AppEnv = AppEnv
  {
    gameEnv :: GameEnv,
    dpadTV :: TVar DpadState,
    renderEnv :: RenderEnv,
    sinceLast' :: IO Double
  }

-- vsync indicates that you should redraw
vsync :: App ()
vsync = do
  dt <- sinceLast
  slip dt
  peek >>= draw

-- press or release the keyboard
keyboard :: PressRelease GLFW.Key -> App ()
keyboard (Press k) = case k of
  GLFW.Key'I -> insertCoin
  GLFW.Key'Enter -> pressStart
  GLFW.Key'A -> useDpad (Press DL)
  GLFW.Key'S -> useDpad (Press DD)
  GLFW.Key'D -> useDpad (Press DR)
  GLFW.Key'W -> useDpad (Press DU)
  _ -> return ()
keyboard (Release k) = case k of
  GLFW.Key'A -> useDpad (Release DL)
  GLFW.Key'S -> useDpad (Release DD)
  GLFW.Key'D -> useDpad (Release DR)
  GLFW.Key'W -> useDpad (Release DU)
  _ -> return ()

--------
-- app is defined in terms of commands hooked up to other components below
--------

peek :: App Snapshot
peek = runGame Game.peek

slip :: Double -> App ()
slip = runGame . Game.slip . realToFrac

insertCoin :: App ()
insertCoin = runGame Game.insertCoin

pressStart :: App ()
pressStart = runGame Game.pressStart

joystick :: Joystick -> App ()
joystick j = runGame (Game.joystick j)

runGame :: Game a -> App a
runGame command = do
  ge <- asks gameEnv
  liftIO $ runReaderT command ge

draw :: Snapshot -> App ()
draw snapshot = do
  re <- asks renderEnv
  liftIO $ runReaderT (Render.draw snapshot) re

sinceLast :: App Double
sinceLast = asks sinceLast' >>= liftIO

dpad :: PressRelease Dir -> App (Maybe Joystick)
dpad d = do
  tv <- asks dpadTV
  liftIO $ runReaderT (Dpad.dpad d) tv

useDpad :: PressRelease Dir -> App ()
useDpad d = do
  mj <- dpad d
  case mj of
    Just j -> joystick j
    Nothing -> return ()

setup :: Int -> Int -> IO AppEnv
setup w h = AppEnv <$>
  Game.setup <*>
  Dpad.setup <*>
  Render.setup w h <*>
  newDeltaClock

debug :: Show a => a -> App ()
debug x = liftIO (print x)

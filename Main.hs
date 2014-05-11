module Main where

import Control.Monad
import Control.Concurrent
import Data.Function
import Control.Monad.Reader

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import Game
import App
import PressRelease

winw = 480
winh = 640

{-
main instantiates the game components and
implements the support system that produces vsync and keyboard events
which drive the game.
-}

main :: IO ()
main = do
  appEnv <- App.setup winw winh
  driveApp appEnv

driveApp :: AppEnv -> IO ()
driveApp appEnv = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  when r $ do
    GLFW.windowHint (GLFW.WindowHint'Resizable False)
    m <- GLFW.createWindow winw winh "Pacman" Nothing Nothing
    case m of
      Nothing -> GLFW.terminate
      (Just win) -> do
        GLFW.makeContextCurrent m
        GLFW.swapInterval 1

        GLFW.setErrorCallback (Just errorCallback)
        GLFW.setKeyCallback win (Just (keyCallback appEnv))

        fix $ \loop -> do
          GLFW.pollEvents
          GLFW.swapBuffers win
          GL.clear [GL.ColorBuffer, GL.DepthBuffer]
          runReaderT vsync appEnv
          continue <- fmap not (GLFW.windowShouldClose win)
          when continue loop

simpleErrorCallback :: GLFW.Error -> String -> IO ()
simpleErrorCallback e s = putStrLn (show e ++ show s)

errorCallback :: GLFW.Error -> String -> IO ()
errorCallback e s = do
  putStrLn $ (show e)++" "++(show s)

-- the main keyboard callback
-- it intercepts the ESC key presses to close the program
-- everything else is passed directly to the app "keyboard" command
keyCallback ::
  AppEnv ->
  GLFW.Window ->
  GLFW.Key ->
  Int ->
  GLFW.KeyState ->
  GLFW.ModifierKeys ->
  IO ()
keyCallback appEnv win k scan act modKeys = case act of
  GLFW.KeyState'Pressed -> case k of
    GLFW.Key'Escape -> GLFW.setWindowShouldClose win True
    _ -> runReaderT (keyboard (Press k)) appEnv
  GLFW.KeyState'Released -> runReaderT (keyboard (Release k)) appEnv
  _ -> return ()


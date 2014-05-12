module Game where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Applicative

import GameTypes
import Level
import Linear
import Joystick
import Snapshot
import Fruit
import MazeA

type Game = ReaderT GameEnv IO

data GameEnv = GameEnv
  {
--    level :: TVar Level,
--    pacPath :: TVar Path
    score' :: TVar Integer,
    level' :: Level
  }

slip :: T -> Game ()
slip dt = return ()

peek :: Game Snapshot
peek = do
  env <- ask
  liftIO . atomically $ Snapshot
    <$> readTVar (score' env)
    <*> pure 999990
    <*> pure (level' env)
    <*> pure (Right (someL (level' env), Munch0, FacingNowhere))
    <*> pure Nothing
    <*> pure []
    <*> pure []
    <*> pure []
    <*> pure []
    <*> pure []
    <*> pure []
    <*> pure [Pomo, Banano]

joystick :: Joystick -> Game ()
joystick j = liftIO (print j)

pressStart :: Game ()
pressStart = liftIO (putStrLn "pressStart")

insertCoin :: Game ()
insertCoin = do
  s <- peek
  liftIO (print s)

--liftIO (putStrLn "insertCoin")

---

setup :: IO GameEnv
setup = GameEnv <$>
  newTVarIO 999990 <*>
  pure (mazeA)

module Game where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Applicative

import Level
import Linear
import Joystick
import Snapshot (Snapshot(Snapshot))
import Fruit

type Game = ReaderT GameEnv IO

data GameEnv = GameEnv
  {
--    level :: TVar Level,
--    pacPath :: TVar Path
    score' :: TVar Integer
  }

slip :: T -> Game ()
slip dt = return ()

peek :: Game Snapshot
peek = do
  env <- ask
  liftIO . atomically $
    Snapshot <$>
      readTVar (score' env) <*>
      pure [Pomo, Banano]

joystick :: Joystick -> Game ()
joystick j = liftIO (print j)

pressStart :: Game ()
pressStart = liftIO (putStrLn "pressStart")

insertCoin :: Game ()
insertCoin = liftIO (putStrLn "insertCoin")

---

setup :: IO GameEnv
setup = GameEnv <$> newTVarIO 999990

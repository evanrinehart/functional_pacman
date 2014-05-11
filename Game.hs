module Game where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Applicative

import Level
import Linear
import Joystick

type Game = ReaderT GameEnv IO

data Snapshot = Snapshot deriving (Show)

data GameEnv = GameEnv
  {
--    level :: TVar Level,
--    pacPath :: TVar Path
    dummy :: TVar Integer
  }

slip :: T -> Game ()
slip dt = return ()

peek :: Game Snapshot
peek = return Snapshot

joystick :: Joystick -> Game ()
joystick j = liftIO (print j)

pressStart :: Game ()
pressStart = liftIO (putStrLn "pressStart")

insertCoin :: Game ()
insertCoin = liftIO (putStrLn "insertCoin")

---

setup :: IO GameEnv
setup = GameEnv <$> newTVarIO 3

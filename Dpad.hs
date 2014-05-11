module Dpad where

import Control.Concurrent.STM
import Control.Monad.Reader

import Joystick
import Dir
import PressRelease

-- state machine to simulate a joystick with a dpad

data DpadState =
  Free |
  Hold1 Dir |
  Hold2 Dir Dir deriving (Show, Eq)

type Dpad = ReaderT (TVar DpadState) IO

dpad :: PressRelease Dir -> Dpad (Maybe Joystick)
dpad ev = do
  s <- get
  let (mj, s') = transition ev s
  put s'
  return mj

transition :: PressRelease Dir -> DpadState -> (Maybe Joystick, DpadState)
transition ev s = case s of
  Free -> case ev of
    Press d               -> (Just (Joy d),    Hold1 d    )
    Release _             -> (Nothing,         Free       )
  Hold1 dh -> case ev of
    Press d   | d == dh   -> (Nothing,         Hold1 dh   )
              | otherwise -> (Just (Joy d),    Hold2 d dh )
    Release d | d == dh   -> (Just JoyNeutral, Free       )
              | otherwise -> (Nothing,         Hold1 dh   )
  Hold2 dh1 dh2 -> case ev of
    Press d   | d == dh1  -> (Nothing,         s          )
              | otherwise -> (Just (Joy d),    Hold2 d dh1)
    Release d | d == dh1  -> (Just (Joy dh2),  Hold1 dh2  )
              | d == dh2  -> (Nothing,         Hold1 dh1  )
              | otherwise -> (Nothing,         s          )


---

get :: Dpad DpadState
get = ask >>= liftIO . readTVarIO

put :: DpadState -> Dpad ()
put j = ask >>= liftIO . atomically . flip writeTVar j

setup :: IO (TVar DpadState)
setup = newTVarIO Free

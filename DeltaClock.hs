module DeltaClock where

import Control.Concurrent.STM
import Data.Time.Clock.POSIX

newDeltaClock :: IO (IO Double)
newDeltaClock = do
  t0 <- getPOSIXTime
  tv <- newTVarIO t0
  return $ do
    t1 <- getPOSIXTime
    atomically $ do
      t0 <- readTVar tv
      let td = realToFrac (t1 - t0)
      writeTVar tv t1
      return td

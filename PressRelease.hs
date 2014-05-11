module PressRelease where

data PressRelease a = Press a | Release a deriving (Show, Eq)

payload :: PressRelease a -> a
payload (Press x) = x
payload (Release x) = x

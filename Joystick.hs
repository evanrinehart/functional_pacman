module Joystick where

import Dir

data Joystick = Joy Dir | JoyNeutral deriving (Show, Eq)

module GameTypes where

import Level

-- characters tend to be looking in a direction parallel to an edge
-- and theres two such directions for a given edge, A to B or B to A
data Orient = Forward | Backward deriving (Show)
data Facing = Facing Edge Orient | FacingNowhere deriving (Show)
data Munch = Munch0 | Munch1 | Munch2 deriving (Show)

data Bounce = Bounce0 | Bounce1 | Bounce2 deriving (Show)
data Death = Death0 | Death1 | Death2 deriving (Show)

data Toggle = On | Off deriving (Show)

toggle :: Toggle -> Toggle
toggle On  = Off
toggle Off = On
